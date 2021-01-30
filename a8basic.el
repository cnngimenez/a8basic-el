;;; a8basic.el --- Helpful functions for programming with 8-bit Basic

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Version: 0.1.0
;; Keywords: tools, convenience, Basic, 8-bit computer
;; URL: https://github.com/cnngimenez/a8basic-el
;; Package-Requires: ((emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides some useful functions to work with 8-bit computer's
;; Basic language.  For example, renumber lines, change LST to readable text
;; files, etc.

;;; Code:

(require 'cl-extra)

(defconst a8basic-charset-map 
  ;; Use "Symbols for legacy computing" unicode blocks. 
  '((#x00 #x1f
	  "🖤" "┣" "▕" "🬷" "┨" "🬨" "🮣" "🮢" "🭊" "🬞" "🬿" "🬁" "🬀" "🬂" "🬭" "🬏"
	  ;; Use "Block elements" and "Box drawing" unicode blocks.
	  "♣" "🬕 " "━" "╋" "●" "▄" "▏" "┳" "┻" "▌" "🬲" "␛" "🠱" "🠳" "🠰" "🠲")
    ;; 0x20 to 0x5f are the usual ASCII
    (#x60 #x60 "♦")
    ;; 0x61 to 0x7a are the usual ASCII
    (#x7b #x7f "♠" "|" "🢰" "⯇" "⯈")
    ;; 0x80 to 0xff are the same characters as before with black background
    ;; except for the following:
    (#x9b #x9b"⏎") ;; EOL symbol
    ;; (#xb0 #b9 "❶")  ;; Black digits
    ;; (#xc1 #xda "🅰"))  ;; Black capital letters
    )
  "Alist that associate the ASCII code of the Altirra Basic to UTF-8 codes.") ;; defconst

(defconst a8basic-label-regexp "[[:alnum:]_-]+"
  "The label regexp.") ;; defconst

(defconst a8basic-line-label-regexp (format "^[[:digit:]]*[[:space:]]*\\(%s\\):[[:space:]]*$"
				     a8basic-label-regexp)
  "Regexp for the label definitions (the in-line definition).") ;; defconst

(defconst a8basic-instructions-with-labels '("goto" "gosub" "trap")
  "These are simple instructions that can have labels.
For instance: \"GOTO somewhere\"
The labels can be changed into line numbers later.") ;; defconst

(defconst a8basic-line-number-regexp "^\\([[:digit:]]+\\)[[:space:]]+"
  "The regular expression to match the line numbers of the code.") ;; defconst

(defface a8basic-char-black-face
  '((t :foreground "black" :background "grey" :height 2.0))
  "Face for Basic ASCII characters representation with black background"
  :group 'a8basic)

(defface a8basic-char-normal
  '((t :foreground "grey" :background "black" :height 2.0))
  "Face for Basic ASCII characters representation with black background"
  :group 'a8basic)

(defun a8basic-convert-to-lst ()
  "Convert from text into atari LST files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\233"))) ) ;; defun

(defun a8basic-convert-to-txt ()
  "Convert from atari LST into text files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\233" nil t)
      (replace-match "\n"))) ) ;; defun

(defun a8basic-erase-numbers ()
  "Erase the lines numbers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward a8basic-line-number-regexp nil t)
      (replace-match ""))) ) ;; defun

(defun a8basic-insert-numbers (start increment)
  "Insert the line numbers.
Use START as the initial line number and INCREMENT as the number increment."
  (interactive "nStart?\nnIncrement?")
  (save-excursion
    (goto-char (point-min))
    (let ((num start))
      (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
	(insert (format "%s " num))
	(forward-line)
	(move-beginning-of-line nil)
	(setq num (+ num increment))))) ) ;; defun

(defun a8basic-renumber (&optional start increment)
  "Renumber the lines.
Use START as the starting number and INCREMENT as the line number increment.
By deault START is 10 and increment is 10."
  (interactive)
  (let ((startnum (if start start 10))
	(incrementnum (if increment increment 10)))
    (a8basic-erase-numbers)
    (a8basic-insert-numbers startnum incrementnum))
  ;; (a8basic-labels-to-line-numbers)
  ) ;; defun

(defun a8basic-beautify ()
  "Make the Basic code a little better for reading and programming."
  (interactive)
  (when (search-forward "\233" nil t)
    (a8basic-convert-to-txt))
  (a8basic-erase-numbers)
  ;; (a8basic-line-numbers-to-labels)
  ) ;; defun

(defun a8basic-uglify ()
  "Make the Basic code ready for the emulator."
  (interactive)
  (a8basic-erase-empty-lines)
  (a8basic-renumber 10 10)
  (let ((labels-alist (a8basic-search-labels-with-linenum)))
    (a8basic-inst-with-label-to-line-number "goto" labels-alist)
    (a8basic-inst-with-label-to-line-number "gosub" labels-alist)
    (a8basic-inst-with-label-to-line-number "trap" labels-alist)
    (a8basic-all-ongoto-label-to-line-number labels-alist))
  (a8basic-comment-all-labels)) ;; defun


(defun a8basic-erase-empty-lines ()
  "Erase empty lines in the current buffer."
  (flush-lines "^[[:space:]]*$" (point-min) (point-max)) ) ;; defun

;; --------------------
;; Label processing
;; --------------------

(defun a8basic-basic-line-num (&optional buffer-line-num)
  "Retrieve the Basic line number if exists, nil otherwise.
BUFFER-LINE-NUM is the buffer line number.  If not present, use the current line."
  (save-excursion
    (when buffer-line-num
      (forward-line (- buffer-line-num (line-number-at-pos))))
    
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\([[:digit:]]+\\) " nil t)
	;; there is a number
	(string-to-number (match-string 1))
      nil)) ) ;; defun


(defun a8basic-search-labels ()
  "Search for labels in the current buffer.
Return an alist with each label defined and the buffer's line number."
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward a8basic-line-label-regexp nil t)
	(setq result (push (cons (match-string-no-properties 1) (line-number-at-pos (match-beginning 1)))
			   result))
	(goto-char (match-end 0))))
    (reverse result))) ;; defun

(defun a8basic-search-labels-with-linenum ()
  "Search for labels in the current buffer.
Return an alist with each label defined an the Basic line number."
  (let ((results nil))
    (dolist (label-cons (a8basic-search-labels))
      (setq results (push (cons (car label-cons)
				(a8basic-basic-line-num (cdr label-cons)))
			  results)))
      (reverse results)) ) ;; defun

(defun a8basic-convert-label-to-line-num (labels-to-map label-alist)
  "Convert all labels in LABELS-TO-MAP into (Basic) line num.
Use the LABEL-ALIST association (a label string and number cons list) to map
them.
If LABELS-TO-MAP contains numbers (or string matching a number), then leave it.
If LABELS-TO-MAP contains label that are not in LABEL-ALIST, return nil for it.
Return a list of numbers."
  (cl-map 'list (lambda (label)
		  (if (string-match-p "[[:digit:]]+" label)
		      ;; It is not a label, leave it.
		      (string-to-number label)
		    ;; It is a label, change it.
		    (alist-get label label-alist nil nil 'string=)))
	   labels-to-map) ) ;; defun

(defun a8basic-comment-all-labels ()
  "Search for all labels and comment them.  No line number should be inserted."
  (save-excursion
    (goto-char (point-min))
    (dolist (label-cons (a8basic-search-labels))
      ;; place the cursor at the line
      (forward-line (- (cdr label-cons) (line-number-at-pos)))
      (goto-char (point-at-bol))
      ;; place the cursor just before the label
      (search-forward (car label-cons))
      (goto-char (match-beginning 0))
      
      (insert "REM "))) ) ;; defun

(defun a8basic-inst-with-label-to-line-number (name label-alist)
  "Convert the label used as parameter in an instruction into line number.
LABEL-ALIST is an alist of label string and the line-number.
NAME is the instruction to search.  These instruction should have the format
\"INSTRUCTION LABEL\", for instance: GOTO somewhere."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "%s[[:space:]]+\\(%s\\)"
				      name a8basic-label-regexp) nil t)
      ;; Found one instruction, is it a label?
      (let ((label (match-string-no-properties 1)))
	(unless (string-match-p "[[:digit:]]+" label)
	  ;; It is a label, replace it
	  (let ((line-num (alist-get label label-alist nil nil 'string=)))
	    (if line-num
		(replace-match (format "%s %s" name line-num))
	      (error (format "Label \"%s\" is founded but no line number is associated to it."
			     (match-string-no-properties 1))))))))) ) ;; defun

(defun a8basic-str-label-list-to-line-num (label-list-as-str label-alist)
  "Return a string formatted as a comma separated list of line numbers.
LABEL-LIST-AS-STR is a string with a comma separated label list.
LABEL-ALIST is an association list between label string and (Basic) line
numbers.
It decode the string, maps each label into line numbers and rebuild the string."
  (combine-and-quote-strings
   (cl-map 'list 'number-to-string
	    (a8basic-convert-label-to-line-num (split-string label-list-as-str "," nil "[[:space:]]+")
					       label-alist))
   ",") ) ;; defun


(defun a8basic-ongoto-label-to-line-number (label-alist)
  "Convert all labels into Basic line numbers in the current line.
The current line must have an ON GOTO statement.
LABEL-ALIST is a ilst of label strings with ther Basic line number."
  (save-excursion
    (goto-char (point-at-bol))
    (when (re-search-forward
	   "\\(on[[:space:]]+[^[:space:]]+[[:space:]]+goto \\)\\([[:alnum:]_-\\,[:space:]]+\\)" (point-at-eol) t)
      (let ((str (match-string-no-properties 2)))
	(delete-region (match-beginning 2) (match-end 2))
	(goto-char (match-end 1))
	(insert (a8basic-str-label-list-to-line-num str label-alist)))
      ;; return t
      t)) ) ;; defun


(defun a8basic-all-ongoto-label-to-line-number (label-alist)
  "Convert the labels into Basic line numbers on all ON GOTO instructions.
LABEL-ALIST is an alist of label string associated with their Basic line number."
  (save-excursion
    (while (re-search-forward "ON[[:space:]]+[^[:space:]]+[[:space:]]+GOTO" nil t)
      (goto-char (match-beginning 0))
      (a8basic-ongoto-label-to-line-number label-alist)
      (forward-line 1))) ) ;; defun

;; ----------------------
;; Charset
;; ----------------------

(defun a8basic-char-group (char)
  "Find the group in `a8basic-charset-map' where the CHAR is."
  (seq-find (lambda (elt)
	      (and (<= (car elt) char)
		   (<= char (cadr elt))))
	    a8basic-charset-map) ) ;; defun

(defun a8basic-char-escape-char (char)
  "Return the representation of the Basic ASCII character CHAR.
CHAR is an escaped character (not a typical symbol or alphanumeric character)."
  (let ((group (a8basic-char-group char)))
    (when group
      (nth (+ 2 (- char (car group))) group))) ) ;; defun

(defun a8basic-char-black-background (char)
  "Return the representation of the Basic ASCII character CHAR.
Consider that the given character has black background."
  (propertize (a8basic-char-a8ascii-to-utf (- char #x80))
	      'face 'a8basic-char-black-face)) ;; defun

(defun a8basic-char-a8ascii-to-utf (char)
  "Convert the old ASCII character to unicode representation string."
  (cond
   ((or (and (<= #x20 char) (<= char #x5f))
	(and (<= #x61 char) (<= char #x7a)))
    (propertize (char-to-string char)
		'face 'a8basic-char-normal))
   ((and (<= #x80 char)
	 (<= char #xff)
	 (/= #x9b char))
    ;; Black background character
    (a8basic-char-black-background char))
   (t
    (propertize (a8basic-char-escape-char char)
		'face 'a8basic-char-normal))) ) ;; defun

(defun a8basic-str-a8ascii-to-utf (str)
  "Convert a string from the old ASCII to its unicode representation."
  
  ) ;; defun

(defun a8basic-show-ascii-table ()
  "Show a new buffer with the ascii representation."
  (interactive)
  (with-current-buffer (get-buffer-create "*ASCII table*")
    (delete-region (point-min) (point-max))
    (switch-to-buffer-other-window (current-buffer))
    (insert "Dec Oct Hex Char \n")
    (dotimes (i 256)
      (insert (format "%3d %3o %3X %s \n" i i i (a8basic-char-a8ascii-to-utf i))))) ) ;; defun


(provide 'a8basic)
;;; a8basic.el ends here
