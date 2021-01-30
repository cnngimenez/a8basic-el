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

(defconst a8basic-label-regexp "[[:alnum:]_-]+"
  "The label regexp.") ;; defconst

(defconst a8basic-line-label-regexp (format "^[[:digit:]]*[[:space:]]*\\(%s\\):[[:space:]]*$"
				     a8basic-label-regexp)
  "Regexp for the label definitions (the in-line definition).") ;; defconst

(defconst a8basic-instructions-with-labels '("goto" "gosub" "trap")
  "These are simple instructions that can have labels.
For instance: \"GOTO somewhere\"
The labels can be changed into line numbers later.") ;; defconst

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

(defconst a8basic-line-number-regexp "^\\([[:digit:]]+\\)[[:space:]]+"
  "The regular expression to match the line numbers of the code.") ;; defconst

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

(defun a8basic-instruction-labels-to-line-number (label line-num)
  "Find all LABEL usage and replace it with the LINE-NUM number.
The label usage are one of the GOTO, GOSUB, TRAP or simmilar Basic instructions."
  (dolist (instname a8basic-instructions-with-labels)
    (a8basic-simple-inst-labels-to-line-number instname label line-num))
  ;; (a8basic-ongoto-inst-labels-to-line-number label line-num)
  ) ;; defun

(defun a8basic-label-to-line-number (label)
  "Search for LABEL and change it into its line number."
  (let ((line-num (a8basic-search-label label)))
    (a8basic-comment-label-line line-num)
    (a8basic-instruction-labels-to-line-number label line-num)) ) ;; defun

(defun a8basic-labels-to-line-numbers ()
  "Replace labels into REM label."
  (while (search-forward-regexp a8basic-line-label-regexp nil t)
    (a8basic-label-to-line-number (match-string 1))) ) ;; defun

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
  (when (search-forward "\233" nil t)
    (a8basic-convert-to-txt))
  (a8basic-erase-numbers)
  ;; (a8basic-line-numbers-to-labels)
  ) ;; defun

(defun a8basic-uglify ()
  "Make the Basic code ready for the emulator."
  (a8basic-erase-empty-lines)
  (a8basic-renumber 10 10)
  (a8basic-comment-all-labels)
  (let ((labels-alist (a8basic-search-labels-with-linenum)))
    (a8basic-inst-with-label-to-line-number "goto" labels-alist)
    (a8basic-inst-with-label-to-line-number "gosub" labels-alist)
    (a8basic-inst-with-label-to-line-number "trap" labels-alist)) ) ;; defun


(defun a8basic-erase-empty-lines ()
  "Erase empty lines in the current buffer."
  (flush-lines "^[[:space:]]*$") ) ;; defun

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
      ;; Found one instruction, replace the label
      (let ((line-num (alist-get (match-string-no-properties 1) label-alist
				 nil nil 'string=)))
	(if line-num
	    (replace-match (format "%s %s" name line-num))
	  (error (format "Label \"%s\" is founded but no line number is associated to it."
			 (match-string-no-properties 1)))))))) ;; defun


(provide 'a8basic)
;;; a8basic.el ends here
