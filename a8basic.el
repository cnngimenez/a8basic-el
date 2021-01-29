;;; a8basic.el --- Helpful functions for programming with 8-bit Basic

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Version: 0.1.0
;; Keywords: tools, convenience, Basic, 8-bit computer
;; URL: https://github.com/cnngimenez/a8basic-el
;; Package-Requires: ((emacs "24.5"))

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

(defconst a8basic-label-regexp "^[[:alnum:]]+:[[:space:]]+$"
  "Regexp for the labels.") ;; defconst

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

(defun a8basic-comment-label-line (line-num)
  "Go to the LINE-NUM position and comment it if it is possible."
  (save-excursion
    (when (search-forward-regexp (format "^%s[[:space:]]+" line-num) nil nil)
      ;; Line found
      (unless (string-match-p (concat "^" line-num "[[:space:]]+rem ")
			      (buffer-substring (point-at-bol) (point-at-eol)))
	;; It does not have the REM, add it!
	(re-search-forward (concat "^" line-num "[[:space:]]+") nil t)
	(replace-match (concat line-num " REM "))))) ) ;; defun

(defun a8basic-search-label (label)
  "Serach for the label LABEL string and return its position."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" label ":[[:space:]]+$") nil t)
	(match-beginning 0)
      nil)) ) ;; defun

(defun a8basic-simple-inst-labels-to-line-number (name label line-num)
  "Change the LABEL string with the LINE-NUM number for the given instruction.
NAME is the instruction to search.  These instruction should have the format
\"INSTRUCTION LABEL\", for instance: GOTO somewhere."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat name "[[:space:]]+" label) nil t)
      (replace-match (format "%s %s" name line-num)))) ) ;; defun

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
  (while (search-forward-regexp a8basic-label-regexp nil t)
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

(provide 'a8basic)
;;; a8basic.el ends here
