;;; a8basic.el --- Some helpful functions for programming with 8-bit computer's Basic.

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Version: 0.1.0
;; Keywords: Basic, 8-bit computer
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

(defun a8basic-convert-to-lst ()
  "Convert from text into atari LST files."
  (interactive)
  (save-excursion
    (goto-char (point-min))    
    (replace-string "\n" "\233")
    )
  ) ;; defun

(defun a8basic-convert-to-txt ()
  "Convert from atari LST into text files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "\233" "\n")
    )  
  ) ;; defun

(defconst a8basic-line-number-regexp "^\\([[:digit:]]+\\)[[:space:]]+"
  "The regular expression to match the line numbers of the code.") ;; defconst

(defun a8basic-erase-numbers ()
  "Erase the lines numbers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp a8basic-line-number-regexp ""))) ;; defun

(defun a8basic-insert-numbers (start increment)
  "Insert the line numbers.
Use START as the initial line number and INCREMENT as the number increment."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((num start))
      (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
	(insert num " ")
	(forward-line)
	(move-beginning-of-line nil)
	(setq num (+ num increment))))) ) ;; defun

(defun a8basic-comment-label-line (line-num)
  "Go to the LINE-NUM position and comment it if it is possible."
  (save-excursion
    (when (search-forward-regexp (concat "^" (number-to-string line-num) "[[:space:]]+") nil nil)
      ;; Line found
      (unless (string-match-p (concat "^" line-num "[[:space:]]+rem ")
			      (buffer-substring (point-at-bol) (point-at-eol)))
	;; It does not have the REM, add it!
	(replace-string (concat "^" line-num "[[:space:]]+")
			(concat line-num " REM "))))) ) ;; defun


(defun a8basic-label-to-line-numbers (label)
  "Search for LABEL and change it into its line number."
  (let ((line-num (a8basic-search-label label)))
    (a8basic-comment-label-line line-num)
    (a8basic-intruction-labels-to-line-number label line-num)) ) ;; defun

(defun a8basic-labels-to-line-numbers ()
  "Replace labels into REM label."
  (while (search-forward-regexp a8basic-label-regexp nil nil)
    (a8basic-label-to-line-number (match-string 1))) ) ;; defun


(defun a8basic-renumber (&optional start increment)
  "Renumber the lines.
Use START as the starting number and INCREMENT as the line number increment.
By deault START is 10 and increment is 10."
  (interactive)
  (a8basic-erase-numbers)
  (a8basic-insert-numbers start increment)
  (a8basic-modify-labels) ) ;; defun

(provide 'a8basic)
;;; a8basic.el ends here
