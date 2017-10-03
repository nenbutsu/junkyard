;;; bm-syntax.el --- Syntax table utilities
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;;  $Id: bm-syntax.el,v 1.1 2004-06-05 08:05:19 yuji Exp $
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; 
;;

;;; History:
;;

;;; Code:

(defvar bm-syntax-class-list
  ;; Mnemonic  Description               Syntax code
  '(("-"       "whitespace"                 0)
    ("."       "punctuation"                1)
    ("w"       "word constituent"           2)
    ("_"       "symbol constituent"         3)
    ("("       "open parenthesis"           4)
    (")"       "close parenthesis"          5)
    ("'"       "expression prefix"          6)
    ("\""      "string quote"               7)
    ("$"       "paired delimiter"           8)
    ("\\"      "escape"                     9)
    ("/"       "character quote"           10)
    ("<"       "comment starter"           11)
    (">"       "comment ender"             12)
    ("@"       "inherit"                   13)
    ("N/A"     "generic comment delimiter" 14)
    ("N/A"     "generic string delimiter"  15))
  "List of syntax class infomation.")

(defvar bm-syntax-flag-list
  '(("1"   "the start of a two-character comment-start sequence.")
    ("2"   "the second character of a two-character comment-start sequence.")
    ("3"   "the start of a two-character comment-end sequence.")
    ("4"   "the second character of  a two-character comment-end sequence.")
    ("b"   "the alternative \"b\" comment style.")
    ("p"   "an additional \"prefix character\" for Lisp syntax."))
  "List of syntax flag information.")



(defun bm-syntax-code-to-class (code)
  "Return syntax class info list corresponding to CODE syntax code."
  (nth (logand code ?\xFFFF) bm-syntax-class-list))

(defun bm-syntax-code-to-flag (code &optional flags)
  "Return syntax flag info list corresponding to CODE syntax code."
  (if (null flags) (setq flags bm-syntax-flag-list))
  (cond
   ((>= code (lsh 1 22)) (error "Invalid syntax-code value(0x%x)" code))
   ((<= code ?\xFFFF) ())
   ((/= (logand code ?\x10000) 0)
    (cons (car flags) (bm-syntax-code-to-flag (lsh code -1) (cdr flags))))
   (t (bm-syntax-code-to-flag (lsh code -1) (cdr flags)))))
  
(defun bm-syntax-table-entry-to-descriptor-list (entry)
  "Return syntax descriptor info list corresponding to syntax table ENTRY."
  (list (bm-syntax-code-to-class (car entry))
	(list (if (cdr entry) (char-to-string (cdr entry)) " ")
	      "matching character.")
	(bm-syntax-code-to-flag (car entry) bm-syntax-flag-list)))

(defun bm-syntax-table-entry-to-descriptor (entry)
  "Return syntax descriptor string equivalent to syntax table ENTRY."
  (interactive)
  (let ((list (bm-syntax-table-entry-to-descriptor-list entry)))
    (concat (caar list)
	    (car (cadr list))
	    (mapconcat 'car (nth 2 list) ""))))

;;;###autoload
(defun bm-syntax-show-descriptor (char)
  "Show syntax descriptor string for CHAR.
Use current buffer's syntax table."
  (interactive "sEnter char: ")
  (if (stringp char) (setq char (string-to-char char)))
  (let ((list (bm-syntax-table-entry-to-descriptor-list 
	       (aref (syntax-table) char))))
    (message "CLASS: %s MATCH: %s FLAGS: %s"
	     (nth 1 (car list))
	     (car (cadr list))
	     (mapconcat 'car (nth 2 list) ""))))
	   

(provide 'bm-syntax)
;;; bm-syntax.el ends here
