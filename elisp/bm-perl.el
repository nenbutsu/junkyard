;;; bm-perl.el -- Utilities for Perl
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; Utilities for Perl


;;; History:
;; 

;;; Code:
(require 'compile)
(require 'bm-perl-alist)

(defvar bm-perl-docdir
  (substring (shell-command-to-string "fdb_id2file 35") 0 -1)
  "Local perl pod text file directory.")

(defvar bm-perl-functxt (concat bm-perl-docdir "/perlfunc.txt")
  "Location of `perlfunc.txt'.")

;;;###autoload
(defun bm-perl-find (word)
  "Visit Perl material denoted by WORD."
  (interactive (list (bm-perl-completing-read)))
  (let* ((cell (assoc word bm-perl-alist)))
    (cond
     ((string= word "") nil)
     ((cdr cell) (find-file (cdr cell)))
     ((string-match "^\\(.+\\)(\\(.+\\))$" word)
      (man (concat (match-string 2 word) " " (match-string 1 word))))
     (t (bm-perl-find-func word)))))

(defun bm-perl-find-func (name)
  "Visit the description of Perl function NAME in perlfunc man page."
  (interactive "sPerl function: ")
  (stringp name)
  (find-file bm-perl-functxt)
  (goto-char (point-min))
  (search-forward "  Alphabetical Listing of Perl Functions")
  (re-search-forward (concat "^    " name))
  (recenter 0))

;;;###autoload
(defun bm-perl-grep-perldoc (arg)
  "Grep Perl document with ARG."
  (interactive "sGrep perldoc: ")
  (let* ((default-directory (concat bm-perl-docdir "/")))
    (grep (concat "grep -Eni -e'" arg "' *.txt"))))
	     
(defun bm-perl-completing-read ()
  "Read a name of Perl stuff using the minibuffer."
  (eval-and-compile (require 'bm-complete))
  (let* ((word (thing-at-point 'word))
	 (completion-ignore-case t)
	 (predicate nil)
	 (require-match t)
	 (initial nil)
	 (hist nil)
	 (default (if (assoc word bm-perl-alist) word))
	 (prompt (bm-complete-default-prompt "Find perl: " default)))
    (bm-completing-read prompt bm-perl-alist  predicate require-match
			initial hist default)))

(provide 'bm-perl)

;;; bm-perl.el ends here
