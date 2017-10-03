;;; bm-find-rfc.el -- Utilities for the RFC series of the Internet documents.
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;;  Utilities for the RFC series of the Internet documents.

;;; History:
;; 

;;; Code:
(require 'bm-rfc-alist)
(require 'bm-rfc)

;;;###autoload
(defun bm-rfc-find (name)
  "Visit an RFC file designated by NAME.
NAME can be a protocol name or a RFC number.
A protocol name can be entered with completion."
  (interactive (list (bm-rfc-completing-read)))
  (let* ((cell (assoc name bm-rfc-alist))
	 (num  (string-to-number name)))
    (cond
     ((string= name "") nil)
     (cell (if (numberp (cdr cell)) (bm-rfc-find-file (cdr cell))
	     (find-file (cdr cell))))
     ((> num 1) (bm-rfc-find-file num))
     (t nil))))

(defun bm-rfc-find-file (num)
  "Visit an RFC file of number NUM.
Switch to a buffer visiting the number NUM of RFC series of
the Internet standard."
  (interactive "nFind RFC file: ")
    (find-file (bm-rfc-name2path (number-to-string num)))
    (goto-char (point-min)))

(defun bm-rfc-completing-read ()
  "Read the Internet protocol name or RFC number using the minibuffer."
  (eval-and-compile (require 'bm-complete))
  (let* ((word (thing-at-point 'word))
	 (completion-ignore-case t)
	 (predicate nil)
	 (require-match t)
	 (initial nil)
	 (hist nil)
	 (default (if (or (assoc word bm-rfc-alist)
			  (and word (string-match "^[0-9]+$" word)
			       (wholenump (string-to-number word))))
		      word))
	 (prompt (bm-complete-default-prompt "Find RFC: " default)))

    (eval-and-compile (require 'bm-complete))
    (bm-completing-read prompt 'bm-rfc-completion predicate require-match
			initial hist default)))

;; [|(Elisp:Programmed Completion)|]
(defun bm-rfc-completion (str pred flag)
"Protocol name and RFC number completion function.
A wrapper of `all-completions' and `try-completion'.
STR is the string to be completed.
PRED is a function to filter possible matches.
FLAG is one of t, nil and `lambda'."
  (let* ((num (if (string-match "^[0-9]+$" str) (string-to-number str) 0)))
    (cond
     ((eq flag 'lambda) ;; exact match?
      (or (> num 0) (assoc str bm-rfc-alist)))
     (flag ;; must return all possible completion strings
      (let ((all (all-completions str bm-rfc-alist pred)))
	(if (> num 0) (cons str all) all)))
     (t ;; t     = a unique and exact match
        ;; "STR" = the longest possible stem
        ;; nil   = not match
      (let ((try (try-completion str bm-rfc-alist pred)))
	(cond
	 ((null try) (if (> num 0) t nil))
	 ((> num 0) str)
	 (t try)))))))

(provide 'bm-find-rfc)

;;; bm-find-rfc.el ends here
