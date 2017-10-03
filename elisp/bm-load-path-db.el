;;; bm-load-path-db.el --- 

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:

(defun bm-load-path-db (emacs)
  (let ((symbol (read (bm-load-path-db-name emacs))))
    (require symbol)
    (eval symbol)))

(defun bm-load-path-db-name (emacs)
  (format "bm-load-path-db-%s" emacs))

(defun bm-load-path-db-update (emacs path-list)
  (let* ((name (bm-load-path-db-name emacs))
	 (el  (locate-library (concat name ".el")))
	 (elc (locate-library (concat name ".elc")))
	 list)
    ;; delete compiled file (.elc) if older than source file (.el)
    (when (and el elc (file-newer-than-file-p el elc))
	(delete-file elc)
	(setq elc nil))

    (if (and (load name "missing-ok")
	     (equal (symbol-value (read name)) path-list))
	"up-to-date"
      (bm-load-path-db-write emacs path-list 
			     (and (stringp el) (file-name-directory el))))))


(defun bm-load-path-db-write (emacs path-list &optional dir)
  (unless dir
    (require 'bm-local-policy)
    (setq dir bm-elisp-data-dir))

  (let* ((var (read (bm-load-path-db-name emacs)))
	 (file (concat (symbol-name var) ".el"))
	 (desc (format "load-path for %s" emacs)))

    (with-temp-buffer
      (insert
       (format ";;; %s --- %s.\n" file desc)
       "\n"
       ";;; This file is generated by bm-elisp-idx-make.\n"
       ";;; DON'T EDIT THIS FILE.\n"
       "\n"
       (format "(defvar %s\n" var)
       "  '")
      (prin1 path-list (current-buffer))
      (insert
       "\n"
       (format "  \"%s.\"\n" desc)
       ")\n\n"
       (format "(provide '%s)\n" var)
       (format ";;; %s ends here" file))
      (write-file (expand-file-name file dir)))))




(provide 'bm-load-path-db)

;;; bm-load-path-db.el ends here