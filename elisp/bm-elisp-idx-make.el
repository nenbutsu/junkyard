;;; bm-elisp-idx-make.el --- make symbol name index of ELisp Info manual.

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
(require 'bm-elisp-idx-config)

(defvar bm-elisp-idx nil
  "Symbol index (alist) of Emacs Lisp info manual for GNU Emacs, XEmacs, ...
e.g.  Actual association list looks like this.
      ((emacs  (defun . \"Defining Functions\")
               (setq . \"Setting Variables\")
               ....)
       (xemacs (defun . \"Defining Functions\")
               (setq . \"Setting Variables\")
               ....))

      Find the elisp manual info node where the definition of `defun' are on.
      (assoc 'defun (cdr (assoc 'emacs bm-elisp-idx)))
      => (defun . \"Defining Functions\")
")

(defun bm-elisp-idx-build-and-install ()
  "Build and install symbol indices of elisp manual for all emacsen."
  (interactive)
  (let ((config-list bm-elisp-idx-config)
	config)
    (while config-list
      (setq config (car config-list))
      (bm-elisp-idx-install (car config))
      (setq config-list (cdr config-list)))))

(defun bm-elisp-idx-install (emacs)
  "Install symbol index file of elisp manual for EMACS."
  (if (null (assoc emacs bm-elisp-idx)) (bm-elisp-idx-build emacs))
  (let ((dir     (bm-elisp-idx-config emacs "idx-dir"))
	(file    (bm-elisp-idx-config emacs "idx-file"))
	(var     (bm-elisp-idx-config emacs "idx-var"))
	(feature (bm-elisp-idx-config emacs "idx-feature"))
	(desc    (format "symbol name index of ELisp Info manual for %s."
		      emacs)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert
       (format ";;; %s --- %s.\n" file desc)
       "\n"
       ";;; This file is generated by bm-elisp-idx-make.\n"
       ";;; DON'T EDIT THIS FILE.\n"
       "\n"
       (format "(defvar %s\n" var)
       "  '")
       (prin1 (cdr (assoc emacs bm-elisp-idx)) (current-buffer))
      (insert
       "\n"
       (format "  %S\n" desc)
       ")\n\n"
       (format "(provide '%s)\n" feature)
       (format ";;; %s ends here" file))
      (write-file (expand-file-name file dir)))))

(defun bm-elisp-idx-build (emacs)
  "Build symbol index of elisp manual for EMACS."
  (bm-elisp-idx-build-index emacs (bm-elisp-idx-file-list emacs))
  (assoc emacs bm-elisp-idx))

(defun bm-elisp-idx-build-index (emacs file-list)
  "Build symbol index of elisp manual for EMACS using FILE-LIST."
  (let ((toggled t)
	nodename)
    ;; turn on auto-compression-mode
    (while (null (auto-compression-mode)) (setq toggled nil))
    (unwind-protect ;; for auto-compression-mode restoration
	(with-temp-buffer
	  (save-match-data

	    (while file-list
	      (erase-buffer)
	      (insert-file-contents (car file-list))
	    
	      (while (re-search-forward 
		      "^ - \\([^:]+\\): \\([^\x00-\x20]+\\)" nil t)
		(save-match-data
		  (save-excursion
		    (re-search-backward "^\^_\n[^\n]+ Node: \\([^,\n]+\\)")
		    (setq nodename (match-string 1))))
		(bm-elisp-idx-register-definition
		 emacs nodename (match-string 1) (match-string 2)))

	      (setq file-list (cdr file-list)))))
      (if toggled (auto-compression-mode)))))

(defun bm-elisp-idx-register-definition (emacs nodename type symbol)
  "Register symbol index entry"
  (if (null (assoc emacs bm-elisp-idx))
      (setq bm-elisp-idx (cons (cons emacs ()) bm-elisp-idx)))
  (setcdr (assoc emacs bm-elisp-idx)
	  (cons (cons symbol nodename)
		(cdr (assoc emacs bm-elisp-idx)))))

(defun bm-elisp-idx-file-list (emacs)
  "Return a list of Elisp info files for EMACS."
  (or (directory-files (bm-elisp-idx-config emacs "info-dir")
		       t (bm-elisp-idx-config emacs "info-file-re"))
      (error "Can't find Elisp manual files for %s" emacs)))

(provide 'bm-elisp-idx-make)

;;; bm-elisp-idx-make.el ends here