;;; bm-el-autoload.el --- utilities for Emacs Lisp autoload facility.

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, lisp, maint

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

;; Symbols of autoload.el used in this library.
;;   var:    generated-autoload-file
;;   fun:    (update-file-autoloads file)
;;              Update the autoloads for FILE in `generated-autoload-file'
;;
;;           FSF peculiarities:
;;             * Signal an error if the size of `generated-autoload-file' is 0.
;;             * When non-interactive, don't save `generated-autoload-file'
;;               after update (but this is the right thing).
;;
;;           XEmacs peculiarities:
;;             * Do nothing if FILE is older than `generated-autoload-file'.
;;             * Do nothing if FILE is the same name as `autoload-file-name'.
;;             * When non-interactive, don't save `generated-autoload-file'
;;               after update (but this is the right thing).

;;; Code:
(require 'autoload) ;; autoload routines provided with Emacs core package.

(defun bm-el-autoload-update-file (src-file def-file)
  "Update DEF-FILE for autoloads of SRC-FILE using `update-file-autoloads'.
Signal an error if SRC-FILE locally binds the variable `generated-autoload-file' and the value is defferent from DEF-FILE."
  (setq def-file (expand-file-name def-file))
  (setq src-file (expand-file-name src-file))
  (let* ((src-existing-buffer (or (get-file-buffer src-file)
				  (get-file-buffer (file-truename src-file))))
	 (def-buffer (find-file-noselect def-file))
	 (generated-autoload-file def-file) ;; localize the global var
	 (src-buffer (or src-existing-buffer (find-file-noselect src-file))))
    (unwind-protect
	(progn
	  (with-current-buffer src-buffer
	    (if (and (local-variable-p 'generated-autoload-file src-buffer)
		     (not (string= (expand-file-name generated-autoload-file) 
				   def-file)))
		(error "Autoloads src file %s has local variable `generated-autoload-file' whose value is %s, which is different from %s." src-file generated-autoload-file def-file)))
	  (with-current-buffer def-buffer
	    ;; FSF update-file-autoload signal an error if buffer size is zero,
	    ;; so we add a null line.
	    (if (zerop (buffer-size)) (insert "\n")))
	  (update-file-autoloads src-file))
      (unless src-existing-buffer (kill-buffer src-buffer)))))

(defun bm-el-autoload-update-files (src-list def-file)
  (let (src result)
    (while src-list
      (setq src (car src-list))
      (condition-case err
	  (bm-el-autoload-update-file src def-file)
	(error
	 (setq result (cons (cons src (error-message-string err)) result))))
      (setq src-list (cdr src-list)))
    result))

(provide 'bm-el-autoload)

;;; bm-el-autoload.el ends here
