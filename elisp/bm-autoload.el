;;; bm-autoload.el --- autoload.el enhancement and Emacsen compatibility layer.

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, lisp

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
;; autoload.el enhancement and Emacsen compatibility layer.
;; This library was tested with FSF Emacs 20.6.2 and XEmacs 21.1.8.



;;; Code:
(require 'autoload)

;; Difference of update-file-autoloads's behavior between FSF Emacs and XEmacs
;;
;; * When the file denoted by `generated-autoload-file' doesn't exist or
;;   its size is 0;
;;   FSF Emacs   : signals an error.
;;   XEmacs      : creats one and proceeds.
;;
;; * When the source file binds `generated-autoload-file' locally;
;;   FSF Emacs   : respects the local binding.
;;   XEmacs      : ignores the local binding.
;;
;; * When the source file is older than `generated-autoload-file'.
;;   FSF Emacs   : doesn't care and proceeds.
;;   XEmacs      : silently returns doing nothing.
;;                 
;;
;; * When the source file is the same name as `autoload-file-name'.
;;   FSF Emacs   : doesn't care and proceeds.
;;   XEmacs      : silently returns doing nothing.

(defun bm-autoload-update-from-directories (autoload-file dir-list)
  "Update AUTOLOAD-FILE with all the current autoloads from DIR-LIST.
And delete stale autoloads.
This is a wrapper of FSF Emacs's `update-autoloads-from-directories' and
XEmacs's update-autoloads-from-directory."
  (let ((generated-autoload-file autoload-file)
	autoload-buf)
    (cond
     ((fboundp 'update-autoloads-from-directories);; for emacs
      (apply 'update-autoloads-from-directories dir-list))

     ((fboundp 'update-autoloads-from-directory);; for xemacs
      (let ((noninteractive t)
	    ;; pretend to be noninteractive, to prevent autoload file from
            ;; getting saved after one dir is checked, otherwise newer source
            ;; files in other dirs won't get checked.
	    autoload-buf)
	(while dir-list
	  (update-autoloads-from-directory (car dir-list))
	  (setq dir-list (cdr dir-list)))
	(when (setq autoload-buf (get-file-buffer generated-autoload-file))
	  (with-current-buffer autoload-buf (save-buffer)))))
     (t (error "update-autoloads-from-director(y|ies) must be defined.")))

    (when (setq autoload-buf (get-file-buffer generated-autoload-file))
      ;; When not saved at this point, something went wrong
      (if (buffer-modified-p autoload-buf)
	  (error "bm-autoload-update-from-directories: autoload buffer must've been saved!"))
      (kill-buffer autoload-buf))))


  


(provide 'bm-autoload)

;;; bm-autoload.el ends here
