;;; bm-complete.el --- compatibility routines for Emacs and XEmacs

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

(require 'bm-emacs)

(if (eq bm-emacs-variant 'emacs)

    (defalias 'bm-completing-read 'completing-read)

  (defun bm-completing-read (prompt collection &optional predicate
          require-match initial hist default inherit-input-method)
    ;; currently inherit-input-method is not implemented.
    (let ((str (completing-read prompt collection predicate
				require-match initial hist)))
      (if (and (string= str "") default)
	  default
	str))))


(defun bm-complete-default-prompt (prompt default)
  (if default (concat prompt "(default " default ") ")
    prompt))




(provide 'bm-complete)

;;; bm-complete.el ends here
