;;; bm-elisp-file-idx.el --- index of elisp library files.

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
(defun bm-elisp-file-idx ()
  (let ((dir-list load-path)
	file-list alist)
    (while dir-list
      (setq file-list
	    (if (file-accessible-directory-p (car dir-list))
		(directory-files (car dir-list) nil "\\.el\\(\\.gz\\)?$" t)
	      nil))
      (while file-list
	(setq alist (cons (cons (car file-list) (car dir-list)) alist))
	(setq file-list (cdr file-list)))
      (setq dir-list (cdr dir-list)))
    (nreverse alist)))

(defvar bm-elisp-file-idx (bm-elisp-file-idx) "alist of emacs lisp files.")

(defun bm-elisp-file-idx-rebuild ()
  "Rebuild elisp file index variable bm-elisp-file-idx."
  (interactive)
  (setq bm-elisp-file-idx (bm-elisp-file-idx)))


(provide 'bm-elisp-file-idx)

;;; bm-elisp-file-idx.el ends here
