;;; bm-charname.el --- character name library

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: 

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

(defvar bm-charname-alist
  nil
  "Alist of charset and bm-charname-<charset>-alist.")

(defun bm-charname-load-charset-alist (charset)
  (or (cdr (assoc charset bm-charname-alist))
      (and (load (bm-charname-alist-filename charset) t)
	   (cdr (assoc charset bm-charname-alist)))))

(defun bm-charname-alist-filename (charset)
  (concat "bm-charname-" (symbol-name charset) "-alist"))

(defun bm-charname-lookup (c)
  ""
  (interactive)
  (let* ((charset (bm-charname-get-charset c))
	 (alist   (bm-charname-load-charset-alist charset))
	 (assoc   (assoc c alist)))
    (cond
     ((null assoc) nil)
     ((list assoc) (cdr assoc))
     (t (error "Data corrupted in %s" (bm-charname-alist-filename charset))))))
    
(defun bm-charname-get-charset (c)
  (let ((charset (car (split-char c))))
    (if (and (eq charset 'ascii) (> c ?\x7F) (<= c ?\xFF))
	'control-1
      charset)))



(provide 'bm-charname)

;;; bm-charname.el ends here
