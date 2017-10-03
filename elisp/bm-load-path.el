;;; bm-load-path.el --- utilities for load-path

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
;; Utilities for load-path

;;; Code:


(defun bm-load-path-make-truename-alist (&optional dir-list)
  "Return alist whose association is (truename dir_which_have_the_truename...).
\"Truenames\" are obtained using `file-truename'.
Elements of alist and values are in reverse order of DIR-LIST."
  (unless dir-list
    (setq dir-list load-path))
  (let (dir truename-alist entry)
    (while dir-list
      (setq dir      (car dir-list)
	    dir-list (cdr dir-list)
	    entry    (assoc (file-truename dir) truename-alist))
      (if entry
	  (setcdr entry (cons dir (cdr entry)))
	(setq truename-alist
	      (cons (list (file-truename dir) dir) truename-alist))))
    truename-alist))
	

;;;###autoload
(defun bm-load-path-remove-redundant-directories ()
  "Remove redundant directories from load-path.
All the directories of the resulting load-path will have unique 
\"truenames\" (see `file-truename')."
  (interactive)
  (let* ((old-len  (length load-path))
	 (alist    (bm-load-path-make-truename-alist load-path))
	 (new-path alist) ;; recycle alist as the new load-path
	 removed true-name dir-list preceding-dir)
    (with-output-to-temp-buffer "*BM-LOAD-PATH*"
      (while alist
	;; association is (truename 0_or_more_redundant_dirs... dir)
	(setq truename  (car (car alist))
	      dir-list  (cdr (car alist))
	      dir       (nth (1- (length dir-list)) dir-list))
	(when (cdr dir-list) ;; when redundant dirs exist
	  (princ (format "truename: %s\n" truename))
	  (princ (format "remained: %s\n" dir))
	  (while (cdr dir-list)
	    (princ (format "removed : %s\n" (car dir-list)))
	    (setq dir-list (cdr dir-list)))
	  (princ "\n"))
	(setcar alist dir);; recycle alist as the new load-path
	(setq alist (cdr alist)))
      (setq new-path (nreverse new-path))

      (setq load-path new-path
	    removed   (- old-len (length load-path)))
      (if (zerop removed)
	  (princ "No redundant directory is found in load-path.")
	(princ (format "%s redundant director%s removed from load-path."
		       removed (if (= removed 1) "y is" "ies are"))))
      removed)))
  


(provide 'bm-load-path)

;;; bm-load-path.el ends here
