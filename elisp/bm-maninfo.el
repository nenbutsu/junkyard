;;; bm-maninfo.el --- skeletal emacs lisp library file

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local

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
(require 'bm-mcmplt)
(require 'bm-man-cmplt)
(require 'bm-man)
(require 'bm-info-cmplt)
(require 'info)
(require 'man)
(require 'cl)

;;;(require 'bookmark)
;;;(bookmark-maybe-load-default-file)

(defconst bm-maninfo-collection-predicate-alist
  (list '(bm-info-cmplt)
  ;;;      (list bookmark-alist)
        '(bm-man-cmplt))

  ;;;(list (cons (mapcar 'list (bm-info-cmplt "" nil t)) nil)
  ;;;      (cons (mapcar 'list (bm-man-cmplt "" nil t))  nil)
  ;;;      (list bookmark-alist))
  "")


;;;###autoload
(defun bm-maninfo (spec)
  (interactive
   (progn
     (if current-prefix-arg
         (setq bm-maninfo-collection-predicate-alist
               (list
                ;;;(list bookmark-alist)
                (cons (mapcar 'list (bm-info-cmplt "" nil t)) nil)
                (cons (mapcar 'list (bm-man-cmplt "" nil t))  nil))))
     (let ((bm-mcmplt-collection-predicate-alist
                      bm-maninfo-collection-predicate-alist))
                 (flet ((icomplete-simple-completing-p
                         ()
                         (and (window-minibuffer-p (selected-window))
                              (not executing-kbd-macro))))
                   (list (completing-read "Maninfo: " 'bm-mcmplt nil t))))))
  (let ((cell ))
    (cond
     ((assoc spec (bm-info-cmplt-get-alist spec))
      (Info-goto-node (cadr (assoc spec (bm-info-cmplt-get-alist spec)))))
     ;;;((assoc spec bookmark-alist)
     ;;; (bookmark-jump spec))
     (t (bm-man spec)))))







(provide 'bm-maninfo)

;;; bm-maninfo.el ends here
