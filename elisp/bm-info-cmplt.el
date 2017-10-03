;;; bm-info-cmplt.el --- skeletal emacs lisp library file

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
(require 'info)

(defconst bm-info-menu-item-regexp
  "^\\* \\(%s.*\\): \\(([^)]+)[^.]*\\)"
  "")

(defun bm-info-cmplt (string predicate flag)
  "Programmed completion function of man page spec. for `completing-read'.
see \"(elisp)Programmed Completion\"."
  (cond 
   ((null flag)
    (try-completion string (bm-info-cmplt-get-alist string) predicate))
   ((eq flag t)
    (all-completions string (bm-info-cmplt-get-alist string) predicate))
   ((eq flag 'lambda)
    ;; `t' if STRING is an exact match for some possibility, otherwise `nil'
    (let ((cell (assoc string (bm-info-cmplt-get-alist string))))
      (if (and cell
               (or (null predicate)
                   (funcall predicate cell)))
          t nil)))
   (t   (error "bm-info-cmplt: Invalid FLAG `%s'" (prin1-to-string flag)))))


(defmacro bm-info-cmplt-with-temp-buffer (&rest forms)
  (if (featurep 'xemacs)
      (list 'save-selected-window
            (list 'other-window 1)
            (list 'save-window-excursion
                  (list 'with-temp-buffer
                        (cons 'progn forms))))
    (list 'with-temp-buffer
          (cons 'progn forms))))

(def-edebug-spec bm-info-cmplt-with-temp-buffer (body)) ; debugging info


(defun bm-info-cmplt-get-alist (menu-item)
  "Return alist of *possible* completions of MAN-SPEC."
  (let* ((regexp (format bm-info-menu-item-regexp menu-item))
         alist item spec)
    (bm-info-cmplt-with-temp-buffer
     (Info-mode)
     (Info-directory)
     (goto-char (point-min))
     (while (re-search-forward regexp nil t)
       (setq item (downcase (match-string 1))
             spec (match-string 2))
       (set-text-properties 0 (length item) nil item)
       (set-text-properties 0 (length spec) nil spec)
       (setq alist (cons (list item spec) alist))))
    alist))







(provide 'bm-info-cmplt)

;;; bm-info-cmplt.el ends here
