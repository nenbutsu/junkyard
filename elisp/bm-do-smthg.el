;;; bm-do-smthg.el --- skeletal emacs lisp library file

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
(require 'bm-sfx)
(require 'bm-thing)

;;-----------------------
;; Action definition
;;-----------------------
(put 'bm-thing-existing-filename       'action 'bm-do-smthg-filename)
(put 'bm-thing-existing-elisp-filename 'action 'bm-do-smthg-elisp-filename)
(put 'bm-thing-existing-c-filename     'action 'bm-do-smthg-c-filename)
(put 'bm-thing-absolute-URI            'action 'bm-do-smthg-absolute-URI)
(put 'bm-thing-man                     'action 'bm-do-smthg-man)
(put 'bm-thing-info                    'action 'bm-do-smthg-info)
(put 'bm-thing-rfc                     'action 'bm-do-smthg-rfc)
(put 'bm-thing-bookmark                'action 'bm-do-smthg-bookmark)
(put 'bm-thing-symbol                  'action 'bm-do-smthg-symbol)
(put 'bm-thing-encoded-int             'action 'bm-do-smthg-encoded-int)
(put 'bm-thing-section                 'action 'bm-do-smthg-section)


;;-----------------------
;; Driver
;;-----------------------
;;;###autoload
(defun bm-do-smthg ()
  "Do something appropriate depending on the object at the point."
  (interactive)
  (let* ((thing-list (bm-thing-at-point))
	 (thing      (car thing-list))
	 result)
    (unless thing-list (message "Nothing recognized."))
    
    (while (and thing (null (setq result (bm-do-smthg-action thing))))
      (setq thing-list (cdr thing-list)
	    thing      (car thing-list)))
    (if result
	(list thing result)
      (beep)
      nil)))
    

(defun bm-do-smthg-action (thing)
  ""
  (let ((range (get thing 'range)))
    (when range
      (push-mark (car range))
      (bm-sfx-flash (car range) (cadr range)))
    (condition-case err
	(funcall (get thing 'action) thing)
      (error (message "%s" (error-message-string err)) nil))))



;;--------------------------------------------------------------
;; man
;;--------------------------------------------------------------
;; e.g.  cp(1)
(defun bm-do-smthg-man (thing)
  (require 'bm-man)
  (bm-man (eval thing)))


;;--------------------------------------------------------------
;; Info
;;--------------------------------------------------------------
;; e.g. (elisp)Excursions
(defun bm-do-smthg-info (thing)
  (require 'info)
  (Info-goto-node (eval thing))
  t)


;;--------------------------------------------------------------
;; RFC
;;--------------------------------------------------------------
;; e.g. "RFC-1468"
(defun bm-do-smthg-rfc (thing)
  (require 'bm-rfc)
  (let ((file (bm-rfc-name2path (eval thing))))
    (if (file-exists-p file)
	(find-file file)
      (error "%s not exists." file))))


;;--------------------------------------------------------------
;; Bookmark
;;--------------------------------------------------------------
;; e.g. {elisp.txt}
(defun bm-do-smthg-bookmark (thing)
  (require 'bookmark)
  (bookmark-jump (eval thing))
  t)


;;--------------------------------------------------------------
;; filename
;;--------------------------------------------------------------
;; e.g. simple.el
(defun bm-do-smthg-filename (thing)
  (let ((file (eval thing)))
    (if (file-exists-p file) (find-file file)
      (error "%s not exists." file))))

(defun bm-do-smthg-elisp-filename (thing)
  (condition-case err
      (bm-do-smthg-filename thing)
    (error
     (if (set thing (locate-library (file-name-nondirectory (eval thing))))
	 (bm-do-smthg-filename thing)
       (signal (car err) (cdr err))))))

(defun bm-do-smthg-c-filename (thing)
  (condition-case err
      (bm-do-smthg-filename thing)
    (error
     (if (set thing (car (bm-locate-c-library
			  (file-name-nondirectory (eval thing)))))
	 (bm-do-smthg-filename thing)
       (signal (car err) (cdr err))))))


;;--------------------------------------------------------------
;; URI
;;--------------------------------------------------------------
(defun bm-do-smthg-absolute-URI (thing)
  (browse-url (eval thing))
  ;;(message "URI action not implemented yet.")
  t)


;;--------------------------------------------------------------
;; Symbol
;;--------------------------------------------------------------
(defun bm-do-smthg-symbol (thing)
  (require 'bm-info)
  (cond
   ((or (eq major-mode 'emacs-lisp-mode)
	(eq major-mode 'lisp-interaction-mode)
	(and (eq major-mode 'Info-mode)
	     (string-match "lisp" (bm-info-current-file))))
    (require 'bm-find-elisp)
    (bm-find-elisp-symbol (eval thing))
    t)
   (t
    (lookup-pattern (eval thing)))
   (t (message "Action for symbol in %s not defined." major-mode)
      nil)))
    
;;--------------------------------------------------------------
;; Int
;;--------------------------------------------------------------
(defun bm-do-smthg-encoded-int (thing)
  (require 'bm-str)
  (bm-show-int (eval thing))
  t)

;;--------------------------------------------------------------
;; Section
;;--------------------------------------------------------------
(defun bm-do-smthg-section (thing)
  (push-mark)
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote (eval thing))))
  t)

(provide 'bm-do-smthg)

;;; bm-do-smthg.el ends here
