;;; bm-thing.el --- skeletal emacs lisp library file

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
(require 'bm-at-point)

;;(defun x ()
;;  (interactive)
;;  (let ((list (bm-thing-at-point))
;; 	(message ""))
;;    (while list
;;      (setq message (format "%s %s (%s)" message (car list) (eval (car list))))
;;      (setq list (cdr list)))
;;    (message "%s" message)))
;;(global-set-key [(control return)] 'x)

;;-----------------------
;; thing definition
;;-----------------------
(defvar bm-thing-existing-filename nil)
(put 'bm-thing-existing-filename 'at-point-p 'bm-at-point-existing-filename)
;(put 'bm-thing-existing-filename 'at-point-p 'bm-at-point-filename)

(defvar bm-thing-existing-elisp-filename nil)
(put 'bm-thing-existing-elisp-filename
     'at-point-p 'bm-at-point-existing-elisp-filename)

(defvar bm-thing-existing-c-filename nil)
(put 'bm-thing-existing-c-filename
     'at-point-p 'bm-at-point-existing-c-filename)

(defvar bm-thing-absolute-URI nil)
(put 'bm-thing-absolute-URI 'at-point-p 'bm-at-point-abs-uri-ref)

(defvar bm-thing-man nil)
(put 'bm-thing-man 'at-point-p 'bm-at-point-man)

(defvar bm-thing-info nil)
(put 'bm-thing-info 'at-point-p 'bm-at-point-info)

(defvar bm-thing-rfc nil)
(put 'bm-thing-rfc 'at-point-p 'bm-at-point-rfc)

(defvar bm-thing-bookmark nil)
(put 'bm-thing-bookmark 'at-point-p 'bm-at-point-bookmark)

(defvar bm-thing-symbol nil)
(put 'bm-thing-symbol 'at-point-p 'bm-at-point-symbol)

(defvar bm-thing-encoded-int nil)
(put 'bm-thing-encoded-int 'at-point-p 'bm-at-point-encoded-int)

(defvar bm-thing-section nil)
(put 'bm-thing-section 'at-point-p 'bm-at-point-section)


;;-----------------------
;; checklist
;;-----------------------
(defvar bm-thing-checklist
  '(bm-thing-symbol
    bm-thing-encoded-int
    bm-thing-existing-filename
    bm-thing-existing-elisp-filename
    bm-thing-existing-c-filename
    bm-thing-absolute-URI
    bm-thing-man
    bm-thing-info
    bm-thing-rfc
    bm-thing-bookmark
    )
  "Check list to tell what is at point. Ascending order of priority.
(the first item has the lowest priority.)")
(make-variable-buffer-local 'bm-thing-checklist)


;;-----------------------
;; Driver
;;-----------------------
(defun bm-thing-at-point (&optional checklist)
  ""
  (or checklist (setq checklist bm-thing-checklist))
  (let (thing-list thing range)
    (while checklist
      (setq thing (car checklist)
            range (funcall (get thing 'at-point-p)))
      (when range
	(put thing 'range range)
	(set thing (buffer-substring (car range) (cadr range)))
	(setq thing-list (cons thing thing-list)))
      (setq checklist (cdr checklist)))
    thing-list))
  





(provide 'bm-thing)

;;; bm-thing.el ends here
