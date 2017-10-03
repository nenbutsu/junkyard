;;; bm-char.el -- character handling library
;;
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;; This file is NOT part of GNU Emacs.
;;
;; $Id: bm-char.el,v 1.1 2004-06-05 08:05:19 yuji Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; 


;;; History:
;;

;;; Code:

;; Emacs coding system name         MIME name
;; japanese-shift-jis               SHIFT_JIS
;; iso-2022-jp                      ISO-2022-JP
;; japanese-iso-8bit                EUC-JP

;; Emacs character => list of encoded bytes
(defun bm-char-jisx0208-euc (c)
  (mapcar '(lambda (x) (+ x ?\x80)) (bm-char-jisx0208-jis c)))

(defun bm-char-jisx0208-sjis (c)
  (let ((rawstr (encode-coding-string (string c) 'japanese-shift-jis)))
    (list (aref rawstr 0) (aref rawstr 1))))
  
(defun bm-char-jisx0208-jis (c)
  (cdr (split-char c)))
  


(defun bm-char-jisx0212-euc (c)
  (mapcar '(lambda (x) (+ x ?\x80)) (bm-char-jisx0212-jis c)))

(defun bm-char-jisx0212-jis (c)
  (cdr (split-char c)))


(defun bm-char-jisx0201kata-euc (c)
  (list (+ ?\x8E80 (cadr (split-char c)))))
  
(defun bm-char-jisx0201kata-sjis (c)
  (list (+ ?\x80 (cadr (split-char c)))))

(defun bm-char-jisx0201kata-jis (c)
  (cdr (split-char c)))

(provide 'bm-char)

;;; bm-char.el ends here
