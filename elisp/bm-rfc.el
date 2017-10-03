;;; bm-rfc.el --- skeletal emacs lisp library file

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

(defun bm-rfc-name2path (arg)
  "Return file path of RFC file ARG."
  (interactive "sRFC file name: ")
  (setq arg (downcase arg))
  (string-match "^[^0-9]*\\([0-9]+\\)" arg)
  (let* ((num (string-to-number (match-string 1 arg)))
	 ;(dir (concat (getenv "DICDIR") "/rfc")))
	 (dir "/dosc/data/rfc/rfc/")) ; temporary hack 2000/02/24
    (concat dir "rfc" (number-to-string num) ".txt")))

(provide 'bm-rfc)

;;; bm-rfc.el ends here
