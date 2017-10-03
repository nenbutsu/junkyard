;;; bm-show-code.el -- show-char-minor mode

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

(require 'bm-charname)

;;; bm-show-code-mode minor mode -------------------------------------

;; register to minor-mode-alist
(or (assq 'bm-show-code-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(bm-show-code-mode " ShowCode") minor-mode-alist)))


(defvar bm-show-code-mode nil
  "Minor mode `bm-show-code-mode' variable.")
(make-variable-buffer-local 'bm-show-code-mode)

;;				    [|(elisp:Minor Mode Conventions)|]
;;;###autoload
(defun bm-show-code-mode (arg)
  "Toggle bm-show-code-mode minor mode.
With arg, turn the mode on if arg is positive, off otherwise.
bm-show-code-mode minor mode shows the information of the char on the point.
The information is character name, code values and so on."
  (interactive "P")
  (setq bm-show-code-mode
	(if (null arg) (not bm-show-code-mode)
	  (> (prefix-numeric-value arg) 0)))
					;;[|(elisp:Command Overview)|]
  (if bm-show-code-mode (add-hook 'post-command-hook 'bm-show-code-mode-show)
    (remove-hook 'post-command-hook 'bm-show-code-mode-show))
  (force-mode-line-update))

(defun bm-show-code-mode-show ()
  "bm-show-code-mode minor mode function."
  (if bm-show-code-mode
    (bm-show-code-on-point)))

(defun bm-show-code-on-point ()
  "Show code point, char name and charset of the char under the point."
  (interactive)
  (message (bm-show-code-str (char-after))))


;;; String format functions -----------------------------------------
(defun bm-show-code-str (c)
  (interactive)
  (if (null c) ""
    (let ((charset (bm-charname-get-charset c)))
      (concat
       (bm-show-code-code-str charset c) " "
       (bm-show-code-charset-name charset) ": "
       (bm-show-code-char-name c)))))

(defun bm-show-code-charset-name (charset)
  (interactive)
  (symbol-name charset))

(defun bm-show-code-char-name (c)
  (interactive)
  (let ((entry (bm-charname-lookup c)))
    (concat (and (cdr (assoc 'ACRO entry)) 
		 (concat "(" (cdr (assoc 'ACRO entry)) ")"))
	    (cdr (assoc 'US entry)) " "
	    (cdr (assoc 'JP entry)))))
  

(defun bm-show-code-code-str (charset c)
  (interactive)
  (format "%-11s" (bm-show-code-format-byte-list (cdr (split-char c)))))

(defun bm-show-code-format-byte-list (l)
  (interactive)
  (let ((str "hex:"))
    (while l
      (setq str (concat str " " (format "%02X" (car l))))
      (setq l (cdr l)))
    str))







(provide 'bm-show-code)

;;; bm-show-code.el ends here
