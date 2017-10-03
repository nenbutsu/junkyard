;;; bm-sfx.el --- Special Effects

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
(require 'bm-emacs)
(unless (fboundp 'make-overlay)
  (require 'overlay)) ;; for XEmacs

(defun bm-sfx-flash (start end &optional face tick)
  "Flash text between START and END in current buffer."
  (interactive "r")
  (or face (setq face 'highlight))
  (or tick (setq tick 3000))

  (let ((overlay (make-overlay start end)))
    (unwind-protect
	(let ((i 0))
	  (overlay-put overlay 'face face)
	  (sit-for 0 300)
	  (while (< i tick) (setq i (1+ i)))))
    (delete-overlay overlay)))




(provide 'bm-sfx)

;;; bm-sfx.el ends here
