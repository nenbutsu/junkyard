;;; follow-link-at-point.el --- follow link (filepath, url, etc) at point

;; Copyright (C) 2006 Yuji Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, help, abbrev

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

(require 'ffap)
(require 'info)

(defun flap-find-url-at-point ()
  (let* ((ffap-url-regexp
          ;; extracted from ffap.el
          (concat
           "\\`\\("
           "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
           "\\|"
           "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
           "\\)."                       ; require one more character
           )))
    (ffap-url-at-point)))

(defun flap-flash-link (link)
  (let* ((regexp (regexp-quote link))
         (start (save-excursion
                  (save-match-data
                    (while (and (/= (point) (point-min))
                                (not (looking-at regexp)))
                      (backward-char)))
                  (point)))
         (end (+ start (length link))))
    ;; LINK might not exist in the text as is, because ffap-file-at-point
    ;; might have touched up LINK.
    (when (or (/= start (point-min)) (looking-at regexp))
      (flap-flash start end))))

(defvar flap-info-spec-regexp
  "([0-9a-zA-Z.+-]+)[0-9a-zA-Z.+-][ 0-9a-zA-Z.+-_]*[0-9a-zA-Z.+-]"
  "Regexp for info spec. e.g. \"(libc)Top\".")

(defun flap-find-info-spec-at-point ()
  (save-excursion
    (let* ((pos (point))
           (min (progn (beginning-of-line) (point)))
           (max (progn (end-of-line) (point))))
      (save-match-data
	(goto-char pos)
	(when (and (or (equal (char-after) ?\() (search-backward "(" min t))
		   (looking-at flap-info-spec-regexp)
		   (>= (match-end 0) pos))
	  (buffer-substring (point) (match-end 0)))))))


(defun flap-flash (start end &optional face tick)
  "Flash text between START and END in current buffer."
  (or face (setq face 'highlight))
  (or tick (setq tick 3000))

  (let ((overlay (make-overlay start end)))
    (unwind-protect
        (let ((i 0))
          (overlay-put overlay 'face face)
          (sit-for 0 300)
          (while (< i tick) (setq i (1+ i)))))
    (delete-overlay overlay)))

(defun flap-follow-link-at-point ()
  (interactive)
  (let ((link (flap-find-info-spec-at-point)))
    (if link
        (progn (flap-flash-link link)
               (Info-goto-node link))
      (setq link (flap-find-url-at-point))
      (if link
          (progn (flap-flash-link link)
                 (find-file-at-point (ffap-fixup-url link)))
        (setq link (ffap-file-at-point))
        (if link
            (progn (flap-flash-link link)
                   (find-file-at-point link))
          (setq link (flap-find-info-spec-at-point))
        
          )))))


(provide 'follow-link-at-point)

;;; follow-link-at-point.el ends here
