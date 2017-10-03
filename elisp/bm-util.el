;;; bm-util.el --- utility routines

;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>

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

;; Catchall library

;;; Code:

;;-------------------------------------------------------------------
;; Point(cursor) manipulation functions
;;-------------------------------------------------------------------
(defconst bm-token-list
  '("[ \t]+"		;space, tab
    "[\n]+"             ;newline
    "[!-/:-@[-`{-~]+"	;punctuation mark
    "[0-9A-Za-z]+"	;alphanumeric
    "\\cA+"		;Japanese 2-byte Alpha numeric character.
    "\\cC+"             ;Japanese 2-byte Kanji characters.
    "\\cG+"             ;Japanese 2-byte Greek character.
    "\\cH+"             ;Japanese 2-byte Hiragana character.
    "\\cK+"             ;Japanese 2-byte Katakana character.
    "\\cS+"             ;Japanese 2-byte symbol character.
    "\\cY+"             ;Japanese 2-byte Cyrillic character.
    ".")
"List of regexps for custom tokens.")

(defconst bm-token
  (concat "\\(" (mapconcat 'identity bm-token-list "\\)\\|\\(") "\\)")
"Regexp for custom token.")

(defun bm-forward-token (arg)
  "Move point forward ARG tokens.
Normally returns new point value.
If an edge of the buffer is reached, point is left there
and nil is returned."
  (interactive "p")
  (re-search-forward bm-token (point-max) t arg))

(defun bm-backward-one-token ()
  "Move backward one token.
Normally returns new point value.
If an edge of the buffer is reached, point is left there
and nil is returned."
  (interactive)
  (if (bobp) nil
    (let* ((pre (char-to-string (char-before)))
	   (class nil)
	   (i 0))
      ;;what is the charclass of the previous char?
      (while (and (not class) (< i (length bm-token-list)))
	(if (string-match (nth i bm-token-list) pre)
	    (setq class (nth i bm-token-list)))
	(setq i (1+ i)))
      ;;go backward until char in different class appears.
      (while (string-match class (char-to-string (char-before)))
	(backward-char))
      (point))))


;;-------------------------------------------------------------------
;; Text manipulation functions
;;-------------------------------------------------------------------

(defun bm-delete-token (num)
  "Delete NUM pieces of custom tokens."
  (interactive "p")
  (delete-region (point) (progn (bm-forward-token num) (point))))


(defun bm-insert-string-rectangle (start end str)
  "Insert STR in lines between START to END at the column of START."
  (interactive "r\nsInsert string rectangle: ")
  (operate-on-rectangle
   (function (lambda (startpos begextra endextra)
	       (goto-char startpos)
	       (insert str)))
   start end t))

(defun bm-delete-syntax-char-within-line (syn &optional backward)
  "Delete characters in syntax catogory SYN after the point.
Delete characters before the point when BACKWARD is non-nil."
  (let ((char-bora   (funcall (if backward 'char-before 'char-after)))
	(delete      (if backward 'delete-backward-char 'delete-char)))
    (when (and char-bora
	       (/= char-bora ?\n)
	       (string-match (regexp-quote (string (char-syntax char-bora)))
			     syn))
      (funcall delete 1)
      (bm-delete-syntax-char-within-line syn backward))))

(defun bm-right-justify-from-here ()
  "Right justify non-whitespace text after the point."
  (interactive)
  (bm-delete-syntax-char-within-line " " "backward")
  (bm-delete-syntax-char-within-line " ")
  (let* ((pos (point))
	 (col (current-column))
	 (len (progn (end-of-line) (- (current-column) col)))
	 (to  (- fill-column len)))
    (goto-char pos)			; go back where we were.
    (cond
     ((> len fill-column) nil)		; there's nothing we can do.
     ((< to col) (insert ?\n) (indent-to to))
     (t (indent-to to)))))

;;;; tab setting functions
(defun bm-tab8 ()
  "Set `tab-width' to 8."
  (interactive)
  (setq tab-width 8) (redraw-display))

(defun bm-tab4 ()
  "Set `tab-width' to 4."
  (interactive)
  (setq tab-width 4) (redraw-display))


;;-------------------------------------------------------------------
;; Buffers, Windows and Frames manipulation functions
;;-------------------------------------------------------------------
(defun bm-kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer nil))

(defun bm-kill-buffer-and-window ()
  "Kill selected window and current buffer."
  (interactive)
  (let ((win (selected-window)))
    (kill-buffer (window-buffer win))
    (condition-case nil
	(delete-window win)
      (error) ;suppress error message in case win is a sole window
      )))

(defun bm-get-shebang-line (&optional buffer)
  (if (not buffer) (setq buffer (current-buffer)))
  (save-excursion
    (save-restriction
      (save-match-data
	(set-buffer buffer)
	(widen)
	(goto-char (point-min))
	(if (looking-at "^#!.*$") (match-string 0) nil)))))

(defun bm-exec-script-buffer ()
  (interactive)
  (let ((script-name (buffer-file-name)))
    (if (not (bm-get-shebang-line)) (error "Shebang line (#!..) not found"))
    (and (buffer-modified-p)
	 (y-or-n-p (format "Save buffer %s first? " (buffer-name)))
	 (save-buffer))
    (and (not (file-executable-p (buffer-file-name)))
	 (y-or-n-p (format "Chmod file %s first? " (buffer-file-name)))
	 (set-file-modes (buffer-file-name)
			 (logior ?\111 (file-modes (buffer-file-name)))))
    (bm-shell)
    (goto-char (point-max))
    (insert (concat script-name " "))))


;;;(defun bm-switch-to-buffer-other-window-other-buffer ()
;;;  (interactive)
;;;  (switch-to-buffer-other-window (other-buffer)))

(defun bm-switch-to-other-buffer ()
  "Switch to other buffer in the selected window."
  (interactive)
  (switch-to-buffer (other-buffer)))


(defun bm-switch-to-scratch-buffer ()
  "Switch to *scratch* buffer."
  (interactive) (switch-to-buffer "*scratch*"))


;;-------------------------------------------------------------------
;; Shell
;;-------------------------------------------------------------------
(defun bm-shell ()
  "Run an inferior shell in another window."
  (interactive)
  ;(switch-to-buffer-other-window "*eshell*")
  ;(eshell)
  (switch-to-buffer-other-window "*shell*")
  (shell)
  )


(defsubst bm-time-elapsed (start end)
  "Stolen from elp.el"
  (+ (* (- (car end) (car start)) 65536.0)
     (- (car (cdr end)) (car (cdr start)))
     (/ (- (car (cdr (cdr end))) (car (cdr (cdr start)))) 1000000.0)))



;;;
(defun complement-2lines ()
  (interactive)
  (save-excursion
    (let* ((top (region-beginning))
           (bottom (region-end))
           (beginning1 (progn (goto-char top) (beginning-of-line) (point)))
	   (end1 (progn (end-of-line) (point)))
           (line1 (buffer-substring beginning1 end1))
           (length1 (length line1))
	   (beginning2 (progn (goto-char bottom) (beginning-of-line) (point)))
	   (end2 (progn (end-of-line) (point)))
           (line2 (buffer-substring beginning2 end2))
           (length2 (length line2))
	   (prefix-length (if (string= line1 line2)
                              (length line1)
                            (1- (abs (compare-strings line1 0 nil
                                                      line2 0 nil)))))
	   (prefix (substring line1 0 prefix-length))
	   (suffix-length (if (string= line1 line2)
                              0
                            (let* ((rev1 (concat (reverse (append line1 nil))))
                                   (rev2 (concat (reverse (append line2 nil)))))
                              (min (1- (abs (compare-strings rev1 0 nil
                                                             rev2 0 nil)))
                                   (- length1
                                      (or (string-match "[^0-9]+$" line1)
                                          length1))
                                   (- length2
                                      (or (string-match "[^0-9]+$" line2)
                                          length2))))))
	   (suffix (substring line1 (- (length line1) suffix-length)))
           (stem1 (substring line1
                             prefix-length (- (length line1) suffix-length)))
           (stem2 (substring line2
                             prefix-length (- (length line2) suffix-length)))
           (i (1+ (string-to-number stem1)))
           (max (string-to-number stem2))
           (format-string (format "\n%s%%0%dd%s" prefix (length stem1) suffix)))
      (goto-char end1)
      (while (< i max)
        (insert (format format-string i))
        (setq i (1+ i))))))


(provide 'bm-util)

;;; bm-util.el ends here
