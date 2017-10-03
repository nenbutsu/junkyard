;;; bm-at-point.el --- predicates for a thing at point.

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

;;;(defun x ()
;;;  (interactive)
;;;  (require 'bm-sfx)
;;;  (let ((range (bm-at-point-filename)))
;;;    (if range
;;; 	(progn
;;; 	  (bm-sfx-flash (car range) (cadr range))
;;; 	  (message "`%s'" (buffer-substring (car range) (cadr range))))
;;;      (message "Nothing found."))))
;;; 
;;;(global-set-key [(control return)] 'x)


;;--------------------------------------------------------------
;; filename
;;--------------------------------------------------------------
;; e.g. bm-at-point.el

(defvar bm-at-point-filename-chars
  "~A-Za-z0-9_.$#%,---")

(defvar bm-at-point-filename-char-re
  (format "[%s]" bm-at-point-filename-chars))

(defvar bm-at-point-no-filename-char-re
  (format "[^/%s]" bm-at-point-filename-chars))

(defvar bm-at-point-filename-re
  (format "/?[%s]+\\(/[%s]+\\)*/?"
	  bm-at-point-filename-chars bm-at-point-filename-chars))

(defun bm-at-point-filename (&optional min max)
;; Warning for caller
;; * We must almost always check to see if it is also a URI.
;; * Relative URIs can't be distinguished from filenames.
  (save-excursion
    (save-match-data
      (let ((orig (point)))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(if (re-search-backward
	     bm-at-point-no-filename-char-re min "GOTO-BOUND-ON-ERROR")
	    (forward-char))
	;; we're looking at first filename constituent char.

	(set-match-data nil)
	(while (and (re-search-forward bm-at-point-filename-re max t)
		    (<= (match-beginning 0) orig)
		    (<  (match-end 0) orig))
	  (goto-char (match-end 0)))

	(when (and (match-data)
		   (<= (match-beginning 0) orig) (>= (match-end 0) orig))
	  (list (match-beginning 0) (match-end 0)))))))

(defun bm-at-point-existing-filename (&optional min max)
  (let ((range (bm-at-point-filename min max)))
    (when (and range
	       (file-exists-p (buffer-substring (car range) (cadr range))))
      range)))

(defun bm-at-point-existing-elisp-filename (&optional min max)
  (save-match-data
    (let* ((range (bm-at-point-filename min max))
	   (file  (if range (buffer-substring (car range) (cadr range)) ""))
	   stem)
      (when (string-match "\\.elc?$" file)
	(cond
	 ((file-exists-p file) range)
	 ((and (locate-library (file-name-nondirectory file) "NOSUFFIX")
	       (<= (- (cadr range) (length (file-name-nondirectory file)))
		   (point)))
	  (list (- (cadr range) (length (file-name-nondirectory file)))
		(cadr range)))
	 (t nil))))))

(defun bm-at-point-existing-c-filename (&optional min max)
  (save-match-data
    (let* ((range (bm-at-point-filename min max))
	   (file  (if range (buffer-substring (car range) (cadr range)) ""))
	   stem)
      (when (string-match "\\.[ch]$" file)
	(require 'bm-c-util)
	(cond
	 ((file-exists-p file) range)
	 ((and (bm-locate-c-library file)
	       (<= (- (cadr range) (length (file-name-nondirectory file)))
		   (point)))
	  (list (- (cadr range) (length (file-name-nondirectory file)))
		(cadr range)))
	 (t nil))))))

;;--------------------------------------------------------------
;; URL
;;--------------------------------------------------------------
;; e.g. ftp://ftp.is.co.za/rfc/rfc1808.txt
;;      gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles
;;      http://www.math.uio.no/faq/compression-faq/part1.html
;;      mailto:mduerst@ifi.unizh.ch
;;      news:comp.infosystems.www.servers.unix
;;      telnet://melvyl.ucop.edu/

(defun bm-at-point-abs-uri-ref (&optional min max)
  (require 'bm-uri)
  (save-excursion
    (save-match-data
      (let* ((orig (point)))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(if (re-search-backward bm-uri-no-uric-re min "GOTO-BOUND-ON-ERROR")
	    (forward-char))
	;; we're looking at first URI constituent char.

	(set-match-data nil)
	(while (and (re-search-forward bm-uri-abs-URI-ref-re max t)
		    (<= (match-beginning 0) orig)
		    (<  (match-end 0) orig))
	  (goto-char (1+ (match-beginning 0))))

	(when (and (match-data)
	           (<= (match-beginning 0) orig) (>= (match-end 0) orig))
	  (list (match-beginning 0) (match-end 0)))))))


;;--------------------------------------------------------------
;; man
;;--------------------------------------------------------------
;; e.g.  dir(1), [(1)

;; 1999/10/12 arg constituent chars for man, obtained by whatis -w \*
;;  +-.0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ[_abcdefghijklmnopqrstuvwxyz
(defvar bm-at-point-man-chars
  "0-9A-Za-z---+.:_"
  "command name constituent characters for man(1).
Omit [ intentionally to prevent [ls(1)] to be recognized as [ls(1)")

(defvar bm-at-point-man-re
  (format "[[%s]+ ?([0-9][0-9a-zA-Z_-]*)" bm-at-point-man-chars)
  "Regexp for man spec.
e.g. man(1), ls (1), unlink(2), ...")


(defun bm-at-point-man (&optional min max)
  "Return a list of start and end position of man spec at point, if any.
Nil otherwise."
  (save-excursion
    (save-match-data
      (let ((orig (point))
	    end)
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
        ;; first, go until ')', then go backward.
	(when (and (or (equal (char-before) ?\)) (search-forward ")" max t))
		   (and (re-search-backward bm-at-point-man-re min t)
			(setq end (match-end 0))
			(>= end orig)
			(skip-chars-backward bm-at-point-man-chars)
			(<= (point) orig)))
	  (list (point) end))))))
		   

;;--------------------------------------------------------------
;; Info
;;--------------------------------------------------------------
;; e.g. (elisp)Excursions 

(defvar bm-at-point-info-re
  "([0-9a-zA-Z.+-]+)[0-9a-zA-Z.+-][ 0-9a-zA-Z.+-_]*[0-9a-zA-Z.+-]"
  "Regexp for info spec. e.g. \"(libc)Top\".")

(defun bm-at-point-info (&optional min max)
  "Return a list of start and end position of RFC spec at point, if any.
Nil otherwise."
  (save-excursion
    (save-match-data
      (let* ((orig (point)))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(when (and (or (equal (char-after) ?\() (search-backward "(" min t))
		   (looking-at bm-at-point-info-re)
		   (>= (match-end 0) orig))
	  (list (point) (match-end 0)))))))


;;--------------------------------------------------------------
;; RFC
;;--------------------------------------------------------------
;; e.g. "RFC-1468"

(defun bm-at-point-rfc (&optional min max)
  "Return a list of start and end position of RFC spec at point, if any.
Nil otherwise."
  (save-excursion
    (save-match-data
      (let* ((orig (point))
	     (case-fold-search t))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(when (and (or (looking-at "R") (search-backward "R" min t))
		   (looking-at "RFC-?\\([0-9]+\\)\\(\\.TXT\\|\\.PS\\)?")
		   (>= (match-end 0) orig))
	  (list (point) (match-end 0)))))))


;;--------------------------------------------------------------
;; Emacs Bookmark (bookmark.el)
;;--------------------------------------------------------------
(defun bm-at-point-bookmark (&optional min max)
  (require 'bookmark)
  (save-excursion
    (save-match-data
      (let* ((orig (point)))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(when (and (or (equal (char-after) ?\{) (search-backward "{" min t))
		   (looking-at "{\\([^}]+\\)}")
		   (>= (match-end 0) orig)
		   (bookmark-get-bookmark (match-string 1)))
	  (list (match-beginning 1) (match-end 1)))))))


;;--------------------------------------------------------------
;; Symbol (using major mode syntax table)
;;--------------------------------------------------------------
(defun bm-at-point-symbol (&optional min max)
  (save-excursion
    (save-match-data
      (let* ((orig (point))
	     (orig-syntax-table (syntax-table))
	     start end)

	(cond
	 ((and (eq major-mode 'Info-mode)
	       (require 'bm-info)
	       (string-match "lisp" (bm-info-current-file)))
	  (set-syntax-table emacs-lisp-mode-syntax-table))
	 (t))
	(unwind-protect
	    (progn
	      (or min (setq min (progn (beginning-of-line) (point))))
	      (or max (setq max (progn (end-of-line) (point))))
	      (goto-char orig)
	      (skip-syntax-backward "_w" min)
	      (setq start (point))
	      (when (and (skip-syntax-forward "_w" max)
			 (setq end (point))
			 (not (equal start end)))
		(list start end)))
	  (set-syntax-table orig-syntax-table))))))

(defvar bm-at-point-designated-mode-symbol
  '(emacs-lisp-mode lisp-interaction-mode c-mode perl-mode Info-mode))

(defun bm-at-point-designated-mode-symbol (&optional min max)
  (if (member major-mode bm-at-point-designated-mode-symbol)
      (bm-at-point-symbol min max)))


;;--------------------------------------------------------------
;; Integer
;;--------------------------------------------------------------
(defvar bm-at-point-encoded-int-re
  (concat "[\\\\0][xX]\\([0-9A-Fa-f]+\\)\\|"      ;; hex  e.g. 0x0A, \x0A
	  "[\\\\]?\\^\\([][\\\\@a-zA-Z^_?]\\)\\|" ;; control  e.g. ^J, \^J
	  "[\\\\0]\\([0-7]+\\)"))                 ;; Oct  e.g. 012, \012

(defun bm-at-point-encoded-int (&optional min max)
  (save-excursion
    (save-match-data
      (let ((orig (point)))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(if (re-search-backward "[^][\\\\@a-zA-Z0-9^_?]"
				min "GOTO-BOUND-ON-ERROR")
	    (forward-char))
	;; we're looking at first int constituent char.

	(set-match-data nil)
	(while (and (re-search-forward bm-at-point-encoded-int-re max t)
		    (<= (match-beginning 0) orig)
		    (<  (match-end 0) orig))
	  (goto-char (match-end 0)))

	(when (and (match-data)
		   (<= (match-beginning 0) orig) (>= (match-end 0) orig))
	  (list (match-beginning 0) (match-end 0)))))))


;;--------------------------------------------------------------
;; Section in C reference manual.
;;--------------------------------------------------------------
(defun bm-at-point-section (&optional min max)
  (require 'bookmark)
  (save-excursion
    (save-match-data
      (let* ((orig (point)))
	(or min (setq min (progn (beginning-of-line) (point))))
	(or max (setq max (progn (end-of-line) (point))))
	(goto-char orig)
	(when (and (or (equal (char-after) ?A) (search-backward "A" min t))
		   (looking-at "A[0-9]+\\(\\.[0-9]+\\)*")
		   (>= (match-end 0) orig))
	  (list (match-beginning 0) (match-end 0)))))))


(provide 'bm-at-point)

;;; bm-at-point.el ends here
