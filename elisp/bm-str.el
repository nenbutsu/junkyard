;;; bm-str.el -- String utilities.
;;
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;; This file is NOT part of GNU Emacs.
;;
;; $Id: bm-str.el,v 1.1 2004-06-05 08:05:19 yuji Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; 


;;; History:
;;

;;; Code:

(or (fboundp 'char-int)
    (defalias char-int 'identity))


(defun bm-int2bitstr (int)
  "Return bit sequence string corresponding to integer INT.
e.g. (bm-int2bitstr 2) => \"10\""
  (let ((bit (if (zerop int) "0" "")))
    (while (/= int 0)
      (setq bit (concat (number-to-string (logand int 1)) bit))
      (setq int (lsh int -1)))
    bit))

(defun bm-int2bitstr-fancy (int &optional width sep)
  "(bm-int2bitstr-fancy 82 4 \"_\") => \"0101_0010\""
  (or width (setq width 4))
  (or sep   (setq sep   "_"))
  (let ((bit (bm-int2bitstr int)))
    (if (and (/= width 0) (> (mod (length bit) width) 0))
	(setq bit (concat 
		   (make-string (- width (mod (length bit) width)) ?0) bit)))
    (mapconcat 'identity (bm-str-divide bit width) sep)))


(defun bm-str-divide (str len)
  "Return a list of STR's substrings with length LEN starting at the end.
e.g. (bm-str-divide \"abcde\" 2) => (\"a\" \"bc\" \"de\")"
  (cond
   ((<= (length str) len) (list str))
   (t (append (bm-str-divide (substring str 0 (- len)) len)
	      (list (substring str (- len) nil))))))

(defun bm-byte-list-to-int (byte-list)
  (let ((int 0))
    (while byte-list
      (setq int (+ (* 256 int) (car byte-list)))
      (setq byte-list (cdr byte-list)))
    int))
    

(defun bm-fuzzy-str2int (str)
  "Return integer which STR probably mean."
  (save-match-data
    (cond
     ;; Emacs char read syntax. e.g. ?\n, ?\x0A, ?\012
     ((and (string-match "^\\?.+" str) 
	   (= (cdr (read-from-string str)) (length str)))
      (char-int (read str)))

     ;; control  e.g. ^J, \^J
     ((string-match "^[\\\\]?\\^\\([]@a-zA-Z[\\\\^_?]\\)$" str)
      (logand ?\x7F
	      (- (upcase (string-to-char (match-string 1 str))) ?\x40)))

     ;; hex  e.g. 0x0A, \x0A
     ((string-match "^[\\\\0][xX]\\([0-9A-Fa-f]+\\)$" str)
      (string-to-number (match-string 1 str) 16))

     ;; Oct  e.g. 012, \012
     ((string-match "^[\\\\0]\\([0-7]+\\)$" str)
      (string-to-number (match-string 1 str) 8))

     ;; Dec  e.g. 10
     ((string-match "^[0-9]+$" str) (string-to-number str))

     ;; One char
     ((string-match "^.$" str)
      (bm-byte-list-to-int (cdr (split-char (string-to-char str)))))
      
     ;; error
     (t nil))))

;;;###autoload
(defun bm-show-int (str)
  "Show integer STR in various formats (hex, octal, ...)."
  (interactive "sInteger: ")
  (let* ((num (or (bm-fuzzy-str2int str) (error "Not integer: %s" str)))
	 (msg ""))
    (setq msg (format "%shex: 0x%02X   " msg num))
    (setq msg (format "%soct: %s%o   "   msg (if (zerop num) "" "0") num))
    (setq msg (format "%sdec: %d   "     msg num))
    (setq msg (format "%sbit: %s   "     msg (bm-int2bitstr-fancy num)))
    (if (or (< num ?\x1F) (= num ?\x7F))
	(setq msg (format "%skey: ^%c" msg (logand (+ num ?\x40) ?\x7F))))
    (message "%s" msg)))
  







(provide 'bm-str)

;;; bm-str.el ends here
