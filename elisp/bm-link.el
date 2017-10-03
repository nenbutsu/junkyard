;;; bm-link.el --- Custom link utility ruoutines.
;;  Copyright (C) bmonkey <ggb01164@nifty.ne.jp>
;;  $Id: bm-link.el,v 1.1 2004-06-05 08:05:19 yuji Exp $
;;
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; Custom link commands
;;
;; Bogus BNF Grammar for custom link
;;
;; link      := "[|" target  macro  "|]"
;;	       
;; target    := current | RFC | info | file | bookmark | URI
;;	       
;; current   := "*"                     ; e.g. [|* #10|]
;; RFC	     := "rfc" [0-9]+		; e.g. [|rfc1468|]
;; info      := """ "(" info ")" node"""; e.g. [|"(emacs)top"|]
;; file      := "../" path | "/" path	; e.g. [|/usr/src/linux/README|]
;; bookmark  := "{" name "}"		; e.g. [|{acro.txt}|]
;; URI       := ( "http" | "ftp" | "file" ) REST
;;                                      ; e.g. [|http://localhost/|]
;; man       := command "(" section ")" ; e.g. [|printf(3)|]
;;	       
;; macro     := (search | re-search | jump | screenpos)+
;;					; e.g. [|{abbr.txt} #1 "^SMTP" T |]
;; screenpos := "T" | "B"		;      T means top, B means bottom
;;					;      the designated line is displayed
;;					;      at the top (bottom) of the
;;					;      screen.
;;
;; search    := "'" string-to-search "'"  ; e.g. [|rfc1468 'iso-2022-jp'|]
;; re-search := "\"" regexp-to-match "\"" ;
;; jump      := "#" number ( "T" | "B" )  ; e.g. [|{book.txt} #10 T|]
;;
;;
;;

;;; Code:

(defconst bm-re-link "\\[|[ \t]*[\"{]?\\(\\([^|]+\\||[^]]\\)+\\)|\\]"
  "Regexp for custom link.")

;;;###autoload
(defun bm-goto-link (backward)
  "Move point forward to next custom link.
Move backward if BACKWARD is non-nil."
  (interactive "P")
  ;; step over when we're already at a link
  (let ((search-f (if backward 're-search-backward 're-search-forward)))
    (if (null (funcall search-f bm-re-link nil t))
	(error (error "No link found."))
      (goto-char (match-beginning 1)))))

;;;###autoload
(defun bm-follow-link ()
  "Follow custom link."
  (interactive)
  (require 'bm-do-smthg)
  (let* ((orig  (current-buffer))
	 (result-list (bm-do-smthg))
	 (thing (car result-list))
	 (result (cadr result-list))
	 macro)
    (when thing
      (save-excursion
	(set-buffer orig)
	(goto-char (cadr (get thing 'range)))
	(if (looking-at "[\"}]?[ \t]*\\([^]}|]*\\)|\\]")
	    (setq macro (match-string 1))))
      (if (and macro (> (length macro) 0)) (bm-run-macro macro)))))

;;;###autoload
(defun bm-follow-link-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (bm-follow-link))

(defun bm-run-macro (actions)
  "Run custom macro.
Run custom macro command string ACTIONS.
ACTIONS is a string like \"#20 'keyword'\" which means
`jump to the line 20 and search forward \"keyword\""
  (interactive "sRun macro: ")
  (let* ((rest "")
	 (cmd ""))
    (cond
     ((string= actions ""))

     ;; screen position command
     ((string-match "^\\([tT]\\|[bB]\\)[ \t]*\\(.*\\)$" actions)
      (setq rest (match-string 2 actions))
      (setq cmd	 (match-string 1 actions))
      (if (string= (upcase cmd) "T") (recenter 0))
      (if (string= (upcase cmd) "B") (recenter (- (window-height) 2))))

     ;; search string	 e.g. 'search me'
     ((string-match "^'\\([^']+\\)'[ \t]*\\(.*\\)$" actions)
      (setq rest (match-string 2 actions))
      (search-forward (match-string 1 actions)))

     ;; re search string    e.g. "\(me\|him\|her\)"
     ((string-match "^\"\\([^\"]+\\)\"[ \t]*\\(.*\\)$" actions)
      (setq rest (match-string 2 actions))
      (re-search-forward (match-string 1 actions)))

     ;; jump to line	 e.g. #10
     ((string-match "^#\\([0-9]+\\)[ \t]*\\(.*\\)$" actions)
      (setq rest (match-string 2 actions))
      (goto-line (string-to-number (match-string 1 actions))))

     (t (error "Invalid custom macro")))

    ;; Recur
    (if (not (string= rest "")) (bm-run-macro rest))))



(provide 'bm-link)

;;; bm-link.el ends here
