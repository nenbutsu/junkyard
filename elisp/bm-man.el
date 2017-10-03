;;; bm-man.el --- browse UNIX manual pages with completion.

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, help

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
;; browse UNIX manual pages with completion.
;; Completion of Unix manual page names using whatis(1) which accepts -w option
;; (shell wildcard search).
;;
;; This library was tested with FSF Emacs 20.6.2 and XEmacs 21.1.8.
;;

;;; Code:
(require 'man)                          ; standard (X)Emacs man page viewer
(require 'bm-man-cmplt)

;;;###autoload
(defun bm-man (man-spec)
  "Read Unix manual page name with completion and display the page.
Prefix argument specifies a section to complete."
  (interactive (list (bm-man-read)))
  (if (featurep 'xemacs) (manual-entry man-spec)
    ;; workaround (FSF Emacs's man.el fails with "[(1)")
    (string-match "\\([^(]+\\)(?\\([^)]*\\)" man-spec)
    (man (format "%s %s"
		 (match-string 2 man-spec)
		 (match-string 1 man-spec)))))


(defun bm-man-read ()
  ""
  (let* ((sec-regexp (format "(%s[^)]*)\\'"
			     (number-to-string
			      (prefix-numeric-value current-prefix-arg))))
	 (predicate  (when current-prefix-arg
		       (lambda (cell)
			 (string-match sec-regexp (car cell))))))
    (completing-read "Manual entry: " 'bm-man-cmplt predicate)))



(provide 'bm-man)

;;; bm-man.el ends here
