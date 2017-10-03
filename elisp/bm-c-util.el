;;; bm-c-util.el --- C language utilities

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
(require 'bm-elisp-util)

(defun bm-c-ver-specific-include-dir ()
  (let* ((result)
	 (cmd "gcc --version")
	 (ver (car (split-string (bm-shell-command-to-string cmd 'result)))))
    (cond 
     ((> result 0)
      (message "bm-c-include-dir-list: Failed %s." cmd)
      (beep) (sleep-for 1) nil)

     ((and ver (> (length ver) 0))
      (format "/usr/lib/gcc-lib/i386-linux/%s/include" ver))

     (t 
      (message "bm-c-include-dir-list: %s returned invalid version string."
	       cmd)
      (beep) (sleep-for 1) nil))))

(defvar bm-c-include-dir-list
  (delq nil (list (bm-c-ver-specific-include-dir) "/usr/include")))

(defun bm-locate-c-library (filename)
  (let* ((exit-status)
	 (cmd (format "find %s -name '%s' -follow"
		      (mapconcat 'identity bm-c-include-dir-list " ")
		      filename))
	 (abs-filenames (bm-shell-command-to-string cmd 'exit-status)))
    (when (zerop exit-status)
      (sort (delete "" (split-string abs-filenames "\n"))
	    (lambda (a b) (< (length a) (length b)))))))
    
			 









(provide 'bm-c-util)

;;; bm-c-util.el ends here
