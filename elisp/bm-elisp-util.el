;;; bm-elisp-util.el --- Emacs lisp utilities

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

(defun bm-grep-seq (pred seq &optional cons-f)
  "Return a list (by default) of elements of SEQ which PRED returns non-nil."
  (let ((i 0)
	(len (length seq))
	item
	result)
    (or cons-f (setq cons-f 'cons))
    (while (< i len)
      (if (funcall pred (setq item (elt seq i)))
	  (setq result (funcall cons-f item result)))
      (setq i (1+ i)))
    result))


(add-hook 'edebug-setup-hook
	  (lambda () (def-edebug-spec bm-with-file-visiting-buffer t)))

(defmacro bm-with-file-visiting-buffer (file &rest body)
  `(if (get-file-buffer ,file)
       (with-current-buffer (get-file-buffer ,file)
	 (save-excursion
	   (widen)
	   (goto-char (point-min))
	   ,@body))

     (let ((buffer (find-file ,file)))
       (prog1
	   (with-current-buffer buffer
	     ,@body)
	 (kill-buffer buffer)))))


(defun bm-shell-command-to-string (command &optional result-var)
  "Execute shell command COMMAND and return its output as a string.
If RESULT-VAR is non-nil, set the exit status of COMMAND to RESULT-VAR."
  (with-output-to-string
    (with-current-buffer standard-output
      (or (and result-var
	       (set result-var
		    (call-process
		     shell-file-name nil t nil shell-command-switch command)))
	  (call-process
	   shell-file-name nil t nil shell-command-switch command)))))



(provide 'bm-elisp-util)

;;; bm-elisp-util.el ends here
