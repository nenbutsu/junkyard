;;; cltest.el --- eval forms in the buffer and stop at a form returning NIL.

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.2 $
;; Keywords: local

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

(require 'slime)

(defun cltest ()
  "Eval successive forms in the current buffer and stop at a form returning NIL."
  (interactive)
  (cltest-forward-sexp)
  (while (not (eobp))
    (let ((result (cltest-eval (slime-last-expression))))
      (if (string= (upcase result) "NIL")
          (error "error")
          (message "%s" result)))
    (cltest-forward-sexp)
    (sit-for 0))
  (beep)
  (message "Everything's ok!"))

(defun cltest-eval (form-string)
  (slime-eval `(swank:interactive-eval ,form-string)))

(defun cltest-forward-sexp ()
  (while (and (not (progn (forward-sexp) (eobp)))
              (progn (backward-sexp) (looking-at "#[+-]")))
    (let* ((complement (equal (char-after (1+ (point))) ?\-))
           (begin (point))
           (feature-expr (progn (forward-sexp)
                                (buffer-substring (+ begin 2) (point))))
           (feature-test-str (concat "#+" feature-expr (if complement
                                                           " nil " " t ")
                                     "#-" feature-expr (if complement
                                                           " t " " nil "))))
      (when (with-temp-buffer
              (lisp-mode)
              (insert feature-test-str)
              (string= (upcase (cltest-eval (buffer-substring (point-min)
                                                              (point-max))))
                       "NIL"))
        (forward-sexp))))
  (forward-sexp))

(provide 'cltest)

;;; cltest.el ends here
