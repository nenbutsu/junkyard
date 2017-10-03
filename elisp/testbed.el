;;; testbed.el --- provide a testbed for inferior lisp using ilisp facilities

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

(require 'ilisp)

(defun testbed-test ()
  "Evaluate the current form."
  (interactive)
  (save-excursion
    (let* ((end (lisp-defun-end))
           (start (lisp-defun-begin)))
      (lisp-send-region start end 'result "Testing..." 'eval
                        "(ILISP:ilisp-eval \"(not %s)\" \"%s\" \"%s\")"))))

(defun testbed-forward-sexp ()
  (while (and (not (progn (forward-sexp) (eobp)))
              (progn (backward-sexp) (looking-at "#[+-]")))
    ;(y-or-n-p "looping!")
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
              (string= (upcase (eval-region-lisp (point-min) (point-max)
                                                 'result))
                       "NIL"))
        (forward-sexp))))
  (forward-sexp)
  ;(y-or-n-p "ok?")
  )

(defun testbed-eval-from-here ()
  (interactive)
  ;;(set-buffer-package-lisp testbed-package-name)
  (testbed-forward-sexp)
  (let (result)
    (while (not (eobp))
      (unless (string= (upcase (setq result (testbed-test))) "NIL")
        (lisp-display-output result)
        (error "error"))
      (testbed-forward-sexp))
    (beep)
    (message "Everything's ok!")))

(global-set-key [f3]  'testbed-eval-from-here)

(provide 'testbed)

;;; testbed.el ends here
