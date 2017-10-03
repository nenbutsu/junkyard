;;; cl-lookup-cmucl.el --- View various documentation on Common Lisp

;; Copyright (C) 2004 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, lisp

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



;;; Change Log:



;;; Code:
(require 'cl-lookup)

(defvar cl-lookup-cmucl-root "http://common-lisp.net/project/cmucl/doc/cmu-user/")
(defvar cl-lookup-cmucl-entry-postfix nil)

(mapc #'(lambda (entry)
          (destructuring-bind (name path) entry
            (let ((symbol (intern (concat (downcase name)
                                          cl-lookup-cmucl-entry-postfix)
                                  cl-lookup-obarray)))
              (if (boundp symbol)
                  (pushnew path (symbol-value symbol) :test #'equal)
                  (set symbol `(,path))))))
      '(("cmucl" (cl-lookup-cmucl-root))
        ("ext:save-lisp" (cl-lookup-cmucl-root "extensions.html#toc48"))
        ("ext:*command-line-strings*" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:*command-line-utility-name*"
         (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:*command-line-words*" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:*command-line-switches*" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:cmd-switch-name" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:cmd-switch-value" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:cmd-switch-words" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:cmd-switch-arg" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:get-command-line-switch" (cl-lookup-cmucl-root "unix.html#toc228"))
        ("ext:defswitch" (cl-lookup-cmucl-root "unix.html#toc228"))
        ))

(provide 'cl-lookup-cmucl)

;;; cl-lookup-cmucl.el ends here
