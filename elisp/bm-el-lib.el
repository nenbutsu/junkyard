;;; bm-el-lib.el --- common definitions for Emacs Lisp library files

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



(defconst bm-el-lib-src-name-re "\\.el\\'"
  "Regexp recognizing Emacs Lisp source file name.")

(defconst bm-el-lib-bin-name-re "\\.elc\\'"
  "Regexp recognizing Emacs Lisp byte-compiled file name.")

(defconst bm-el-lib-name-re "\\.elc?\\'"
  "Regexp recognizing Emacs Lisp source and byte-compiled file name.")

(defconst bm-el-lib-dir-name-re "\\`[a-zA-Z0-9]"
  "Regexp recognizing Emacs Lisp library directory name.")

(defmacro bm-el-lib-bin-name (src-file)
  "Return default byte-compiled file name of Elisp source file SRC-FILE.
This is the same name as the name of the file `byte-compile-file' creates.
e.g. (bm-el-lib-bin-name 'mail.el') => 'mail.elc'"
  (list
   'save-match-data
   (list
    'if (list 'string-match "\\.el\\'" src-file)
        (list 'concat (list 'substring src-file 0 '(match-beginning 0)) ".elc")
       (list 'concat src-file ".elc"))))

(provide 'bm-el-lib)

;;; bm-el-lib.el ends here
