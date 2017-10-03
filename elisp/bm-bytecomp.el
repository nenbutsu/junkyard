;;; bm-bytecomp.el --- bytecomp.el enhancement and Emacsen compatibility layer

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

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
;; bytecomp.el enhancement and Emacsen compatibility layer.
;; This library was tested with FSF Emacs 20.6.2 and XEmacs 21.1.8.


;;; Code:
(require 'bytecomp)


;;;###autoload
(defun bm-bytecomp-recompile-directory (src-dir &optional
						bin-dir arg force reload)
  "Recompile newer `.el' files in SRC-DIR than `.elc' files in BIN-DIR.
Recompilation is performed recursively on subdirectories of SRC-DIR and
corresponding subdirectories of BIN-DIR.

This command adds some functionalities to the standard
 `byte-recompile-directory' which both FSF Emacs and XEmacs define
in bytecomp.el.

ARG and FORCE has the same meaning as `byte-recompile-directory'.

  case #1: src (.el) has bin (.elc)
  ______________________________________________
  |           |             action             |
  +---+-------+--------------------------------+
  | F |non-nil|             compile            |
  | O |       |                                |
  | R |-------+--------------------------------+
  | C |       |                                |
  | E |  nil  |compile if src is newer than bin|
  +---+-------+--------------------------------+

  
  case #2: src (.el) doesn't have bin (.elc)
  ______________________________________________
  |           |              action            |
  +---+-------+--------------------------------+
  |   |  nil  |           don't compile        |
  | A |-------+--------------------------------|
  | R |  0    |              compile           |
  | G |-------+--------------------------------|
  |   | other |                ask             |
  +---+----------------------------------------+

If the fifth argument RELOAD is non-nil, reload every newly compiled `.elc'
file which is already loaded (determined using `featurep')."
  (interactive "DByte recompile source directory: \nDByte recompile binary directory: \nP")
  (if arg (setq arg (prefix-numeric-value arg)))
  (let* ((src-root-dir (expand-file-name src-dir))
	 (dest-root-dir (if bin-dir (expand-file-name bin-dir) src-root-dir))
 	 (orig-byte-compile-dest-file	; preserve original func
	  (symbol-function 'byte-compile-dest-file))
	 (orig-byte-compile-file	; preserve original func
	  (symbol-function 'byte-compile-file)))
    (fset 'byte-compile-dest-file	; flet
	  (lambda (src-file)
	    (let ((bin-file (expand-file-name
			     (file-relative-name
			      (funcall orig-byte-compile-dest-file src-file)
			      src-root-dir)
			     dest-root-dir)))
	      (if (and (not (file-directory-p (file-name-directory bin-file)))
		       (or (and (numberp arg) (= arg 0))
			   (y-or-n-p (format
				      "Directory %s doesn't exist.  create? "
				      (file-name-directory bin-file)))))
		  (make-directory (file-name-directory bin-file)))
	      bin-file)))
    (unwind-protect
	(progn
	  (fset 'byte-compile-file	; flet
		(lambda (filename &optional load)
                  (let ((feature-name (file-name-sans-extension
                                       (file-name-nondirectory filename))))
                    (when (funcall orig-byte-compile-file filename load)
                      (when (and reload
                                 (intern-soft feature-name)
                                 (featurep (intern feature-name)))
                        (load (byte-compile-dest-file filename)
                              nil       ; signal error
                              nil       ; emit message
                              t)        ; don't add suffix
                        (byte-compile-log-1 "++ reloaded"))
                      t))))
	  (unwind-protect
	      (if (featurep 'xemacs)	; call the standard function.
		  (byte-recompile-directory src-dir arg nil force)
		(byte-recompile-directory src-dir arg force))
	    (fset 'byte-compile-file orig-byte-compile-file))
	  (fset 'byte-compile-dest-file orig-byte-compile-dest-file)))))
    



(provide 'bm-bytecomp)

;;; bm-bytecomp.el ends here
