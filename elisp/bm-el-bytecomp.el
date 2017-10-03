;;; bm-el-bytecomp.el --- routines handling Emacs Lisp byte-compilation.

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
(require 'bm-el-lib)

(defun bm-el-bytecomp-last-error ()
  ;; try to extract fatal error message from "*Compile-Log*" buffer
  (condition-case err
      (with-current-buffer (get-buffer "*Compile-Log*")
	(save-excursion
	  (goto-char (point-max))
	  (save-match-data
	    (search-backward "\014")
	    (re-search-forward "!! *\\(.+\\)")
	    (match-string 1))))
    (error nil)))


(defun bm-el-bytecomp-file (src-file)
  "Byte-compile Emacs Lisp SRC-FILE. Signal an error if compilation fails."
  (unless (byte-compile-file src-file)
    ;; fatal error occured.
    (let ((error-message
	   (format "Byte-compiling %s: %s" src-file
	   ;; try to extract fatal error message from "*Compile-Log*" buffer
		   (condition-case err
		       (with-current-buffer (get-buffer "*Compile-Log*")
			 (save-excursion
			   (save-match-data
			     (search-backward "\014")
			     (re-search-forward "!! *\\(.+\\)")
			     (match-string 1))))
		      ;; just a condition-case handler, not a function call.
		     (error "fatal error")))))
      ;; signal error.
      (error "%s" error-message)))
  t)

(defun bm-el-bytecomp-files (src-files)
  "Byte-compile SRC-FILES.
Compilation errors are not signaled but returned as a alist of file names and
 error messages."
  (bm-el-bytecomp-and-rename-files (mapcar 'list src-files)))

(defun bm-el-bytecomp-and-rename-file (src-file &optional bin-file)
  "Byte-compile SRC-FILE and rename the compiled file to BIN-FILE.
Signal an error if the operation fails."
  (unless bin-file (setq bin-file (bm-el-lib-bin-name src-file)))
  (if (string= (file-truename src-file) (file-truename bin-file))
      (error "Byte-compile %s: src and bin names are the same." src-file))
  (let* ((bin-dir (file-name-directory bin-file))
	 (default-bin-file (bm-el-lib-bin-name src-file)))
    (unless (file-exists-p bin-dir)
      (make-directory bin-dir))
    (bm-el-bytecomp-file src-file)
    (unless (string= default-bin-file bin-file)
      (rename-file default-bin-file bin-file "OVERWRITE")
      (message "Wrote %s" bin-file))
    t))

(defun bm-el-bytecomp-and-rename-files (src-bin-alist)
  "Byte-compile and rename the compiled file according to the SRC-BIN-ALIST.
Compilation errors are not signaled but returned as an alist of source file
 names and error messages.
If cdr of a pair in SRC-BIN-ALIST is `nil', default compiled file name is
 used for that pair."
  (let (result src bin)
    (while src-bin-alist
      (setq src (caar src-bin-alist)
	    bin (or (cdar src-bin-alist) (bm-el-lib-bin-name src)))
      (condition-case err
	  (bm-el-bytecomp-and-rename-file src bin)
	(error
	 (setq result (cons (cons src (error-message-string err)) result))))
      (setq src-bin-alist (cdr src-bin-alist)))
    result))


(provide 'bm-el-bytecomp)

;;; bm-el-bytecomp.el ends here
