;;; bm-el-lib-dir.el --- Get a list of Emacs Lisp library directory contents.

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
;;

;;; History:
;; 

;;; Code:
(require 'bm-el-lib)

;;---------------------------------------------------------------
(defconst bm-el-lib-dir-ignore-files
  '("." ".." "CVS" "RCS")
  "List of file names to be ignored by this library.")

(defconst bm-el-lib-dir-name-re "\\`[a-zA-Z0-9]"
  "Regexp recognizing Emacs Lisp library directory name.")
;;---------------------------------------------------------------


(defun bm-el-lib-dir-src-files (dir &optional want-full-name)
  "Return a list of the names of the Emacs Lisp source files in DIR.
If WANT-FULL-NAME is non-nil, a list of the absolute file names are returned."
  (cdr (assoc 'src (bm-el-lib-dir-files dir want-full-name))))


(defun bm-el-lib-dir-bin-files (dir &optional want-full-name)
  "Return a list of the names of the Emacs Lisp compiled files in DIR.
If WANT-FULL-NAME is non-nil, a list of the absolute file names are returned."
  (cdr (assoc 'bin (bm-el-lib-dir-files dir want-full-name))))


(defun bm-el-lib-dir-subdirs (dir &optional want-full-name)
  "Return a list of the names of the library directories in DIR.
If WANT-FULL-NAME is non-nil, a list of the absolute file names are returned."
  (cdr (assoc 'subdir (bm-el-lib-dir-files dir want-full-name))))


(defun bm-el-lib-dir-info (src-dir &optional bin-dir want-full-name loaddefs)
  "Return an associative list of the names of the files in SRC-DIR and BIN-DIR.
If BIN-DIR is nil, it is assumed to be the same as SRC-DIR.
If WANT-FULL-NAME is non-nil, absolute file names are returned.
The keys and values of the alist are as follows.
    key symbol      value
    --------------------------------------------------------------------------
    subdir          A list of the names of the library directories in SRC-DIR.
    src             A list of the names of the source files in SRC-DIR.
    bin             A list of the names of the compiled files in BIN-DIR.
    need-compile    A list of the names of source files in SRC-DIR which are
                    newer than their compiled files, or which have no compiled
                    files.
    need-autoload   When LOADDEFS is non-nil, A list of the names of source
                    files in SRC-DIR which are newer than the file LOADDEFS.
                    Otherwise, nil.
    no-src-bin      A list of the names of compiled files in BIN-DIR which
                    have no source files.
    stray-src       When BIN-DIR is different from SRC-DIR,
                    A list of the names of the source files in BIN-DIR.
                    Otherwise, nil.
    stray-bin       When BIN-DIR is different from SRC-DIR,
                    A list of the names of the compiled files in SRC-DIR.
                    Otherwise, nil.
    --------------------------------------------------------------------------"
  (setq src-dir  (expand-file-name src-dir)
	bin-dir  (if bin-dir (expand-file-name bin-dir) src-dir)
	loaddefs (if loaddefs (expand-file-name loaddefs)))
  (let* ((same (string= (file-truename src-dir) (file-truename bin-dir)))
	 (src-dir-files (bm-el-lib-dir-files src-dir))
	 (bin-dir-files (unless same (bm-el-lib-dir-files bin-dir)))
	 (default-directory (file-name-as-directory src-dir)) ; chdir SRC-DIR
	 (src-files (cdr (assoc 'src src-dir-files)))
	 (no-src-bin (copy-sequence (cdr (assoc 'bin (if same src-dir-files
						       bin-dir-files)))))
	 need-compile need-autoload src-name bin bin-name)

    (while (setq src-name (car src-files))
      (setq bin-name (bm-el-lib-bin-name src-name)
            bin      (if same bin-name (expand-file-name bin-name bin-dir)))
      (if (file-newer-than-file-p src-name bin)
	  (setq need-compile (cons src-name need-compile)))
      (setq no-src-bin (delete bin-name no-src-bin))
      (if (and loaddefs (file-newer-than-file-p src-name loaddefs))
	  (setq need-autoload (cons src-name need-autoload)))
      (setq src-files (cdr src-files)))

    (setq src-dir-files
	  (nconc src-dir-files (list (cons 'need-compile  need-compile)
				     (cons 'need-autoload need-autoload))))

    (setq bin-dir-files
	  (nconc bin-dir-files (list (cons 'no-src-bin no-src-bin))))

    (when want-full-name
	(setq src-dir-files
	      (bm-el-lib-dir-expand-file-name-alist src-dir-files src-dir))
	(setq bin-dir-files
	      (bm-el-lib-dir-expand-file-name-alist bin-dir-files bin-dir)))

    (if same (setq bin-dir-files
		   (nconc (mapcar 'list '(stray-bin stray-src)) bin-dir-files))
      (setcar (assoc 'bin    src-dir-files) 'stray-bin)
      (setcar (assoc 'src    bin-dir-files) 'stray-src)
      (setcar (assoc 'subdir bin-dir-files) 'bin-subdir))

    (nconc src-dir-files bin-dir-files)))


(defun bm-el-lib-dir-files (dir &optional want-full-name)
  "Return an associative list of the names of the files in DIR.
If WANT-FULL-NAME is non-nil, absolute file names are returned.
The keys and values of the alist are as follows.
    key symbol    value
    --------------------------------------------------------------------
    subdir        A list of the names of the library directories in DIR
    src           A list of the names of the source files in DIR
    bin           A list of the names of the compiled files in DIR
    --------------------------------------------------------------------
e.g. (bm-el-lib-dir-files \"~/.elisp\")
     => ((subdir \"init\" \"data\") (src \"lib.el\") (bin \"lib.elc\"))"
  (let* ((files (condition-case nil (directory-files dir) (error nil)))
	 (abs-dir (expand-file-name dir))
	 (default-directory (file-name-as-directory abs-dir)) ;; chdir to DIR
	 name subdirs src-files bin-files result)
    (while (setq name (car files))
      (cond
       ;; skip ".", "..", "RCS", "CVS", ...
       ((member name bm-el-lib-dir-ignore-files))
       ;; src file
       ((and (string-match bm-el-lib-src-name-re name) (file-regular-p name))
	(setq src-files (cons name src-files)))
       ;; bin file
       ((and (string-match bm-el-lib-bin-name-re name) (file-regular-p name))
	(setq bin-files (cons name bin-files)))
       ;; subdir
       ((and (string-match bm-el-lib-dir-name-re name)
	     (file-directory-p name)
	     (not (file-exists-p
		   (concat (file-name-as-directory name) ".nosearch"))))
	(setq subdirs (cons name subdirs))))
      (setq files (cdr files)))
    (setq result (list (cons 'subdir subdirs)
		       (cons 'src    src-files)
		       (cons 'bin    bin-files)))
    (if want-full-name
	(bm-el-lib-dir-expand-file-name-alist result abs-dir)
      result)))


(defun bm-el-lib-dir-expand-file-name-alist (alist dir)
  "Return an alist which is a copy of ALIST with file names expanded with DIR."
  (mapcar (function
	   (lambda (title-list)
	     (cons (car title-list)
		   (mapcar (function
			    (lambda (name)
			      (expand-file-name name dir)))
			   (cdr title-list)))))
	  alist))
  
(provide 'bm-el-lib-dir)

;;; bm-el-lib-dir.el ends here
