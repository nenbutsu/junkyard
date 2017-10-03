;;; bm-local-lib.el --- routines implementing local library policy

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

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Location of local libraries
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; /usr/local/share/elisp/emacsen   Libraries for all emacsen.
;; /usr/local/share/elisp<VARIANT>  Libraries for a specific emacs variant.
;;                                     e.g. /usr/local/share/fsfemacs
;;                                          /usr/local/share/xemacs
;; /usr/local/share/elisp/<FLAVOR>  Libraries for a specific emacs flavor.
;;                                     e.g. /usr/local/share/fsfemacs20
;;                                          /usr/local/share/xemacs21
;;


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Location of source files(.el) and compiled files(.elc)
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Assumption: A specific flavor of emacsen (e.g. FSF Emacs 20.4 and 
;;             FSF Emacs 20.5) can share byte-code.
;;
;; <DIR>/script.el                Emacs lisp source file.
;; <DIR>/.<FLAVOR>/script.elc     Compiled emacs lisp file.
;;                                e.g.  <DIR>/.fsfemacs20/script.elc
;;                                      <DIR>/.xemacs21/script.elc


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Where to install a new elisp program.
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   1: Distribution's Package
;;      If there's a distribution's native package (deb, rpm, ...) for 
;;      the program, install the package.
;;
;;   2: Emacsen
;;      If the program runs under all emacsen,
;;      install under /usr/local/share/emacsen/<PROGRAM-DIR>
;;
;;   3: Variant
;;      If the program runs under all versions of a specific Emacs variant,
;;      install under /usr/local/share/<VARIANT>/<PROGRAM-DIR>
;;        e.g.  /usr/local/share/emacs/fsfemacs/lookup
;;
;;   4: Flavor
;;      If the program runs under a certain major version of 
;;      a specific Emacs variant,
;;      install under /usr/local/share/<FLAVOR>/<PROGRAM-DIR>
;;        e.g.  /usr/local/share/emacs/fsfemacs20/lookup
;;
;;   5: Version
;;      
;;
;;


;;; Code:
(require 'bm-emacs)      ;; Emacs variant name and flavor name
(require 'bm-el-lib)     ;; General library handling routines
(require 'bm-el-lib-dir) ;; library directory routines

;;=================[ Define library hierarchy ]=======================
(defconst bm-local-lib-top-dir
  "/usr/local/share/elisp/")

(defconst bm-local-lib-auto-maintenance-dirs
  (let ((flavor  (symbol-name bm-emacs-flavor))
	(variant (symbol-name bm-emacs-variant)))
    (list (concat bm-local-lib-top-dir  flavor   "/site-lisp")
	  (concat bm-local-lib-top-dir  variant  "/site-lisp")
	  (concat bm-local-lib-top-dir "emacsen" "/site-lisp"))))

(defconst bm-local-lib-other-dirs
  (let ((flavor  (symbol-name bm-emacs-flavor))
	(variant (symbol-name bm-emacs-variant)))
    (list (concat bm-local-lib-top-dir  flavor   "/contrib")
	  (concat bm-local-lib-top-dir  variant  "/contrib")
	  (concat bm-local-lib-top-dir "emacsen" "/contrib"))))

(defconst bm-local-lib-bin-dir-name
  (concat "." (symbol-name bm-emacs-flavor))
  "Directory name for byte-compiled ELisp files.
This must start with chars other than [0-9A-Za-z].")

(defconst bm-local-lib-dir-loaddefs-alist
  (mapcar (lambda (dir) (cons dir (expand-file-name "loaddefs.el" dir)))
	  bm-local-lib-auto-maintenance-dirs))

;;--------------------------------------------------------------------
;; Utilities
;;--------------------------------------------------------------------
(defun bm-local-lib-bin (src-file)
  ""
  (expand-file-name
   (bm-lib-default-bin-name (file-name-nondirectory src-file))
   (expand-file-name bm-local-lib-bin-dir (file-name-directory src-file))))

;;--------------------------------------------------------------------
;; Build list of local library directories
;;--------------------------------------------------------------------
(defvar bm-local-lib-dirs nil)
(defvar bm-local-lib-src-files nil)
(defvar bm-local-lib-need-compile nil)
(defvar bm-local-lib-need-autoload nil)
(defvar bm-local-lib-need-delete nil)

(defun bm-local-lib-collect-info ()
  "Collect library information and set them to global variables."
  (let ((info (bm-lib-collect-names bm-local-lib-top-dirs)))
    (setq bm-local-lib-dirs         (nth 0 info)
	  bm-local-lib-src-files    (nth 1 info)
	  bm-local-lib-need-compile nil          ;; compute later
	  bm-local-lib-autoload     nil	         ;; compute later
	  bm-local-lib-need-delete  (nth 2 info) ;; stale .elc files
    ))
  (let ((src-list bm-local-lib-src-files)
	(compile-dirs-re (bm-local-lib-dirs-re
			  bm-local-lib-byte-compile-top-dirs))
	(autoload-dirs-re (bm-local-lib-dirs-re
			   (mapcar 'car bm-local-lib-autoload-top-dirs-alist)))
	src need-autoload)
    (while (setq src (car src-list))
      (when (and (string-match compile-dirs-re src)
		 (file-newer-than-file-p src (bm-local-lib-bin src)))
	(setq bm-local-lib-need-compile
	      (cons src bm-local-lib-need-compile)))
      (setq src-list (cdr src-list))))
)

(defun bm-el-lib-collect-full-names (dir-list)
  "Return a list of subdir, source and byte-compiled files under DIR-LIST.
All names are absolute file names.
All names are in depth first and alphabetically revese order.
Can be considered as a recursive version of `bm-el-lib-directory-files'.
e.g. (setq load-path
           (nconc load-path
                  (nreverse (car (bm-el-lib-collect-full-names local-libs)))))"
  (let (subdirs src-files bin-files info)
    (while dir-list
      (setq dir       (expand-file-name (car dir-list))
	    info      (bm-el-lib-directory-files dir "FULL")
	    subdirs   (cons dir subdirs)
	    src-files (nconc (nth 1 info) src-files)
	    bin-files (nconc (nth 2 info) bin-files)
            dir-list  (nconc (nreverse (car info))
			     (cdr dir-list))))
    (list subdirs src-files bin-files)))

(defun bm-el-lib-collect-names (dir-list)
  "Return a list ((alldirs) (dir->src alist) (dir->bin alist)) for DIR-LIST.
All names in associative lists are without directory.
All names are in depth first and alphabetically revese order.
Can be considered as a recursive version of `bm-el-lib-directory-files'."
  (let (subdirs src-files bin-files info)
    (while dir-list
      (setq dir       (expand-file-name (car dir-list))
	    info      (bm-el-lib-directory-files dir)
	    subdirs   (cons dir subdirs)
	    src-files (cons (cons dir (nth 1 info)) src-files)
	    bin-files (cons (cons dir (nth 2 info)) bin-files)
            dir-list  (nconc (nreverse
			      (mapcar
			       (lambda (subdir) (expand-file-name subdir dir))
			       (car info)))
			     (cdr dir-list))))
    (list subdirs src-files bin-files)))

(defun bm-el-lib-dir-info (dir &optional want-full-name loaddefs)
  (let* ((abs-dir           (expand-file-name dir))
	 (result            (bm-el-lib-dir-files abs-dir))
	 (default-directory (file-name-as-directory abs-dir))
	 (src-files         (cdr (assoc 'src result)))
	 (no-src-bin        (copy-sequence (cdr (assoc 'bin result))))
	 need-compile need-autoload src bin)

    (while (setq src (car src-files))
      (setq bin (bm-el-lib-bin-name src))
      (if (file-newer-than-file-p src bin)
	  (setq need-compile (cons src need-compile)))
      (setq no-src-bin (delete bin no-src-bin))
      (if (and loaddefs (file-newer-than-file-p src loaddefs))
	  (setq need-autoload (cons src need-autoload)))
      (setq src-files (cdr src-files)))

    (nconc result (list (cons 'need-compile need-compile)
			(cons 'no-src-bin no-src-bin)
			(cons 'need-autoload need-autoload)))
    (if want-full-name (bm-el-lib-dir-expand-file-name abs-dir result)
      result)))

;;--------------------------------------------------------------------
;; Byte-compile local libraries
;;--------------------------------------------------------------------
(defun bm-local-lib-byte-compile-dir (directory &optional recursive)
  "Byte-compile Emacs Lisp source files (.el) under local library DIRECTORY.
If RECURSIVE is non-nil, compile the whole directory tree.
Compiled files are placed under a directory according to the local policy.
Return value is ...
a list whose car is a list of file names successfully compiled and installed,
and cadr (second element) is a list of error info like 
('FILENAME' 'ERROR MESSAGE').
 e.g. An example of return value.
      (;; list of successfully compiled files
       ('~/bm-lib.el' '~/bm-xlib.el')
       
       ;; list of error infos
       (
         ;; error info's format is (FILENAME ERRORMSG)
        ('~/bm-local-lib.el'          
         'Renaming: permission denied, ~/bm-local-lib.elc, ~/bm-local-lib.elc')
       
        ('~/bm-emacs.el'
         'Renaming: permission denied, ~/bm-emacs.elc, ~/bm-emacs.elc')
       )
      )
"
  (setq directory (expand-file-name directory))
  (let ((file-list (directory-files directory "FULL-NAME" nil "NOSORT"))
	(ignore-re (concat "^\\("
			   (mapconcat 'identity 
				      bm-local-lib-ignore-dir-re-list "\\|")
			   "\\)$"))
	file compiled-list failed-list)
    (while (setq file (car file-list))
      (cond
       ((not (string= (expand-file-name file) file))) ;; skip '.', '..'
       ((string-match ignore-re file))                ;; skip ignore-re dir
       ((and recursive (file-directory-p file))       ;; add sub dirs
	(nconc file-list 
	       (directory-files file "FULL-NAME" nil "NOSORT")))
       ((and (string-match "\.el$" (file-name-nondirectory file))
	     (or (file-regular-p file) (file-symlink-p file)))
	(condition-case err                           ;; check source file
	    (if (bm-local-lib-byte-compile-file file)
		(setq compiled-list (cons file compiled-list)))
	  (error 
	   (message "%s" (error-message-string err))
	   (setq failed-list
		 (cons (list file (error-message-string err)) failed-list))))))
      (setq file-list (cdr file-list)))
    (list compiled-list failed-list)))


(defun bm-local-lib-auto-byte-compile ()
  "Byte-compile files under `bm-local-lib-auto-byte-compile-dir' if necessary.
When you modify some local libraries, just run this command and the sources
will be compiled and the libraries will be re-loaded into the current Emacs 
session if necessary."
  (interactive)
  (let* ((result (bm-local-lib-byte-compile-dir ;; byte-compile
		  bm-local-lib-auto-byte-compile-dir "RECURSIVE"))
	 (compiled (car  result))
	 (failed   (cadr result))
	msg-buf)
    (unless (and (null compiled) (null failed))
      (setq msg-buf (get-buffer-create "*Byte compilation messages*"))
      (set-buffer msg-buf)
      (when compiled
	(bm-load-path-init 2)   ;; rebuild load-path
	(insert (current-time-string) "\n")
	(insert "These files were successfully compiled and installed.\n")
	(mapcar '(lambda (file) ;; reload libraries if necessary
		   (insert file)
		   (let ((feature-name (file-name-sans-extension 
					(file-name-nondirectory file))))
		     (when (featurep (read feature-name))
		       (setq features (delete (read feature-name) features))
		       (insert " (reloaded)")
		       (require (read feature-name)))
		     (insert "\n")))
		compiled))
      (when failed
	(and compiled (insert "\n"))
	(insert "Error occurred while compiling/installing these files.\n")
	(mapcar '(lambda (info) (insert (car info) "\n")) failed)
	(insert "\n")
	(insert "Error messages are as follows.\n")
	(mapcar '(lambda (info) (insert (cadr info) "\n")) failed))
      (insert "\n\n")
      (switch-to-buffer-other-window msg-buf))))

(provide 'bm-local-lib)

;;; bm-local-lib.el ends here
