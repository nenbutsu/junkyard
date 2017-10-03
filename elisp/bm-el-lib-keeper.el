;;; bm-el-lib-keeper.el --- ELisp library maintenance (bytecomp, autoload,..)

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: lisp

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
;; Keep local Emacs Lisp libraries' consistency and keep them up to date.
;;
;; Once you set some global variables and a function to describe your local
;; library directory trees, the function `bm-el-lib-keeper-update' takes care
;; of byte-compilation, updating of autoloads files and deletion of
;; out-of-place source/compiled files.
;; When error occurs, correct the error and call `bm-el-lib-keeper-update'.
;; Repeat the above procedure until `bm-el-lib-keeper-update' says nothing.
;;
;;
;; Three different library directory types
;;
;;   1 A source-binary directory contains source files and their
;;     corresponding compiled files. The other compiled files are considered
;;     out of place and might be deleted.
;;
;;   2 A source-only directory should contain only source files, and has the
;;     corresponding binary-only directory. All compiled files in this type
;;     of directory are considered out of place and might be deleted.
;;
;;   3 A binary-only directory should contain only compiled files, and has the
;;     corresponding source-only directory. All source files in this type
;;     of directory are considered out of place and might be deleted (but
;;     the default behaviour is only to report their existence.)
;;     The mapping of a source-only directory to a binary-only directory
;;     must be unique and exclusive.
;;     For example, if src_dir_a maps to bin_dir_a, no other source
;;     directory shall map to bin_dir_a, and vice versa.
;;     A binary-only directory must either be outside of 
;;     `bm-el-lib-keeper-top-src-trees' or have names starting with 
;;     a character other than [0-9A-Za-z].
;;
;;   + The phrase "a source directory" is used to denote a source-binary or 
;;     source-only directory abstractly.
;;   + The notion of source-only and binary-only directories are needed to 
;;     handle library directories shared by several Emacsen (FSF Emacs, 
;;     XEmacs, Mule), since byte-compiled files can be incompatible among them.
;;   + The names of library directories must start with [0-9A-Za-z].
;;     (Emacs convention)
;;   + Directories containing '.nosearch' file is ignored. (Emacs convention)
;;
;;
;; Byte-compilation
;;   * If (file-newer-than-file-p source-file compiled-file), the source-file
;;     needs compilation.
;;   * For bm-el-lib-bytecomp to reload properly, all libraries must provide
;;     a feature which is their file name without the extention.
;;     e.g. my-lib.el must contain (provide 'my-lib).
;;          But lacking (provide 'my-lib) just causes it to be not reloaded.
;;     Autoloads files need not provide feature. They are forcibly reloaded
;;     whenever they are byte-compiled.
;;     This behavior should cause no trouble since `autoload' function
;;     does nothing if its argument symbol has a function definition.
;;
;; Autoloads file update
;;   * If (file-newer-than-file-p source-file loaddefs-file), the source-file
;;     needs to be checked for autoloads.
;;   * Source files must contain special comment ";;;###autoload" above
;;     the definitions of functions, macros, ... which are to be put into
;;     autoloads file. (Emacs convention)
;;   * Autoload definitions files (containing lots of autoload definitions of 
;;     other source files) can be byte-compiled.
;;   * Autoload definitions files are Emacsen specific
;;     (might be fixed in the future).
;;   * If you want to check all the source files, just delete the autoloads
;;     files.
;;   * The function `bm-el-lib-keeper-update-autoload' may delete autoloads
;;     definition files without notice. This enables very easy maintenance.
;;
;;
;; The relations of functions and global variables.
;;
;;    bm-el-lib-keeper-update <----------------------------------+
;;     |    |    |    |    |                                     |
;;    refer call call call call                       libraries' current state
;;     |    |    |    |    |                                     |
;;     |    |    |    |    +--> bm-el-lib-keeper-collect-info ---+
;;     |    |    |    +-------> bm-el-lib-keeper-bytecomp
;;     |    |    +------------> bm-el-lib-keeper-update-autoload
;;     |    +-----------------> bm-el-lib-keeper-delete
;;     |		     
;;     +----------------------> Global vars specify static library structures.
;;                              bm-el-lib-keeper-top-src-trees
;;                              bm-el-lib-keeper-shared-src-dirs-re
;;                              bm-el-lib-keeper-shared-src-bin-dir-func
;;                              bm-el-lib-keeper-bin-check-dirs-re
;;                              bm-el-lib-keeper-autoload-check-dirs-re-alist
;;
;; Regexp recognizing directories
;;   Gloval variables bm-el-lib-keeper-shared-src-dirs-re,
;;   bm-el-lib-keeper-bin-check-dirs-re and 
;;   the values of bm-el-lib-keeper-autoload-check-dirs-re-alist recognize
;;   the names of directories.
;;   E.g.    "\\`/usr/local/share/emacs"
;;               a directory tree with "/usr/local/share/emacs" as the top.
;;
;;           "\\`/usr/local/share/emacs\\'"
;;               a single directory "/usr/local/share/emacs"
;;
;;           "site-lisp"
;;               any directories which have "site-lisp" in their file name
;;               components. e.g. "/usr/local/share/emacs/site-lisp"
;;                                "/usr/share/emacs/site-lisp/package"
;;
;;
;; Compatibility
;;   I checked this library with FSF Emacs 20.5.2 and XEmacs 21.
;;   But I mainly use FSF Emacs, so you may encounter some glitches under 
;;   XEmacs (please, E-mail me if you find one).
;;
;;
;; -------------------------------------
;; Example
;; -------------------------------------
;; Sample configuration for FSF Emacs and XEmacs.
;;
;;   The symbol `bm-emacs-flavor' is defined in bm-emacs.el.
;;
;;   (setq bm-el-lib-keeper-top-src-trees
;;         (list
;;          "/usr/local/share/elisp/emacsen"
;;	    (concat "/usr/local/share/elisp/" (symbol-name bm-emacs-variant))))
;;
;;   (setq bm-el-lib-keeper-shared-src-dirs-re
;;             "\\`/usr/local/share/elisp/emacsen")
;;
;;   (setq bm-el-lib-keeper-shared-src-bin-dir-func
;;         (lambda (src-dir)
;;    	     (expand-file-name
;;    	      (concat "." (symbol-name bm-emacs-flavor)) src-dir)))
;;    
;;   (setq bm-el-lib-keeper-bin-check-dirs-re
;;      (concat "\\`\\("
;;    	   (mapconcat (lambda (dir) (regexp-quote dir))
;;    		      bm-el-lib-keeper-top-src-trees  "\\)\\|\\(")
;;    	   "\\)\\'"))
;;    
;;   (setq bm-el-lib-keeper-autoload-check-dirs-re-alist
;;         '(("/usr/local/share/elisp/fsfemacs/local-loaddefs.el" . 
;;            "\\`/usr/local/share/elisp/\\(emacsen\\|fsfemacs\\)\\'")))
;;
;;
;;   The directory tree denoted by the above configuration.
;;    /usr/local/share/elisp
;;                      |
;;     +----------------+
;;     |
;;     |
;;     +---------- emacsen (source-only dir shared by FSF Emacs, XEmacs)
;;     |              |
;;     |              +-- a.el
;;     |              +-- b.el
;;     |              |
;;     |              |
;;     |              +-- .fsfemacs20 (binary-only dir used by FSF Emacs 20)
;;     |              .        |
;;     |              .        +-- a.elc
;;     |              .        +-- b.elc
;;     |              . 
;;     |              ..  .xemacs21
;;     |                       .
;;     |                       . . a.elc
;;     |                       . . b.elc
;;     |
;;     +---------- fsfemacs20 (source-binary dir for FSF Emacs ver. 20)
;;     .              |
;;     .              +-- local-loaddefs.el
;;     .              +-- local-loaddefs.elc
;;     .              +-- x.el
;;     .              +-- x.elc
;;     .              +-- y.el
;;     .              +-- y.elc
;;     .
;;     . . . . . . xemacs21 (source-binary dir for XEmacs ver. 21)
;;              
;;
;; * call the top level function *
;;    bm-el-lib-keeper-update uses Common Lisp style arguments passing which
;;    looks like the following.
;;        (func :keyword1 val1 :keyword2 val2 :keyword3 val3)
;;    You can omit any keyword and value pairs. In that case the default
;;    values are used.
;;
;;   e.g.
;;   (bm-el-lib-keeper-update
;;                        :byte-compile     t
;;                                          ;; check and byte-compile
;;                                          ;; (default)
;;			  :autoload         t
;;                                          ;; check and update autoload 
;;                                          ;; (default)
;;			  :delete-types     ;; 
;;                           '(no-src-bin   ;; delete the compiled files
;;                                          ;; without sources. (default)
;;
;;                             stray-bin)   ;; delete the compiled files in
;;                                          ;; the source only directories.
;;                                          ;; (default)
;;
;;                                          ;; You can also put `stray-src'
;;                                          ;; in this list which causes
;;                                          ;; the deletion of the source files
;;                                          ;; in the binary-only directories.
;;
;;			  :rebuild-load-path t
;;                                           ;; set load-path using
;;                                           ;; global variables.
;;                                           ;; directories outside of this
;;                                           ;; library's scope are preserved.
;;                                           ;; (default)
;;    )
;;
;;
;;

;;; Code:
(require 'cl)
(require 'bm-el-lib-dir)
(require 'bm-el-bytecomp)
(require 'bm-el-autoload)
(defun bm-file-name-dirs-re (dirs &optional noexpand)
  "Return regexp recognizing files under DIRS.
More precisely, files which have any one of DIRS as a ancestor directory.
If NOEXPAND is non-`nil', names in DIRS are used as is, thus not expanded by
 `expand-file-name'."
  (concat "\\`" (mapconcat (function
			    (lambda (name)
			      (regexp-quote
			       (file-name-as-directory
				(if noexpand name (expand-file-name name))))))
			   dirs "\\|")))


;;=========================================================================
;; The following vars must be set. (some can be nil)
;;=========================================================================

;; Describe local library directory trees
(defvar bm-el-lib-keeper-top-src-trees nil
  "List of the names of top source directories which this library maintains."
  )

(defvar bm-el-lib-keeper-shared-src-dirs-re nil
  "Regexp recognizing the names of source-only directories.
Recognized directories must be under `bm-el-lib-keeper-top-src-trees' directory
trees.
Can be nil if you use only one specific Emacsen (FSF Emacs, XEmacs...)
If non-nil, bm-el-lib-keeper-shared-src-bin-dir-func must be set."
  )

(defvar bm-el-lib-keeper-shared-src-bin-dir-func nil
  "Function taking a name of a source-only directory, returning the name of the corresponding binary-only directory.
A binary-only directory must either be outside of 
`bm-el-lib-keeper-top-src-trees' or have names starting with a character 
other than [0-9A-Za-z].
The intention is to enable to specify different directories depending on
which Emacsen it runs on and the location of the source file."
)

;; Which files are checked for what
(defvar bm-el-lib-keeper-bin-check-dirs-re nil
  "Regexp recognizing the names of source directories whose source
files are checked to see if they need compilation.
Can be nil."
)

(defvar bm-el-lib-keeper-autoload-check-dirs-re-alist nil
  "Alist telling the mapping of autoload definition files and source 
directories.
    key    the name of the file containing autoload definitions of
           source files.
    value  regexp recognizing source directories whose source files' 
           autoload definitions are put into the key's file.
Can be nil."
)

;;=========================================================================
;; The preceding vars must be set.
;;=========================================================================


(defvar bm-el-lib-keeper-message-buffer "*Library keeper*"
  "Name of the buffer for logging.")

;;-------------------------------------------------------------------

(defun* bm-el-lib-keeper-update
    (&key (byte-compile      t)
	  (autoload          t)
	  (delete-types      '(no-src-bin stray-bin))
	  (rebuild-load-path t))
  "Update libraries under `bm-el-lib-keeper-top-src-trees'.
  BYTE-COMPILE          nil       don't compile
                        non-nil   compile new source files.

  AUTOLOAD              nil       don't update autoloads.
                        non-nil   update old autoload definition files.

  DELETE-TYPES          a list containing some of the following symbols.
                        no-src-bin stray-bin stray-src

  REBUILD-LOAD-PATH     non-nil rebuilds `load-path'"
  (interactive) ;; need UI
;;;  (unless bm-el-lib-keeper-top-src-trees
;;;    (error
;;;     "bm-el-lib-keeper-update: `bm-el-lib-keeper-top-src-trees' must not be `nil'"))
;;;
  (let* ((info (bm-el-lib-keeper-collect-default-info))
	 (need-autoload (cdr (assoc 'need-autoload info)))
	 (need-compile  (cdr (assoc 'need-compile  info)))
	 (message-buffer (get-buffer-create bm-el-lib-keeper-message-buffer))
	 (message-start  (with-current-buffer message-buffer 
			   (goto-char (point-max)) (point)))
	 (current-time   (current-time-string))
	 (del-type-msg
	  '((no-src-bin . "compiled files without the source files")
	    (stray-bin  . "compiled files in source-only directories")
	    (stray-src  . "source files in binary-only directories"))))

    ;; load-path setting
    (if rebuild-load-path (setq load-path (bm-el-lib-keeper-load-path)))

    ;; deletion
    (loop for type in '(no-src-bin stray-bin stray-src)
	  for src-list = (cdr (assoc type info))
	  if (and (member type delete-types) src-list) do
	     (with-current-buffer message-buffer
	       (insert
		(format "** DELETE %s **\n" (cdr (assoc type del-type-msg)))))
	     (bm-el-lib-keeper-delete (cdr (assoc type info)))
	  else if src-list do
	     (with-current-buffer message-buffer
	       (insert
		(format "++ Found %s ++\n" (cdr (assoc type del-type-msg)))
		(mapconcat 'identity src-list "\n") "\n\n\n"))
          end)

    ;; update autoload
    (when need-autoload
      (if (not autoload)
	  (with-current-buffer message-buffer
	    (loop for (def-file . src-list) in need-autoload
		  do
		  (insert
		   "++ Found source files in need of autoload check ++\n"
		   (format "Autoloads definition file: %s\n" def-file)
		   (mapconcat 'identity src-list "\n") "\n\n")))
	(with-current-buffer message-buffer (insert "** AUTOLOAD UPDATE **\n"))
	(bm-el-lib-keeper-update-autoload-alist
	 (cdr (assoc 'need-autoload info)))
	;; check if loaddefs files must be byte-compiled and reloaded.
	(let ((def-alist 
		(loop for pair in bm-el-lib-keeper-autoload-check-dirs-re-alist
		      for src = (expand-file-name (car pair))
		      for bin = (bm-el-lib-keeper-bin-name src)
		      if (and
			  (string-match bm-el-lib-keeper-bin-check-dirs-re src)
			  (file-newer-than-file-p src bin))
		          collect (cons src bin))))
	  ;; Although need-compile may contain the same loaddefs, that's ok.
	  (when def-alist
	    (bm-el-lib-keeper-bytecomp def-alist "FORCE-RELOAD")))))

    ;; byte compile 
    (when (and byte-compile need-compile)
      (with-current-buffer message-buffer (insert "** BYTE COMPILATION **\n"))
      (bm-el-lib-keeper-bytecomp (cdr (assoc 'need-compile info)) t))

    (when (with-current-buffer message-buffer (> (point-max) message-start))
      (switch-to-buffer-other-window message-buffer)
      (goto-char message-start)
      (insert "\f\n" current-time "\n\n")
      (search-backward "\f\n")
      (recenter 0))

    ;; Should return an alist of the result in the future version.
))

(defun bm-el-lib-keeper-bin-name (src)
  "Return the absolute name of the compiled file of SRC using global variables."
  (setq src (expand-file-name src))
  (if (string-match bm-el-lib-keeper-shared-src-dirs-re src)
      (expand-file-name
       (bm-el-lib-bin-name (file-name-nondirectory src))
       (funcall bm-el-lib-keeper-shared-src-bin-dir-func
		(file-name-directory src)))
    (bm-el-lib-bin-name src)))

(defun bm-el-lib-keeper-load-path ()
  "Return a list appropriate for `load-path'."
  (let* ((top-dirs-re (bm-file-name-dirs-re bm-el-lib-keeper-top-src-trees))
	 (other-path  (loop for dir in load-path
			    if (not (string-match top-dirs-re dir)) 
			      collect dir))
	 (dir-list bm-el-lib-keeper-top-src-trees)
	 path bin-path dir dir-files)
    (while dir-list
      (setq dir (car dir-list)
            path (cons dir path)
	    dir-files (bm-el-lib-dir-files dir "FULL-NAME"))
      (when (and bm-el-lib-keeper-shared-src-dirs-re
		 (string-match bm-el-lib-keeper-shared-src-dirs-re dir))
	(setq bin-path (cons (funcall bm-el-lib-keeper-shared-src-bin-dir-func
				      dir)
			bin-path)))
      (setq dir-list (nconc (cdr (assoc 'subdir dir-files)) (cdr dir-list))))
    (nconc other-path (nreverse bin-path) (nreverse path))))

(defun bm-el-lib-keeper-collect-default-info ()
  "Call `bm-el-lib-keeper-collect-info' with libraries global variables."
  (bm-el-lib-keeper-collect-info
   bm-el-lib-keeper-top-src-trees
   bm-el-lib-keeper-shared-src-dirs-re
   bm-el-lib-keeper-shared-src-bin-dir-func
   bm-el-lib-keeper-bin-check-dirs-re
   bm-el-lib-keeper-autoload-check-dirs-re-alist))


(defun bm-el-lib-keeper-collect-info (top-src-trees
				      &optional shared-src-dirs-re
				                shared-src-bin-dir-func
				                bin-check-dirs-re
						autoload-check-dirs-re-alist)
  "Return an associative list of Emacs Lisp source and binary files' info.
 Arguments
   TOP-SRC-TREES
       A list of the names of source directories which are searched
       recursively by this function.

   SHARED-SRC-DIRS-RE
       A regexp recognizing the names of source-only directories.
       If non-`nil', SHARED-SRC-BIN-DIR-FUNC must be set.
   
   SHARED-SRC-BIN-DIR-FUNC
       A function taking a name of a source-only directory,
       returning the name of the corresponding binary-only directory.
   
   BIN-CHECK-DIRS-RE
       A regexp recognizing the names of source directories whose source
       files are checked to see if they need compilation.
   
   AUTOLOAD-CHECK-DIRS-RE-ALIST
       An alist telling the mapping of autoload definition files and source 
       directories.
       key    the name of the file containing autoload definitions of
              source files defined by value.
       value  regexp recognizing source directories whose source files' 
              autoload definitions are put into the key's file.


The returned alist's keys and values are as follows.

    key symbol      value
    --------------------------------------------------------------------------
    src             A list of the absolute names of all the source files under
                    source-binary directories and source-only directories.
    bin             A list of the absolute names of the existing compiled 
                    files corresponding to src, plus no-src-bin.
    src-path        A list of the absolute names of source-binary directories
                    and source-only directories.
    bin-path        A list of the absolute names of binary-only directories.
    need-compile    An associative list. The key is the absolute name of a
                    source file whose directory matches BIN-CHECK-DIRS-RE 
                    and needs to be compiled. 
                    The value is the absolute name of the compiled file 
                    corresponding to the key's source file.
    need-autoload   An associative list. The key is one of the keys of 
                    AUTOLOAD-CHECK-DIRS-RE-ALIST.
                    The value is a list of the absolute names of source files
                    that need to be ckecked for autoload definitions .
    no-src-bin      A list of the absolute names of compiled files under 
                    source-binary directories and binary-only directories,
                    without the corresponding source files.
    stray-src       A list of the absolute names of the source files under 
                    binary-only directories.
    stray-bin       A list of the absolute names of the compiled files under 
                    source-only directories.
    --------------------------------------------------------------------------
"
  (let* ((dir-list (mapcar 'expand-file-name top-src-trees))
	 src-dir bin-dir loaddefs src-path bin-path
	 src bin no-src-bin need-compile need-autoload 
	 stray-src stray-bin)
    (while dir-list
      (setq src-dir  (car dir-list)
            bin-dir  (if (and shared-src-dirs-re
			      (string-match shared-src-dirs-re src-dir))
			 (funcall shared-src-bin-dir-func src-dir) src-dir)
            loaddefs (if autoload-check-dirs-re-alist
			 (loop for (name . re) in autoload-check-dirs-re-alist
			       if (string-match re src-dir) return name))
            info     (bm-el-lib-dir-info src-dir bin-dir "FULL-NAME" loaddefs))

      (when (and bin-check-dirs-re (string-match bin-check-dirs-re src-dir))
	(setq need-compile
	      (nconc
	       (loop for src in (cdr (assoc 'need-compile info))
		     collect ;; ((src1 . bin2) (src2 . bin2) ...)
		      (cons src
			    (expand-file-name 
			     (bm-el-lib-bin-name (file-name-nondirectory src))
			     bin-dir)))
	       need-compile)))

      (when (and loaddefs (cdr (assoc 'need-autoload info)))
	(if (assoc loaddefs need-autoload)
	    (setcdr (assoc loaddefs need-autoload)
		    (nconc (cdr (assoc 'need-autoload info))
			   (cdr (assoc loaddefs need-autoload))))
	  (setq need-autoload
		(cons (cons loaddefs (cdr (assoc 'need-autoload info)))
		      need-autoload))))

      (loop
       for symbol in '(src bin no-src-bin stray-src stray-bin)
       do (set symbol (nconc (cdr (assoc symbol info)) (symbol-value symbol))))

      (unless (string= src-dir bin-dir) 
	(setq bin-path (cons bin-dir bin-path)))
      (setq src-path (cons src-dir src-path))
      (setq dir-list (nconc (cdr (assoc 'subdir info)) (cdr dir-list))))

    (loop for symbol in '(src bin src-path bin-path need-compile need-autoload
			  stray-bin stray-src no-src-bin)
     collect (cons symbol (symbol-value symbol)))))

(defun bm-el-lib-keeper-bytecomp (src-bin-alist &optional reload 
						          message-buffer)
  "Byte-compile and rename Emacs Lisp files using SRC-BIN-ALIST.
Use keys of SRC-BIN-ALIST as source file names, and values as compiled file
names. Return an alist of failed file names and error messages.
If RELOAD is `nil', reload nothing.
If RELOAD is t, reload already loaded libraries (assuming the libraries
provide their file names without extensions as features).
Otherwise, load all successfully compiled files.
If MESSAGE-BUFFER is non-`nil', use it as an output buffer for messages.
"
  (unless message-buffer
    (setq message-buffer (get-buffer-create bm-el-lib-keeper-message-buffer)))
  (let* ((failed    (bm-el-bytecomp-and-rename-files src-bin-alist))
	 (installed (set-difference (mapcar 'car src-bin-alist)
				    (mapcar 'car failed))))

    (if (featurep 'xemacs) ;; needed to make `locate-library' to find bin file.
	(locate-file-clear-hashing load-path))
    (when (or installed failed)
      (with-current-buffer message-buffer
	(when failed
	  (insert
	   "Error occurred while compiling and installing these files.\n")
	  (loop for (err-src . err-msg) in failed
		for err-bin = (or (cdr (assoc err-src src-bin-alist))
				  (bm-el-lib-bin-name err-src))
		do (insert "src:" err-src "\n"
			   "bin:" err-bin "\n"
		           "error message: " err-msg "\n\n"))
	  (insert "\n"))

	(when installed
	  (insert "Successfully compiled and installed files.\n")
	  (loop for src in installed
		for bin = (or (cdr (assoc src src-bin-alist)) 
			      (bm-el-lib-bin-name src))
		for feature-name = (file-name-sans-extension
				     (file-name-nondirectory src))
		for feature = (read feature-name)
		for located = (if reload (locate-library feature-name))
		do (insert "src:" src "\n"
                           "bin:" bin)
		(when (and reload
			   (or (and (eq reload t) (featurep feature))
			       (not (eq reload t))))
		  (condition-case err
		      (progn
			(load bin nil nil "NOSUFFIX")
			(insert " (reloaded")
			(cond
			 ((null located)
			  (insert " but not in the `load-path'"))
			 ((not (string= (file-truename located)
					(file-truename bin)))
			  (insert (format
			   " but %s has higher precedence in the `load-path'"
				   located)))))
		    (error
		     (insert
		      " (failed to reload %s" (error-message-string err))))
		  (insert ")"))
		(insert "\n\n")))
	(insert "\n")))
    failed))

(defun bm-el-lib-keeper-delete (file-list &optional message-buffer)
  "Delete files in FILE-LIST. Return an alist of failed files and error messages."
  (unless message-buffer
    (setq message-buffer (get-buffer-create bm-el-lib-keeper-message-buffer)))
  (let (failed deleted)
    (loop for file in file-list
	  do
	  (condition-case err
	      (progn (delete-file file) (setq deleted (cons file deleted)))
	    (error (setq failed (cons (cons file (error-message-string err))
				      failed)))))
    (when (or deleted failed)
      (with-current-buffer message-buffer
	(when failed
	  (insert "Error occurred while deleting these files.\n")
	  (loop for (err-src . err-msg) in failed
		do (insert err-src "\n")
		   (insert "error message: " err-msg "\n\n")))
	(when deleted
	  (insert "These files were successfully deleted.\n"
		  (mapconcat 'identity deleted "\n") "\n\n"))
	(insert "\n")))
    failed))


(defun bm-el-lib-keeper-update-autoload (src-list def-file &optional
						           message-buffer)
  (unless message-buffer
    (setq message-buffer (get-buffer-create bm-el-lib-keeper-message-buffer)))

  ;; assume def-file is never modified by hand and kill the buffer even if
  ;; it is modified.
  (let ((already-open (or (get-file-buffer def-file)
			  (get-file-buffer (file-truename def-file)))))
    (when already-open
      (with-current-buffer already-open (set-buffer-modified-p nil))
      (kill-buffer already-open)))

  (let* ((def-buffer   (find-file-noselect def-file))
	 (failed       (bm-el-autoload-update-files src-list def-file))
	 (checked      (set-difference src-list (mapcar 'car failed))))
    (when (or failed checked)
      (with-current-buffer message-buffer
	(insert (format "Autoload update: %s\n" def-file))
	(when failed
	  (insert "Error occurred while checking these files.\n")
	  (loop for (err-src . err-msg) in failed
		do (insert err-src "\n")
		   (insert "error message: " err-msg "\n\n"))
	  (insert "\n"))

	(when checked
	  (insert "Sucessfully checked files.\n"
		  (mapconcat 'identity checked "\n") "\n\n"))
	(insert "\n")
	))
    (with-current-buffer def-buffer
      (if (and checked (not failed))
	  (progn (set-buffer-modified-p t) (save-buffer))
	(set-buffer-modified-p nil)))
    (kill-buffer def-buffer)
    failed))


(defun bm-el-lib-keeper-update-autoload-alist (autoload-alist
					       &optional message-buffer)
  "Update autoload defition files using AUTOLOAD-ALIST.
Update autoload definition files which are the keys of AUTOLOAD-ALIST for
Emacs Lisp source files which are the values of AUTOLOAD-ALIST and containing
special \";;;###autoload\" comment.
Return an alist whose keys are the names of failed autoload definition files,
and whose values are an alist of failed source files and error messages."
  (let (failed-alist)
    (loop for (def-file . src-list) in autoload-alist
	  collect (cons def-file
			(bm-el-lib-keeper-update-autoload src-list def-file
							  message-buffer)))
    failed-alist))


(provide 'bm-el-lib-keeper)

;;; bm-el-lib-keeper.el ends here
