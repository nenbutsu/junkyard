;;; bm-lib-mnt.el --- local Emacs Lisp library maintenance commands.

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
;; Local Emacs Lisp library maintenance commands.
;; This library was tested with FSF Emacs 20.6.2 and XEmacs 21.1.8.
;;
;;

;;; Code:
(require 'bm-autoload)
(require 'bm-bytecomp)


(defvar bm-lib-mnt-autoload-alist
  nil
  "*Alist for automatic update of Emacs Lisp autoloads definitions.
Association is (autoload-file source-directory...).
e.g. (setq bm-lib-mnt-autoload-alist
           '((autoload-fileA src-dirA0 src-dirA1)
             (autoload-fileB src-dirB0 src-dirB1 src-dirB2)))
     In this case, autoloads definitions for all the source files in 
     src-dirA0 and src-dirA1 (not including their sub directories) are 
     put into autoload-fileA.
     And the same rule applies to autoload-fileB, src-dirB0, src-dirB1 and
     src-dirB2.
")


;;;###autoload
(defun bm-lib-mnt-update-autoload ()
  "Collect & update autoload definitions of Emacs Lisp source files.
This command uses `bm-lib-mnt-autoload-alist' as configuration."
  (interactive)
  (let ((alist bm-lib-mnt-autoload-alist))
    (while alist
      (bm-autoload-update-from-directories (caar alist) (cdar alist))
      (load (caar alist))
      (setq alist (cdr alist)))))



(defvar bm-lib-mnt-src-bin-dir-tree-map
  nil
  "*List denoting Emacs Lisp library directory hierarychy for byte-compilation.
Every element represents a mapping from a source directory tree to 
a binary directory tree.
e.g. (setq bm-lib-mnt-src-bin-dir-tree-map
           '((src-dir-tree1 . bin-dir-tree1)
             src-bin-dir-tree2))
     The byte-compiled files of source files under src-dir-tree1 are placed
     under the corresponding bin-dir-tree1.
     When source files and their compiled files are to be placed in the same
     directory, only one directory tree needs to be specified (which is 
     the case for src-bin-dir-tree2).
     Direcoty trees are specified by their top directory name,
     e.g. (setq bm-lib-mnt-src-bin-dir-tree-map
                '((\"/usr/local/share/emacs/site-lisp/package1\" . 
                   \"/usr/local/share/emacs/20.6/site-lisp/package1\")
                  \"/usr/local/share/emacs/site-lisp/package2\"))
Currently all directory trees must be isolated from each other.")


;;;###autoload
(defun bm-lib-mnt-recompile ()
  "Recompile Emacs Lisp source files if needed.
This command uses `bm-lib-mnt-src-bin-dir-tree-map' as configuration."
  (interactive)
  (let ((map bm-lib-mnt-src-bin-dir-tree-map)
	src-tree bin-tree)
    (while map
      (if (stringp (car map)) (setq src-tree (car map)
				    bin-tree nil)
	(setq src-tree (caar map)
	      bin-tree (cdar map)))
      (bm-bytecomp-recompile-directory src-tree bin-tree 0 nil "reload")
      (setq map (cdr map)))))


;;;###autoload
(defun bm-lib-mnt ()
  "Update library (autoloads update and byte-compilation).
This command uses `bm-lib-mnt-update-autoload' and `bm-lib-mnt-recompile'."
  (interactive)
  (bm-lib-mnt-update-autoload)
  (bm-lib-mnt-recompile))


(provide 'bm-lib-mnt)

;;; bm-lib-mnt.el ends here
