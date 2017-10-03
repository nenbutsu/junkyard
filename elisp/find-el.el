;;; find-el.el --- skeletal emacs lisp library file

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
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

;; This package is based on the following package:
;;
;;   find-library.el --- a library finder with completion capability.
;;   Author: Takeshi Morishima <tm@interaccess.com>
;;
;; which is distributed under GPL.
;; Thanks to the author for writing an excellent package.


;;; Commentary:


;;; Code:

(eval-when-compile
  (defvar current-prefix-arg)
  (defvar default-directory)
  (defvar load-path))
  
(defvar find-el-history nil
  "List of elisp source files user has visited.")

(defvar find-el-ignore-case nil
  "Non-nil means `find-el' command ignores case differences in completion.")

(defvar find-el-alist nil
  "Alist for `find-el', whose element is (file-spec directory filename).")

(defun find-el (spec)
  "Find a elisp source file with completion."
  (interactive (list (progn
                       (when (or current-prefix-arg
                                 (not (find-el-alist-fresh-p load-path)))
                         (find-el-update-alist load-path))
                       (find-el-minibuffer-read nil nil 'find-el-history))))
  (when spec
    (let ((assoc (assoc spec find-el-alist)))
      (find-file (expand-file-name (cadr (cdr assoc)) (cadr assoc))))))


(defun find-el-minibuffer-read (&optional pred init history)
  "Read the name of an elisp source file in the minibuffer."
  (let* ((completion-ignore-case find-el-ignore-case)
         (table find-el-alist)
         (must-match t)
         (default nil)
         (prompt (format "Find elisp source%s: " (if default
                                        (format " (default %s)" default)
                                      ""))))
    (find-el-completing-read prompt
                             table pred must-match init history default)))


(defun find-el-completing-read (prompt table
                                       &optional pred must-match init
                                       history default inherit-input-method)
  "Compatibility function for FSF Emacs and XEmacs."
  (let* ((common-args (list prompt table pred must-match init history))
         (args (append common-args (unless (featurep 'xemacs)
                                     (list default inherit-input-method))))
         (input (apply 'completing-read args)))
    (or (and (string= input "") default) ; default handling
        input)))


(defvar find-el-dirs-mtimes nil
  "Cons cell whose car is dir list, and whose cdr is dir's mtime list.
e.g. ((\"dir1\" \"dir2\") (<mtime-of-dir1> <mtime-of-dir2>)).
<mtime-of-dirN> is a list of two integer returned by file-attributes.")

(defun find-el-alist-fresh-p (dir-list)
  "Return non-nil when `find-el-alist' is fresh against DIR-LIST."
  (and find-el-dirs-mtimes
       (equal (car find-el-dirs-mtimes) dir-list)
       (equal (cdr find-el-dirs-mtimes)
              (mapcar '(lambda (dir) (elt (file-attributes dir) 5))
                      dir-list))))

(defun find-el-update-alist (dir-list)
  "Update `find-el-alist' using DIR-LIST."
  (setq find-el-alist (find-el-make-alist dir-list))
  (setq find-el-dirs-mtimes
        (cons (apply 'list dir-list)
              (mapcar #'(lambda (dir) (elt (file-attributes dir) 5))
                      dir-list))))

(defvar find-el-ext-regexp "\\.el\\(\\.gz\\)?\\'"
  "Regular expression denoting file name extensions for elisp source files.")

(defun find-el-name-p (name)
  "Return non-nil if name looks like a elisp source file.
NAME is assumed to be under `default-directory'."
  (or (file-regular-p name)
      (and (file-symlink-p name)
           (file-regular-p (file-truename (expand-file-name name))))))

(defun find-el-make-alist (dir-list)
  "Make an alist for `find-el', whose element is (spec directory filename).
e.g. ((\"my-lib\"    \"~/elisp\"     \"my-lib.el\")
      (\"my-lib<2>\" \"~/elisp/old\" \"my-lib.el.gz\"))"
  (let ((default-directory default-directory) ; create another binding
        (name-obarray (make-vector 255 nil))
        dir name-list name name-sans-ext alist key val sym)
    (while dir-list
      (setq dir (car dir-list)
            dir-list (cdr dir-list))
      (when (file-directory-p dir)
        (setq name-list (directory-files dir nil find-el-ext-regexp t))
        (setq default-directory (file-name-as-directory dir))
        ;; current dir is now `dir'
        (while name-list
          (setq name (car name-list)
                name-list (cdr name-list))
          (when (find-el-name-p name)
            (setq name-sans-ext (progn (string-match find-el-ext-regexp name)
                                       (substring name 0 (match-beginning 0))))
            (setq sym (intern name-sans-ext name-obarray))
            (cond
             ((boundp sym)
              (setq val (symbol-value sym))
              (setcdr val (1+ (cdr val)))
              (setq key (concat (car val)
                                "<" (number-to-string (cdr val)) ">")))
             (t
              (set sym (cons name-sans-ext 1))
              (setq key (car (symbol-value sym)))))
            (setq alist (cons (list key default-directory name) alist))))))
    alist))




(provide 'find-el)

;;; find-el.el ends here
