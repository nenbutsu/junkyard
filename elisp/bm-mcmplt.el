;;; bm-mcmplt.el --- completion with multiple collections

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, help, abbrev

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
;; This library provides a programmed completion function `bm-mcmplt' for
;; `completing-read' with a capability of handling multiple collections.
;; See "(elisp)Programmed Completion".
;;
;; Example
;; #1: Mix various kinds of collections.
;;    (let ((bm-mcmplt-collection-predicate-alist
;;           '((alist . some-predicate)
;;             (obarray . nil) ;; same as (obarray), thus, no predicate
;;             (completion-func . another-predicate))))
;;      (completing-read "input: " 'bm-mcmplt dummy-predicate-to-be-ignored))
;;
;;
;; #2: Use a bunch of already classified alists.
;;    (let ((bm-mcmplt-collection-predicate-alist
;;           '((a-alist . predicate)
;;             (b-alist . predicate)
;;             (c-alist . predicate)
;;             ..
;;             (z-alist . predicate))))
;;      (completing-read "input: " 'bm-mcmplt))
;;

;;; Code:
(require 'cl)

(defvar bm-mcmplt-collection-predicate-alist
  nil
  "Alist whose association is (collection . predicate).
Referred by `bm-mcmplt' and its subordinate functions.
Set this variable before you call standard `completing-read' with
`bm-mcmplt'.")


(defun bm-mcmplt (string dummy-predicate flag)
  "Programmed completion function with multiple collections.
The meanings of STRING and FLAG are the same as a normal programmed completion
function described in \"(elisp)Programmed Completion\".
DUMMY-PREDICATE is completely ignored by this function.
Use `bm-mcmplt-collection-predicate-alist' to specify a predicate for each
collection."
  (funcall (cond ((null flag)       'bm-mcmplt-try-completion)
                 ((eq flag t)       'bm-mcmplt-all-completions)
                 ((eq flag 'lambda) 'bm-mcmplt-match-exists)
                 (t (error "bm-mcmplt: Invalid flag: %s"
                           (prin1-to-string flag))))
           string))


(defun bm-mcmplt-try-completion (string)
  "Multiple collections version of `try-completion'."
  (let (result)
    (loop for (collection . predicate) in bm-mcmplt-collection-predicate-alist
          for try = (try-completion string collection predicate)
          do
          (cond
           ((eq try t)
            (if (or (null result) (eq result t))
                (setq result t)
              (setq result string)))
           ((stringp try)
            (cond
             ((stringp result) (setq result (bm-mcmplt-max-prefix result try)))
             ((null result)    (setq result try))
             (t                (setq result string))))
           ((null try)) ; do nothing
           (t (error "bm-mcmplt-try-completion: got invalid result"))))
    result))


(defun bm-mcmplt-all-completions (string)
  "Multiple collections version of `all-completions'."
  (loop for (collection . predicate) in bm-mcmplt-collection-predicate-alist
        nconc (all-completions string collection predicate)))


(defun bm-mcmplt-match-exists (string)
  "Return t if STRING exists in at least one of collections, otherwise nil."
  (let ((test-f (if completion-ignore-case 'equalp 'equal)))
    (loop for (collection . predicate) in bm-mcmplt-collection-predicate-alist
          if (member* string (all-completions string collection predicate)
                      :test test-f)
            return t
          end
          finally return nil)))


(defun bm-mcmplt-max-prefix (a b)
  (let ((a-len (length a))
        (b-len (length b))
        (case-fold-search completion-ignore-case)) ; for char-equal
    (substring a
               0
               (loop for i from 0 to (max a-len b-len)
                     while (and (< i a-len)
                                (< i b-len)
                                (char-equal (aref a i) (aref b i)))
                     finally return i))))


(provide 'bm-mcmplt)

;;; bm-mcmplt.el ends here
