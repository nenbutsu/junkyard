;;; bm-iman.el --- call man & Info viewer + completion + short descriptions

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, help

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
;; Call Unix manual page & GNU Info viewer with completion.
;; The completions buffer can display possible completions with or without
;; their short descriptions.
;;
;; If you don't know what "completion" is, see "(emacs)Completion".


;; Requirement
;; apropos(1) program.
;; Tested with FSF Emacs 20.6.2 and XEmacs 21.1.9.

;; Install
;; * Put bm-iman.el and bm-mb.el in one of the directories listed in
;;   `load-path'.
;; * Write the following lines in your .emacs file.
;;   (autoload 'bm-iman "bm-iman" "bm-iman: call man & info viewer")
;; * Bind some key to `bm-iman' if you like.


;; Usage
;; * Invoke the command with `M-x bm-iman' or the key sequence you bound to
;;   bm-iman.
;;   Renew the index with prefix argument like `C-u M-x bm-iman'.
;;
;; * In the minibuffer, key sequences described in 
;;   "(emacs)Completion Commands" should work as usual.
;;   Here's some of the key sequences you probably use most.
;;      <TAB>        completion.
;;      <SPACE>      word completion.
;;                   You can use this key to enter '(', ' ' and '-'.
;;      M-v          Go into the completion buffer.
;;                   Inside the completion buffer, press <RET> to select.
;;      `C-u <TAB>'  toggles whether to display short descriptions in the
;;                   completions buffer.
;;                   This behavior is specific to `bm-iman'.
;;
;; * 

;;; Code:

;;; declarations to suppress compiler warnings
(eval-when-compile
  (autoload 'Info-directory "info")
  (autoload 'Info-menu "info")
  (autoload 'manual-entry "man"))


;;; customization
(defcustom bm-iman nil
  ""
  :group 'help)


(defcustom bm-iman-merge-Info-menu-items t
  "Non-nil means merging the items of GNU Info top directory to the index."
  :type 'boolean
  :group 'bm-iman)

(defcustom bm-iman-man-index-command-and-args '("man" "-k" "")
  "List of a command name and arguments to dump man page index.
Please note that the last empty string in the default is significant.
The default should get the same output as the one you get when
you enter \"man -k ''\" in a shell session, which should print out
something like the following.
-------------------------------------------------------------------
~$ man -k ''
. (1) [builtins]     - bash built-in commands, see bash(1)
.ldaprc (5) [ldaprc] - ldap configuration file
.netrc (5) [netrc]   - user configuration for ftp
2a_ctrl (4)          - (unknown)
2b_romkana (4)       - (unknown)
822-date (1)         - Print date and time in RFC822 format
....... 
-------------------------------------------------------------------"
  :type  '(repeat string)
  :group 'bm-iman)


(defcustom bm-iman-index-obarray-bucket-number 4095
  ""
  :type 'integer
  :group 'bm-iman)

;;; Variables for internal use
(defvar bm-iman-index-obarray nil
  "")


;;;###autoload
(defun bm-iman (item)
  "Read a man spec. or an Info menu item with completion, then display the page.
With prefix argument, rebuild the index.

In the minibuffer, completion keys work as usual.
<Tab> with Prefix argument like `C-u <TAB>' toggles whether to display
 short descriptions in the completions buffer."
  (interactive (list (progn
                       (when (or current-prefix-arg
                                 (not (vectorp bm-iman-index-obarray)))
                         (bm-iman-build-index))
                       (bm-iman-read))))
  (string-match "\\`\\([^(]*\\)(?\\([^)]*\\)" item) ; always match
  (let ((name     (match-string 1 item))
        (section  (match-string 2 item)))
    (cond
     ((string= item ""))                ; do nothing

     ((string= section "")              ; Info
      (require 'info)                   ; standard info viewer
      (Info-directory)
      (Info-menu item))

     (t                                 ; man
      (require 'man)                    ; standard man viewer
      (if (featurep 'xemacs) (manual-entry item)
        (manual-entry (concat section " " name)))))))


(defun bm-iman-read ()
  "Minibuffer read function for `bm-iman'."
  (let* ((completion-highlight-first-word-only t) ; for XEmacs
         (prompt          "iman")
         (collection      bm-iman-index-obarray)
         (predicate       nil)
         (require-match   t)
         (initial-content nil)
         (history         nil)
         (default         (bm-iman-entry-at-point))
         (prompt-with-default (format "%s%s"
                                      prompt
                                      (if default
                                          (concat " (default " default "): ")
                                        ": "))))
    (bm-iman-completing-read prompt-with-default collection predicate
                             require-match initial-content history
                             default)))


(defun bm-iman-completing-read (prompt collection
                                       &optional predicate require-match
                                       init history default
                                       inherit-input-method)
  "Compatibility function for FSF Emacs and XEmacs."
  (let* ((minibuffer-completion-table     collection)
         (minibuffer-completion-predicate predicate)
         (minibuffer-completion-confirm 
          ;; This variable is used in `bm-mb-complete-and-exit' only when
          ;; `REQUIRE-MATCH' is non-nil.
          (if (eq require-match 't) nil t))

         (keymap (if require-match 
                     minibuffer-local-must-match-map
                   minibuffer-local-completion-map))
         (input (cond
                 ((featurep 'xemacs)
                  (read-from-minibuffer prompt init keymap nil history))
                 (t
                  (read-from-minibuffer prompt init keymap nil history
                                        default inherit-input-method)))))
    ;; default handling
    (or (and (string= input "") default)
        input)))


;;; item at point
(defun bm-iman-entry-at-point ()
  (or (bm-iman-man-spec-at-point)
      (let* ((case-fold-search t)
             (name (bm-iman-symbol-name-at-point))
             name-re)
        (when name
          (setq name-re (concat "\\`" (regexp-quote name) "\\((\\|\\'\\)"))
          (catch 'found
            (mapatoms
             #'(lambda (entry-symbol)
                 (when (string-match name-re (symbol-name entry-symbol))
                   (throw 'found (symbol-name entry-symbol))))
             bm-iman-index-obarray))))))


(defun bm-iman-symbol-name-at-point ()
  "Return a symbol name at point. Return nil if none found.
Ripped off out of Mike Williams's thingatpt.el"
  (save-excursion
    (save-match-data
      (let* ((orig-pt (point))
             (bol     (prog2 (beginning-of-line) (point) (goto-char orig-pt)))
             (eol     (prog2 (end-of-line)       (point) (goto-char orig-pt)))
             (sym-beg (progn (skip-syntax-backward "w_" bol) (point)))
             (sym-end (progn (skip-syntax-forward  "w_" eol) (point))))
        (unless (= sym-beg sym-end)
          (buffer-substring sym-beg sym-end))))))


(defun bm-iman-man-spec-at-point ()
  ""
  (interactive)                         ; for debug
  (save-excursion
    (save-match-data
      (let* ((orig-pt (point))
             (bol     (prog2 (beginning-of-line) (point) (goto-char orig-pt)))
             (eol     (prog2 (end-of-line)       (point) (goto-char orig-pt)))
             (rpar    (when (or (and (char-before)
                                     (char-equal (char-before) ?\)))
                                (search-forward ")" eol t))
                        (point)))       ; "`ls (1)-!-'"

             (lpar    (and rpar
                           (search-backward "(" bol t)))
                                        ; "`ls -!-(1)'"

             (sec     (when (looking-at "([1-9][a-zA-Z0-9]*)")
                        (buffer-substring lpar rpar)))
             (name-end (when sec
                         (when (and (char-before)
                                    (char-equal (char-before) ?\ ))
                           (backward-char))
                         (point))))     ; "`ls-!- (1)'"
        (when sec
          (when (re-search-backward "\\s-" bol "MOVE-ON-FAIL")
            (forward-char))             ; "-!-`ls (1)'"
          (when (catch 'found
                  (while (and (<= (point) (min orig-pt lpar)))
                    (when (intern-soft
                           (concat (buffer-substring (point) name-end) sec)
                           bm-iman-index-obarray)
                      (throw 'found t))
                    (forward-char)))    ; "`-!-ls (1)'" gotcha!
            (concat (buffer-substring (point) name-end) sec)))))))


;;; Index ------------------------------------------------------------
;;;###autoload
(defun bm-iman-build-index ()
  (interactive)
  (setq bm-iman-index-obarray
        (make-vector bm-iman-index-obarray-bucket-number 0))
  (message "Making bm-iman index...")
  (bm-iman-build-man-index bm-iman-index-obarray)
  (when bm-iman-merge-Info-menu-items
    (bm-iman-build-Info-index bm-iman-index-obarray))
  (message nil))
    
  
;;; man index.
(defvar bm-iman-man-index-regexp
  ;;        1             2            3
  ;;      _____          ___      _______________________________
  ;;      alias          (1)       [builtins] - bash built-in ...
  "^\\([^ (]+\\) *\\(([^ )]+)\\)\\(.*\\)$"

  ;; This is just a note
  ;;        1             2                3                    4
  ;;      alias          (1)          [builtins]        - bash built-in ...
  ;;"^\\([^ (]+\\) *\\(([^ )]+)\\) *\\(\\[[^]]+\\]\\)? *- \\(.*\\)$"
  "Regular expression recognizing the output of apropos(1).")

(defvar bm-iman-man-index-buffer-name
  "*bm-iman-man-index*"
  "")


(defun bm-iman-build-man-index (index-obarray)
  ""
  (when (get-buffer bm-iman-man-index-buffer-name)
    (kill-buffer bm-iman-man-index-buffer-name))
  (let* ((buffer       (get-buffer-create bm-iman-man-index-buffer-name))
         (program      (car bm-iman-man-index-command-and-args))
         (args         (cdr bm-iman-man-index-command-and-args))
         (exit-status  (apply 'call-process
                              program
                              nil       ; < /dev/null
                              buffer    ; > current temporary buffer
                              nil       ; no redisplay
                              args)))
    (when (not (zerop exit-status))
      (switch-to-buffer-other-window buffer)
      (goto-char (point-max))
      (error "%s program returned error exit status %s" program exit-status))

    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward bm-iman-man-index-regexp nil t)
        (set (intern (concat (match-string 1) (match-string 2)) index-obarray)
             (match-string 3))))
    ;;(kill-buffer bm-iman-man-index-buffer-name)
    ))


;;; GNU Info top index.
(defvar bm-iman-info-menu-regexp
  ;;  "*    Binutils: (binutils).         The GNU binary utilities."
  ;;  (match-string 1) => "Binutils"
  ;;  (match-string 2) => ": (binutils).         The GNU binary utilities."
  "^\\* \\([^:]+\\)\\(:.*\\)$"
  "")


(defun bm-iman-build-Info-index (index-obarray
                                 &optional file node)
  ""
  (unless file (setq file "dir"))
  (unless node (setq node "top"))
  (let* (item description)
    (save-excursion
      (save-window-excursion
        (with-temp-buffer
          (Info-mode)
          (Info-find-node file node)
          (goto-char (point-min))
          (while (re-search-forward bm-iman-info-menu-regexp nil t)
            (setq item        (downcase (match-string 1))
                  description (match-string 2))
            (set-text-properties 0 (length item) nil item)
            (set-text-properties 0 (length description) nil description)
            (set (intern item index-obarray) description)))))))


(provide 'bm-iman)

;;; bm-iman.el ends here
