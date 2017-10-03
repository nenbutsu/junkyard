;;; minibuffer-commands.el --- minibuffer completion commands in elisp

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local, extensions

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
;; Minibuffer completion commands.


;;; Code:
(defvar minibuffer-commands-minibuffer-setup-hook '()
  "Hook variable which is run whenever the minibuffer is entered.")

(add-hook 'minibuffer-setup-hook
          #'(lambda () (run-hooks 'minibuffer-commands-minibuffer-setup-hook)))


;;; Core functions
(defvar minibuffer-commands-try-completion-function
  'try-completion
  "Function which should be equivalent to `try-completion'.")

(defvar minibuffer-commands-all-completions-function
  'all-completions
  "Function which should be equivalent to `all-completions'.")

(defvar minibuffer-commands-exact-match-p-function
  'minibuffer-commands-exact-match-p
  "Function to tell if an exact match exists in a completion table.")

(defun minibuffer-commands-exact-match-p (str table &optional pred)
  "Return t if an exact match for STR, satisfying PRED, exists in TABLE."
  (let ((ignore-case completion-ignore-case))
    (cond
     ((listp table)                     ; alist or nil
      (let ((assoc (funcall (if ignore-case 'assoc-ignore-case 'assoc)
                            str table)))
        (and assoc (or (null pred) (funcall pred assoc)))))

     ((vectorp table)                   ; obarray
      (if (and (or (string= str "nil")
                   (and ignore-case (string= (downcase str) "nil")))
               (eq table obarray))
          t                             ; We catch the `nil' here
        (let* ((lowercase-str (downcase str))
               (symbol (if (not ignore-case)
                           (intern-soft str table)
                         (catch 'found
                           (mapatoms
                            #'(lambda (s)
                                (when (string= lowercase-str
                                               (downcase (symbol-name s)))
                                  (throw 'found s)))
                            table)))))
          (and symbol (or (null pred) (funcall pred symbol))))))
     
     (t                                 ; programmed completion
      (funcall table str pred 'lambda)))))



;;; Utilities
(defun minibuffer-commands-message (str &optional sec)
  "Display STR at the end of the minibuffer for SEC (default 2) seconds.
The minibuffer must be the current buffer.
Stop displaying when the next input event arrives.
Work almost the same as `minibuffer-message'."
  (unless sec (setq sec 2))
  (let ((buffer-undo-list t)            ; prevent undo recording
        (pt-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (message nil)
      (goto-char (point-max))
      (insert str))
    (sit-for sec)
    (delete-region pt-max (point-max))
    (when quit-flag
      (let ((quit-char (if (fboundp 'current-input-mode)
                           (nth 3 (current-input-mode))
                         ?\^G))
            (char-to-event (if (fboundp 'character-to-event)
                               'character-to-event
                             'identity)))
        (cond
         ;; check new features first
         ((boundp 'unread-command-events)
          (setq unread-command-events (list (funcall char-to-event quit-char))
                quit-flag nil))
         ;;;((boundp 'unread-command-event)
         ;;; (setq unread-command-event (funcall char-to-event quit-char)
         ;;;       quit-flag nil))
         ;;;
         ;;;((boundp 'unread-command-char)
         ;;; (setq unread-command-char quit-char
         ;;;       quit-flag nil))
         (t
          (error "mcomplete-message: %S is not bound."
                 'unread-command-events)))))))

            

(defvar minibuffer-commands-last-exact-completion
  nil
  "Private variable to hold a state of `minibuffer-commands-do-completion'.")

(add-hook 'minibuffer-commands-minibuffer-setup-hook
          #'(lambda () (setq minibuffer-commands-last-exact-completion nil)))

(defun minibuffer-commands-do-completion ()
  "Perform completion in the minibuffer."
  (let* ((completion (funcall minibuffer-commands-try-completion-function
                              (buffer-string)
                              minibuffer-completion-table
                              minibuffer-completion-predicate))
         completed-p last)

    (setq last minibuffer-commands-last-exact-completion
          minibuffer-commands-last-exact-completion nil)

    (cond
     ((null completion)
      (minibuffer-commands-message " [No match]")
      0)                                ; 0: no possible completion
     ((eq completion t)
      1)                                ; 1: was already an exact and
                                        ;    unique completion
     (t
      (setq completed-p (> (length completion) (length (buffer-string))))
      (when completed-p
        (erase-buffer)
        (insert completion))
      (cond
       ((funcall minibuffer-commands-exact-match-p-function
                 (buffer-string)
                 minibuffer-completion-table
                 minibuffer-completion-predicate)
        (if (not completed-p)
            (progn
              (setq minibuffer-commands-last-exact-completion (buffer-string))
              (when (and last (equal last (buffer-string)))
                (minibuffer-commands-completion-help))
              3)                        ; 3: was already an exact completion,
                                        ;    but not unique.
          4))                           ; 4: completed to an exact completion,
                                        ;    but not unique.
       (completed-p                     ; 5: some completion happened,
        5)                              ;    but not to an exact completion.
       (t
        (if completion-auto-help
            (minibuffer-commands-completion-help)
          (minibuffer-commands-message " [Next char not unique]"))
        6))))))                         ; 6: no completion happened.
                                        ;    there're some possible completions
                                        ;    but the next char is not unique.


;;; Commands
(defun minibuffer-commands-complete ()
  "Emulate the original C primitive function in elisp."
  (interactive)
  (unless (eq last-command this-command)
    (setq minibuffer-scroll-window nil))
  (let* ((minibuffer        (current-buffer))
         (help-win-exists-p (and (windowp        minibuffer-scroll-window)
                                 (window-buffer  minibuffer-scroll-window)
                                 (buffer-name
                                  (window-buffer minibuffer-scroll-window))))
         status)
    (cond
     (help-win-exists-p
      (set-buffer (window-buffer minibuffer-scroll-window))
      (if (pos-visible-in-window-p (point-max) minibuffer-scroll-window)
          (set-window-start minibuffer-scroll-window (point-min))
        (scroll-other-window))
      (set-buffer minibuffer)
      nil)
     (t
      (setq status (minibuffer-commands-do-completion))
      (cond
       ((= status 0) nil)
       ((= status 1)
        (minibuffer-commands-message " [Sole completion]") t)
       ((= status 3)
        (minibuffer-commands-message " [Complete, but not unique]") t)
       (t t))))))


(defvar minibuffer-commands-complete-word-high-priority-strings
  (list " "  "-")
  "The default value should make `minibuffer-commands-complete-word' act like `minibuffer-complete-word'")


(defun minibuffer-commands-complete-word ()
  "Emulate the original C primitive function in elisp."
  (interactive)
  ;; Completion behavior of FSF Emacs's `minibuffer-complete-word'
  ;;
  ;; (completing-read "test: " '(("bm-emacs_something.el")))
  ;; <SPACE>
  ;; bm-<SPACE>
  ;; bm-emacs_<SPACE>
  ;; bm-emacs_something.<SPACE>
  ;; bm-emacs_something.el
  ;;
  ;; (completing-read "test: " '(("space wins") ("space_wins") ("space-wins") ("spacewinds")))
  ;; <SPACE>
  ;; "space<SPACE>"
  ;; "space <SPACE>"
  ;; "space wins"
  ;;
  ;; (completing-read "test: " '(("hyphen-wins") ("hyphen_wins")))
  ;; <SPACE>
  ;; hyphen<SPACE>
  ;; hyphen-<SPACE>
  ;; hyphen-wins
  ;;
  ;; (completing-read "test: " '(("can't_decide") ("can'tdecide")))
  ;; <SPACE>
  ;; can'<SPACE>
  ;; can't<SPACE>
  ;; [open help window]
  ;;
  (let* ((buf-str    (buffer-string))
         (completion (funcall minibuffer-commands-try-completion-function
                              buf-str
                              minibuffer-completion-table
                              minibuffer-completion-predicate))
         (suffix (when (stringp completion)
                   (substring completion (length buf-str)))))
    (cond
     ((null completion)
      (minibuffer-commands-message " [No match]") nil)
     ((eq completion t)
      (minibuffer-commands-message " [Sole completion]") nil)
     ((string= suffix "")
      (let ((strings minibuffer-commands-complete-word-high-priority-strings))
        (unless (catch 'inserted
                  (while strings
                    (when (funcall minibuffer-commands-try-completion-function
                                   (concat (buffer-string)
                                           (car strings))
                                   minibuffer-completion-table
                                   minibuffer-completion-predicate)
                      (goto-char (point-max))
                      (insert (car strings))
                      (throw 'inserted t))
                    (setq strings (cdr strings))))
          (if completion-auto-help
              (minibuffer-commands-completion-help)
            (minibuffer-commands-message " [Next char not unique]")))))
     ((string-match "\\`\\sw*\\Sw?" suffix)
      (goto-char (point-max))
      (insert (match-string 0 suffix))
      t)
     (t (error "minibuffer-commands-complete-word: logical error")))))


(defun minibuffer-commands-complete-and-exit ()
  "Emulate the original C primitive function in elisp."
  (interactive)
  (if (or (string= (buffer-string) "")
          (funcall minibuffer-commands-exact-match-p-function
                   (buffer-string)
                   minibuffer-completion-table
                   minibuffer-completion-predicate))
      (throw 'exit nil)
    (let ((status (minibuffer-commands-do-completion)))
      (cond
       ((or (= status 1) (= status 3))
        (throw 'exit nil))
       ((= status 4)
        (if (not minibuffer-completion-confirm)
            (throw 'exit nil)
          (minibuffer-commands-message " [Confirm]"))))
      nil)))


(defvar minibuffer-commands-completion-help-sort-function
  #'(lambda (completions) (sort completions 'string<)))

(defun minibuffer-commands-completion-help ()
  "Emulate the original C primitive function in elisp."
  (interactive)
  (message "Making completion list...")
  (let ((completions (funcall minibuffer-commands-all-completions-function
                              (buffer-string)
                              minibuffer-completion-table
                              minibuffer-completion-predicate)))
    (message nil)
    (cond
     ((null completions)
      (ding)
      (minibuffer-commands-message " [No completions]"))
     (t
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list
         (funcall minibuffer-commands-completion-help-sort-function
                  completions)))))))


;;; Hook
(defvar minibuffer-commands-load-hook nil
  "Hook to run at the end of loading minibuffer-commands.")

(provide 'minibuffer-commands)
(run-hooks 'minibuffer-commands-load-hook)

;;; minibuffer-commands.el ends here
