;;; bm-mb-ad.el --- advice to emulate minibuffer completion commands

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
;; advice to emulate minibuffer completion commands.


;;; Code:
(eval-when-compile (require 'advice))

;;;###autoload
(defun bm-mb-ad-activate-all (&optional compile)
  "Activate all pieces of advice defined in bm-mb-ad.el."
  (interactive (list current-prefix-arg))
  (ad-activate-regexp "\\`bm-mb-ad\\'" compile))


;;;###autoload
(defun bm-mb-ad-deactivate-all ()
  "Deactivate all pieces of advice defined in bm-mb-ad.el."
  (interactive)
  (ad-deactivate-regexp "\\`bm-mb-ad\\'"))


(defun bm-mb-ad-message (string &optional sec)
  "Show STRING in the minibuffer for SEC seconds (default 2 sec.)"
  (unless sec (setq sec 2))
  (let ((pt-max (point-max))
        (buffer-undo-list t))           ; prevent recording
    (goto-char (point-max))
    (message nil)
    (insert string)
    (goto-char pt-max)
    (let ((inhibit-quit t))
      (sit-for sec)
      (delete-region pt-max (point-max))
      (when quit-flag
        (let ((quit-char (cond
                          ((fboundp 'current-input-mode)
                           (nth 3 (current-input-mode)))
                          (t ?\^G)))
              (char-to-event (if (fboundp 'character-to-event)
                                 'character-to-event
                               'identity)))
          (cond
           ;; check new features first
           ((boundp 'unread-command-events)
            (setq unread-command-events (list
                                         (funcall char-to-event quit-char))
                  quit-flag nil))
;            ((boundp 'unread-command-event)
;             (setq unread-command-event (funcall char-to-event quit-char)
;                   quit-flag nil))
;            ((boundp 'unread-command-char)
;             (setq unread-command-char quit-char
;                   quit-flag nil))
           (t)))))))
            

(defun bm-mb-ad-exact-match-exists-p (string table predicate)
  "Return t if an exact match for STRING exists in `minibuffer-completion-table'.
Otherwise return nil."
  (unless (string= string "")
    (let ((ignore-case completion-ignore-case))
      (cond
       ((or (consp table) (null table)) ; alist
        (let ((assoc (funcall (if ignore-case 'assoc-ignore-case 'assoc)
                              string table)))
          (and assoc (or (null predicate) (funcall predicate (car assoc))))))

       ((vectorp table)                 ; obarray
        (if (and (or (string= string "nil")
                     (and ignore-case (string= (downcase string) "nil")))
                 (eq table obarray))
            t                           ; We catch the `nil' here
          (let* ((upcase-str (upcase string))
                 (symbol (if (not ignore-case)
                             (intern-soft string table)
                           (catch 'found
                             (mapatoms
                              #'(lambda (s)
                                  (when (string= upcase-str
                                                 (upcase (symbol-name s)))
                                    (throw 'found s)))
                              table)))))
            (and symbol
                 (or (null predicate)
                     (funcall predicate symbol))))))
       (t                               ; programmed completion
        (funcall table string predicate 'lambda))))))


(defvar bm-mb-ad-last-exact-completion
  nil
  "Used in `bm-mb-ad-do-completion'.")

(defun bm-mb-ad-do-completion ()
  (let* ((completion (try-completion (buffer-string)
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         completed-p last)

    (setq last bm-mb-ad-last-exact-completion
          bm-mb-ad-last-exact-completion nil)

    (cond
     ((null completion)
      (bm-mb-ad-message " [No match]")
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
       ((bm-mb-ad-exact-match-exists-p (buffer-string)
                                       minibuffer-completion-table
                                       minibuffer-completion-predicate)
        (if (not completed-p)
            (progn
              (setq bm-mb-ad-last-exact-completion (buffer-string))
              (when (and last (equal last (buffer-string)))
                (minibuffer-completion-help))
              3)                        ; 3: was already an exact completion,
                                        ;    but not unique.
          4))                           ; 4: completed to an exact completion,
                                        ;    but not unique.
       (completed-p                     ; 5: some completion happened,
        5)                              ;    but not to an exact completion.
       (t
        (if completion-auto-help
            (minibuffer-completion-help)
          (bm-mb-ad-message " [Next char not unique]"))
        6))))))                         ; 6: no completion happened.
                                        ;    there're some possible completions
                                        ;    but the next char is not unique.



;;; Advice
(defadvice minibuffer-complete (around bm-mb-ad last
                                       activate compile preactivate)
  "Emulate the original C primitive function in elisp."
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
      (setq status (bm-mb-ad-do-completion))
      (cond
       ((= status 0) nil)
       ((= status 1) (bm-mb-ad-message " [Sole completion]") t)
       ((= status 3) (bm-mb-ad-message " [Complete, but not unique]") t)
       (t t))))))


(defvar bm-mb-ad-complete-word-high-priority-strings
  '(" "  "-")
  "The default value should make `bm-mb-ad-complete-word' act like `minibuffer-complete-word'")


(defadvice minibuffer-complete-word (around bm-mb-ad last
                                            activate compile preactivate)
  "Emulate the original C primitive function in elisp."
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
         (completion (try-completion buf-str
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (suffix (when (stringp completion)
                   (substring completion (length buf-str)))))
    (cond
     ((null completion)
      (bm-mb-ad-message " [No match]") nil)
     ((eq completion t)
      (bm-mb-ad-message " [Sole completion]") nil)
     ((string= suffix "")
      (let ((strings bm-mb-ad-complete-word-high-priority-strings))
        (unless (catch 'inserted
                  (while strings
                    (when (try-completion (concat (buffer-string)
                                                  (car strings))
                                          minibuffer-completion-table
                                          minibuffer-completion-predicate)
                      (goto-char (point-max))
                      (insert (car strings))
                      (throw 'inserted t))
                    (setq strings (cdr strings))))
          (if completion-auto-help
              (minibuffer-completion-help)
            (bm-mb-ad-message " [Next char not unique]")))))
     ((string-match "\\`\\sw*\\Sw?" suffix)
      (goto-char (point-max))
      (insert (match-string 0 suffix))
      t)
     (t (error "bm-mb-ad-complete-word: logical error")))))


(defadvice minibuffer-complete-and-exit (around bm-mb-ad last
                                                activate compile preactivate)
  "Emulate the original C primitive function in elisp."
  (if (or (string= (buffer-string) "")
          (bm-mb-ad-exact-match-exists-p (buffer-string)
                                         minibuffer-completion-table
                                         minibuffer-completion-predicate))
      (throw 'exit nil)
    (let ((status (condition-case err (bm-mb-ad-do-completion) (error 1))))
      (cond
       ((or (= status 1) (= status 3))
        (throw 'exit nil))
       ((= status 4)
        (if (not minibuffer-completion-confirm)
            (throw 'exit nil)
          (bm-mb-ad-message " [Confirm]"))))
      nil)))


(defadvice minibuffer-completion-help (around bm-mb-ad last
                                              activate compile preactivate)
  "Emulate the original C primitive function in elisp."
  (message "Making completion list...")
  (let ((completions (all-completions (buffer-string)
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)))
    (message nil)
    (cond
     ((null completions)
      (ding)
      (bm-mb-ad-message " [No completions]"))
     (t
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list
         (sort completions 'string<)))))
  nil))



(provide 'bm-mb-ad)

;;; bm-mb-ad.el ends here
