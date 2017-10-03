;;; bm-mb.el --- minibuffer completion extensions

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

;; Minibuffer completion commands and functions implemented in Emacs Lisp,
;; plus some extensions (substring completion, short descriptions of
;; possible completions in "*Completions*" buffer).

;; This library is for Emacs Lisp programmers; it doesn't contain any Emacs
;; commands for end users.

;; Tested environments
;;     FSF Emacs 20.6.2
;;     XEmacs 21.1.9

;; References
;;     "(emacs)Completion"
;;     "(elisp)Completion"

;; Why I wrote this library
;;     In FSF Emacs, almost all of the completion commands and functions
;;     described in "(elisp)Completion" are implemented in the C language
;;     (see minibuf.c).
;;
;;     These commands and functions call each other within C, without going
;;     through the normal Lisp function call sequences.
;;
;;     This means you can't tweak the behavior of a command in a Lisp way;
;;     by establishing local definitions of functions which are called by the
;;     command.


;; Correspondence
;;
;;  Functions
;;            FSF Emacs                           bm-mb.el
;;      completing-read                    bm-mb-completing-read
;;      * Those who want to read from the minibuffer with completion call
;;        this function.
;;
;;
;;  Keymaps
;;            FSF Emacs                           bm-mb.el
;;      minibuffer-local-completion-map    bm-mb-local-completion-map
;;      minibuffer-local-must-match-map    bm-mb-local-must-match-map
;;      * `completing-read' (or `bm-mb-completing-read') uses one of
;;        these keymaps when reading from the minibuffer.
;;
;;
;;  Commands
;;            FSF Emacs                           bm-mb.el
;;      minibuffer-complete                bm-mb-complete
;;      minibuffer-complete-word           bm-mb-complete-word
;;      minibuffer-complete-and-exit       bm-mb-complete-and-exit
;;      minibuffer-completion-help         bm-mb-completion-help
;;      * These commands are registered in the above keymaps and activated
;;        via specific key sequences while reading from the minibuffer.

;; Some hacks (display completions with their short descriptions)
;;   (Actually, this is the real reason I wrote this library.)
;;
;;   `display-completion-list' does all the job of displaying completions
;;   in the `*Completions*' buffer.
;;
;;   The argument to this function is normally a list of completions just
;;   returned by `all-completions'. But it can also be a list whose element
;;   is a list of two strings, which is printed as if the strings were 
;;   concatenated. Note this list can also be considered as an alist,
;;   thus, can be given to `completing-read'.
;;
;;   By setting `bm-mb-completion-help-with-descriptions' to non-nil,
;;   and giving `bm-mb-completing-read' an alist described above,
;;   `bm-mb-completion-help' displays completions with descriptions.
;;   e.g.
;;        (require 'bm-mb)
;;        (let ((bm-mb-completion-help-with-descriptions t))
;;          (bm-mb-completing-read "test: " '(("Item1" "    description1")
;;                                            ("Item2" "    description2")
;;                                            ("Item3" "    description3"))))

;; Memo
;;     Revision 1.6 resembles the standard functions more closely.
;;

;;; Code:
(eval-when-compile (require 'cl))


;;; Keymaps ----------------------------------------------------------
(defvar bm-mb-local-completion-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map "?"    'bm-mb-completion-help)
    (define-key map "\t"   'bm-mb-complete)
    (define-key map " "    'bm-mb-complete-word)
    map)
  "`minibuffer-local-completion-map' counterpart for bm-mb.el")


(defvar bm-mb-local-must-match-map
  (let ((map (copy-keymap minibuffer-local-must-match-map)))
    (define-key map "?"    'bm-mb-completion-help)
    (define-key map "\t"   'bm-mb-complete)
    (define-key map " "    'bm-mb-complete-word)
    (define-key map "\r"   'bm-mb-complete-and-exit)
    (define-key map "\n"   'bm-mb-complete-and-exit)
    map)
  "`minibuffer-local-must-match-map' counterpart for bm-mb.el")



;;; "*Completions*" buffer related stuff
(defvar bm-mb-completions-buffer-truncate-lines
  t
  "Non-nil means truncate lines in \"*Completions*\" buffer.")


(defun bm-mb-completions-buffer-truncate-lines ()
  "Hook function for `truncate-lines' option of \"*Completions*\" buffer."
  (save-excursion 
    (set-buffer standard-output)
    (setq truncate-lines bm-mb-completions-buffer-truncate-lines)))


;;; Entry point function to this library.
;;;###autoload
(defun bm-mb-completing-read (prompt collection
                                     &optional predicate require-match
                                     init history default inherit-input-method)
  (let* ((minibuffer-completion-table     collection)
         (minibuffer-completion-predicate predicate)
         (minibuffer-completion-confirm 
          ;; This variable is used in `bm-mb-complete-and-exit' only when
          ;; `REQUIRE-MATCH' is non-nil.
          (if (eq require-match 't) nil t))

         (bm-mb-substr-completion-flag bm-mb-substr-completion-flag)
         (bm-mb-show-descriptions-flag bm-mb-show-descriptions-flag)

         (bm-mb-last-exact-completion     nil)
         (keymap (if require-match
                     bm-mb-local-must-match-map
                   bm-mb-local-completion-map))
         input)

    (unwind-protect
        (progn
          (add-hook 'completion-setup-hook
                    'bm-mb-completions-buffer-truncate-lines
                    ;; Must be called after `completion-setup-function'.
                    "APPEND")
          (setq input (if (featurep 'xemacs)
                          (read-from-minibuffer prompt
                                                init
                                                keymap
                                                nil
                                                history)

                        (read-from-minibuffer prompt
                                              init
                                              keymap
                                              nil
                                              history
                                              default
                                              ;; default accessible in history
                                              ;; via M-n
                                              inherit-input-method)))
      (remove-hook 'completion-setup-hook
                   'bm-mb-completions-buffer-truncate-lines)))
         
    ;; default handling
    (or (and (string= input "") default)
        input)))


(defun bm-mb-message (string &optional sec)
  "Show STRING in the minibuffer for SEC seconds (default 2 sec.)."
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
        (let ((quit-c (cond
                       ((fboundp 'quit-char)
                        (quit-char))
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
                                         (funcall char-to-event quit-c))
                  quit-flag nil))
           ((boundp 'unread-command-event)
            (setq unread-command-event (funcall char-to-event quit-c)
                  quit-flag nil))
           ((boundp 'unread-command-char)
            (setq unread-command-char quit-c
                  quit-flag nil))
           (t)))))))
            

;;; The completion gang of three. ------------------------------------
;;
;; See "(elisp)Basic Completion".
;; 1: try-completion function
;; 2: all-completion function
;; 3: exact match function

(defvar bm-mb-substr-completion-flag
  nil
  "Non-nil means currently we're in the substring completion mode.
Otherwise, we're in the prefix completion mode (Emacs standard mode).")

(defvar bm-mb-show-descriptions-flag
  t
  "Non-nil means \"*Completions*\" buffer displays completions with descriptions.
Otherwise, without descriptions (Emacs standard behavior).")

(defvar bm-mb-add-descriptions-function
  nil
  "Function adding descriptions to the return value of `bm-mb-all-completions'.
")


(defun bm-mb-try-completion (string collection &optional predicate)
  (if (or (zerop (length string))
          (not bm-mb-substr-completion-flag)
          (functionp collection))
      (try-completion string collection predicate)
    (let* ((case-fold-search completion-ignore-case)
           (regexp (regexp-quote string))
           alist result)
      (cond
       ((or (consp collection) (null collection))
        (let ((rest collection))
          (while rest
            (when (string-match regexp (caar rest))
              (setq alist (cons (car rest) alist)))
            (setq rest (cdr rest))))
        (setq result (try-completion "" alist predicate)))
       ((vectorp collection)
        (mapatoms
         #'(lambda (symbol)
             (if (and (string-match regexp (symbol-name symbol))
                      (or (null predicate) (funcall predicate symbol)))
                 (setq alist (cons (list (symbol-name symbol)) alist))))
         collection)
        (setq result (try-completion "" alist))) ; without predicate
       (t (error "%s" "bm-mb-try-completions: logic error")))

      (if (and (stringp result) (not (string-match regexp result)))
          ""
        result))))


(defun bm-mb-exact-match-exists-p (string)
  "Return t if an exact match for STRING exists in `minibuffer-completion-table'.
Otherwise return nil."
  (let ((table       minibuffer-completion-table)
        (predicate   minibuffer-completion-predicate)
        (ignore-case completion-ignore-case))
    (cond
     ((or (consp table) (null table))
      (let ((assoc (funcall (if ignore-case 'assoc-ignore-case 'assoc)
                            string table)))
        (and assoc (or (null predicate) (funcall predicate (car assoc))))))
     ((vectorp table)
      (if (and (or (string= string "nil")
                   (and ignore-case (string= (downcase string) "nil")))
               (eq table obarray))
          ;; THE `nil' is a symbol whose name is "nil" in THE `obarray'.
          t
        (let* ((upcase-str (upcase string))
               (symbol (if (not ignore-case)
                           (intern-soft string table)
                         (catch 'found
                           (mapatoms
                            #'(lambda (s)
                                (if (string= upcase-str
                                             (upcase (symbol-name s)))
                                    (throw 'found s)))
                            table)))))
          (and symbol                   ; Only THE `nil' is false
               (or (null predicate) (funcall predicate symbol))))))
     (t                            ; programmed completion
      (funcall table string minibuffer-completion-predicate 'lambda)))))


(defun bm-mb-all-completions (string collection &optional predicate nospace)
  (if (or (string= string "")
          (not bm-mb-substr-completion-flag)
          (symbolp collection))
      (if (featurep 'xemacs)
          (all-completions string collection predicate)
        (all-completions string collection predicate nospace))
    (let ((case-fold-search completion-ignore-case)
          (regexp (regexp-quote string))
          list)
      (cond
       ((or (consp collection) (null collection))
        (let ((rest collection))
          (while rest
            (if (and (string-match regexp (caar rest))
                     (or (null predicate) (funcall predicate (car rest))))
                (setq list (cons (caar rest) list)))
            (setq rest (cdr rest)))))
       ((vectorp collection)
        (mapatoms
         #'(lambda (s)
             (if (and (string-match regexp (symbol-name s))
                      (or (null predicate) (funcall predicate s)))
                 (setq list (cons (symbol-name s) list))))
         collection))
       (t (error "%s" "bm-mb-all-completions: logic error")))
      list)))


(defvar bm-mb-last-exact-completion
  nil
  "Used by `bm-mb-do-completion'.")

(defun bm-mb-do-completion ()
  (let* ((completion (bm-mb-try-completion (buffer-string)
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate))
         completed-p last)

    (setq last bm-mb-last-exact-completion
          bm-mb-last-exact-completion nil)

    (cond
     ((null completion)
      (bm-mb-message " [No match]")
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
       ((bm-mb-exact-match-exists-p (buffer-string))
        (if (not completed-p)
            (progn
              (setq bm-mb-last-exact-completion (buffer-string))
              (when (and last (equal last (buffer-string)))
                (bm-mb-completion-help))
              3)                        ; 3: was already an exact completion,
                                        ;    but not unique.
          4))                           ; 4: completed to an exact completion,
                                        ;    but not unique.
       (completed-p                     ; 5: some completion happened,
        5)                              ;    but not to an exact completion.
       (t
        (if completion-auto-help
            (bm-mb-completion-help)
          (bm-mb-message " [Next char not unique]"))
        6))))))                         ; 6: no completion happened.
                                        ;    there're some possible completions
                                        ;    but the next char is not unique.



;;; commands registered in minibuffer keymaps ------------------------

(defun bm-mb-complete (prefix)
  (interactive "p")
  (if (/= prefix 1)
      (progn
        (setq minibuffer-scroll-window nil)
        (cond
         ((< (abs prefix) 0)
          (setq bm-mb-substr-completion-flag
                (y-or-n-p (format "Substring completion? (currently `%s') "
                                  (if bm-mb-substr-completion-flag
                                      "YES" "NO"))))
          (setq bm-mb-show-descriptions-flag
                (y-or-n-p (format "Show descriptions? (currently `%s') "
                                  (if bm-mb-show-descriptions-flag
                                      "YES" "NO")))))
         ((< prefix 5)
          (bm-mb-message (format " [Show descriptions: %s]"
                                 (if (setq bm-mb-show-descriptions-flag
                                           (not bm-mb-show-descriptions-flag))
                                     "YES" "NO"))))
         (t
          (bm-mb-message (format " [Substring completion: %s]"
                                 (if (setq bm-mb-substr-completion-flag
                                           (not bm-mb-substr-completion-flag))
                                     "YES" "NO"))))))
    (unless (eq last-command this-command)
      (setq minibuffer-scroll-window nil))
    (let* ((minibuffer (current-buffer))
           (win minibuffer-scroll-window)
           (help-win-exists-p (and (windowp win)
                                   (window-buffer win)
                                   (buffer-name (window-buffer win))))
           status)
      (cond
       (help-win-exists-p
        (set-buffer (window-buffer win))
        (if (pos-visible-in-window-p (point-max) win)
            (set-window-start win (point-min))
          (scroll-other-window))
        (set-buffer minibuffer)
        nil)
       (t
        (setq status (bm-mb-do-completion))
        (cond
         ((= status 0) nil)
         ((= status 1) (bm-mb-message " [Sole completion]") t)
         ((= status 3) (bm-mb-message " [Complete, but not unique]") t)
         (t t)))))))


(defvar bm-mb-complete-high-priority-strings
  '(" "  "-")
  "The default value should make `bm-mb-complete-word' act like `minibuffer-complete-word'")


(defun bm-mb-complete-word ()
  ;; Completion behavior of FSF Emacs's `minibuffer-complete-word'
  ;;
  ;; (completing-read "test: " '(("bm-emacs_something.el")))
  ;; (bm-mb-completing-read "test: " '(("bm-emacs_something.el")))
  ;; <SPACE>
  ;; bm-<SPACE>
  ;; bm-emacs_<SPACE>
  ;; bm-emacs_something.<SPACE>
  ;; bm-emacs_something.el
  ;;
  ;; (completing-read "test: " '(("space wins") ("space_wins") ("space-wins") ("spacewinds")))
  ;; (bm-mb-completing-read "test: " '(("space wins") ("space_wins") ("space-wins") ("spacewinds")))
  ;; <SPACE>
  ;; "space<SPACE>"
  ;; "space <SPACE>"
  ;; "space wins"
  ;;
  ;; (completing-read "test: " '(("hyphen-wins") ("hyphen_wins")))
  ;; (bm-mb-completing-read "test: " '(("hyphen-wins") ("hyphen_wins")))
  ;; <SPACE>
  ;; hyphen<SPACE>
  ;; hyphen-<SPACE>
  ;; hyphen-wins
  ;;
  ;; (completing-read "test: " '(("can't_decide") ("can'tdecide")))
  ;; (bm-mb-completing-read "test: " '(("can't_decide") ("can'tdecide")))
  ;; <SPACE>
  ;; can'<SPACE>
  ;; can't<SPACE>
  ;; [open help window]
  ;;
  (interactive)
  (let* ((buf-str    (buffer-string))
         (completion (try-completion buf-str
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (suffix (when (stringp completion)
                   (substring completion (length buf-str)))))
    (cond
     ((null completion)
      (bm-mb-message " [No match]") nil)
     ((eq completion t)
      (bm-mb-message " [Sole completion]") nil)
     ((string= suffix "")
      (loop for str in bm-mb-complete-high-priority-strings
            ;;;if (or (and (string= str "") ; ugly hack
            ;;;            (bm-mb-exact-match-exists-p (buffer-string)))
            ;;;       (and (not (string= str ""))
            ;;;            (try-completion (concat (buffer-string) str)
            ;;;                            minibuffer-completion-table
            ;;;                            minibuffer-completion-predicate)))
            if (try-completion (concat (buffer-string) str)
                               minibuffer-completion-table
                               minibuffer-completion-predicate)
            do (goto-char (point-max))
              and do (insert str)
              and return t
            end
            finally
            do (if completion-auto-help
                   (bm-mb-completion-help)
                 (bm-mb-message " [Next char not unique]"))))
     ((string-match "\\`\\sw*\\Sw?" suffix)
      (goto-char (point-max))
      (insert (match-string 0 suffix))
      t)
     (t (error "bm-mb-complete-word: logical error")))))


(defun bm-mb-complete-and-exit ()
  (interactive)
  (if (or (string= (buffer-string) "")
          (bm-mb-exact-match-exists-p (buffer-string)))
      (throw 'exit nil)
    (let ((status (condition-case err (bm-mb-do-completion) (error 1))))
      (cond
       ((or (= status 1) (= status 3))
        (throw 'exit nil))
       ((= status 4)
        (if (not minibuffer-completion-confirm)
            (throw 'exit nil)
          (bm-mb-message " [Confirm]"))))
      nil)))


(defun bm-mb-completion-help ()
  (interactive)
  (message "Making completion list...")
  (let ((completions (bm-mb-all-completions (buffer-string)
                                            minibuffer-completion-table
                                            minibuffer-completion-predicate)))
    (when (and bm-mb-show-descriptions-flag
               bm-mb-add-descriptions-function)
      (setq completions (funcall bm-mb-add-descriptions-function completions)))
    (message nil)
    (cond
     ((null completions)
      (ding)
      (bm-mb-message " [No completions]"))
     (t
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list
         (sort completions
               (if (consp (car completions))
                   #'(lambda (a b) (string< (car a) (car b)))
                 'string<)))))))
  nil)
    
      


;;; read functions ---------------------------------------------------
(defvar bm-mb-add-function-descriptions
  t)

(defun bm-mb-add-symbol-descriptions (completions)
  (let ((rest completions)
        (width 30)
        symbol doc doc1 sym-len sp-len)
    (while rest
      (setq symbol  (intern (car rest))
            doc     (or (if bm-mb-add-function-descriptions
                            (documentation symbol "VERBATIM")
                          (documentation-property symbol
                                                  'variable-documentation
                                                  "VERBATIM"))
                        "")
            sym-len (length (symbol-name symbol))
            sp      (if (< sym-len width)
                        (make-string (- width sym-len) ?\ )
                      (concat "\n" (make-string width ?\ )))
            doc1    (concat sp (progn (string-match "^.*$" doc)
                                      (match-string 0 doc))))
      (setcar rest (list (car rest) doc1))
      (setq rest (cdr rest)))
    completions))


(defun bm-mb-read-command (prompt &optional default)
  ""
  (let ((bm-mb-add-function-descriptions t)
        (bm-mb-add-descriptions-function 'bm-mb-add-symbol-descriptions))
    (bm-mb-completing-read prompt obarray 'commandp t nil nil default)))


(defun bm-mb-read-function (prompt &optional default)
  ""
  (let ((bm-mb-add-function-descriptions t)
        (bm-mb-add-descriptions-function 'bm-mb-add-symbol-descriptions))
    (bm-mb-completing-read prompt obarray 'functionp t nil nil default)))


(defun bm-mb-read-user-variable (prompt &optional default)
  ""
  (let ((bm-mb-add-function-descriptions nil)
        (bm-mb-add-descriptions-function 'bm-mb-add-symbol-descriptions))
    (bm-mb-completing-read prompt obarray 'user-variable-p t nil nil default)))


(provide 'bm-mb)

;;; bm-mb.el ends here
