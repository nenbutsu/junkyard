;;; sw-complete.el --- switch minibuffer completion methods on the fly

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

;; This package contains part of the code and the documentation of the
;; following packages:
;;
;;   icomplete.el by Ken Manheimer <klm@python.org>
;;   iswitchb.el  by Stephen Eglen <stephen@anc.ed.ac.uk>
;;
;; All of these are bundled with FSF Emacs and are distributed under GPL.
;; Thanks to the authors for writing these excellent packages.


;;; Commentary:

;; In short, this is icomplete.el + iswitchb.el's substring match
;; method +/- something.
;;
;; The command `M-x sw-complete-mode' enables you to switch minibuffer
;; completion methods while you're entering an argument in the minibuffer.
;; Completion candidates are displayed in the minibuffer.
;;
;; Currently supported methods are as follows:
;; * prefix-match method
;;   This is Emacs's default completion method.
;;
;;   e.g. "buffer" matches            "buffer-disable-undo",
;;                                    "buffer-enable-undo"
;;                                    "buffer-flush-undo", and
;;                                    "buffer-menu",
;;
;;        but doesn't match      "bury-buffer",
;;                            "display-buffer",
;;                               "kill-buffer-and-window", and
;;                                "minibuffer-complete".
;;
;; * substring-match method
;;   
;;

;; Implementation note:
;;
;; sw-complete-mode
;;   * is the switch function.
;;   * registers `sw-complete-minibuffer-setup' in `minibuffer-setup-hook'.
;;
;; sw-complete-minibuffer-setup
;;   * decides whether we should activate the sw-complete-mode's behavior
;;     or fall back to the normal behavior.
;;
;;   * is run via `minibuffer-setup-hook'
;;   * registers `sw-complete-pre-command-hook' in localized 
;;     `pre-command-hook'.
;;   * registers `sw-complete-post-command-hook' in localized
;;     `post-command-hook'.
;;   * runs sw-complete-minibuffer-setup-hook
;;
;; sw-complete-pre-command-tidy
;;   * is registered in `sw-complete-pre-command-hook' on load.
;;
;;
;; sw-complete-post-command-exhibit
;;   * is registered in `sw-complete-post-command-hook' on load.
;;


;;; Code:
(require 'minibuffer-commands)          ; minibuffer completion commands

;; Global variable declarations
(eval-when-compile
  (defvar deactivate-mark)
  (defvar icomplete-mode))


;;; Customization
(defgroup sw-complete nil
  "Switch minibuffer completion methods on the fly."
  :group 'minibuffer)

(defcustom sw-complete-mode nil
  "Toggle minibuffer completion method switching mode.

Setting this variable directly (e.g. (setq sw-complete-mode t)) does not
take effect; use either `\\[customize-group] sw-complete' or the commands
`sw-complete-mode', `sw-complete-mode-turn-on', and 
`sw-complete-mode-turn-off'."
  :set (lambda (symbol value)
	 (sw-complete-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'sw-complete
  :require 'sw-complete)

(defcustom sw-complete-minibuffer-setup-hook nil
  "*Hook for the sw-complete.el specific customization of the minibuffer setup.

This hook is run during minibuffer setup if sw-complete will be active.
It is intended for use in customizing sw-complete for interoperation
with other packages.  For instance:

    (add-hook 'sw-complete-minibuffer-setup-hook
              #'(lambda ()
                  (make-local-variable 'resize-minibuffer-window-max-height)
                  (setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
icompletion is occurring."
  :type 'hook
  :group 'sw-complete)

(defvar sw-complete-minibuffer-setup-internal-hook nil
  "Internal hook")


;;; Utilities
(defun sw-complete-rotate-list (list count)
  "\(sw-complete-rotate-list (list 1 2 3) 1) => (2 3 1).
\(sw-complete-rotate-list (list 1 2 3) -1) => (3 1 2)."
  (when list
    (let* ((len (length list))
           (count (mod count len))
           new-top new-last)
      (if (zerop count)
          list
        (setq new-last (nthcdr (1- count) list)
              new-top  (cdr new-last))
        (setcdr (last new-top) list)
        (setcdr new-last nil)
        new-top))))

;;; Completion method set utilities
(defvar sw-complete-method-set
  '(sw-complete-prefix-match-method
    sw-complete-substring-match-method)
  "List of all known completion method symbols for sw-complete.el package.")

(defvar sw-complete-current-method-set
  nil
  "List of completion method symbols for current minibuffer.
Users can activate these methods while they're in the minibuffer.")
(make-variable-buffer-local 'sw-complete-current-method-set)

(defvar sw-complete-current-method-set-init-function
  #'(lambda () (setq sw-complete-current-method-set
                     (apply 'list sw-complete-method-set)))
  "Function to initialize `sw-complete-current-method-set' for each minibuffer.")
(add-hook 'sw-complete-minibuffer-setup-internal-hook
          #'(lambda () (funcall sw-complete-current-method-set-init-function)))

(defun sw-complete-rotate-method-set (count-or-method)
  (run-hooks (sw-complete-get :method-leave-hook))

  (setq sw-complete-current-method-set
        (sw-complete-rotate-list
         sw-complete-current-method-set
         (if (integerp count-or-method)
             count-or-method
           (catch 'count
             (let ((list  sw-complete-current-method-set)
                   (count 0))
               (while list
                 (when (eq (car list) count-or-method)
                   (throw 'count count))
                 (setq count (1+ count)
                       list  (cdr list)))
               (error "Method symbol %S not found" count-or-method))))))

  (run-hooks (sw-complete-get :method-enter-hook)))

;;; Completion method utilities
(defun sw-complete-current-method ()
  "Return the completion method symbol for the current minibuffer."
  (car sw-complete-current-method-set))

(defun sw-complete-get (property &optional method)
  "Return PROPERTY of completion METHOD."
  (unless method (setq method (sw-complete-current-method)))
  (plist-get (symbol-value method) property))

(defcustom sw-complete-display-method-name-sec 0.5
  ""
  :type 'number
  :group 'sw-complete)

(defun sw-complete-display-method-name ()
  (unless (input-pending-p)
    (let* ((symbol-name (symbol-name (sw-complete-current-method)))
           (name (if (string-match "sw-complete-\\(.+\\)-method" symbol-name)
                     (match-string 1 symbol-name)
                   (error "sw-complete: Malformed method name %S"
                          symbol-name))))
      (minibuffer-commands-message (format " [%s]" name)
                                   sw-complete-display-method-name-sec))))

;;; Key bindings
(defcustom sw-complete-permissive-completion-map-alist
  '(("\t"   . SW-complete-complete)
    (" "    . SW-complete-complete-word)
    ("?"    . SW-complete-completion-help)
    ("\r"   . SW-complete-exit-minibuffer)
    ("\e\r" . exit-minibuffer)
    ("\n"   . SW-complete-exit-minibuffer)
    ("\e\n" . exit-minibuffer)
    ("\C-n" . SW-complete-next-method)
    ("\C-p" . SW-complete-previous-method)
    ("\C-s" . SW-complete-next-candidate)
    ("\C-r" . SW-complete-previous-candidate))
  "Alist of key bindings to override `minibuffer-local-completion-map'.
These bindings are used when an exact match is NOT required."
  :type  '(repeat
           (cons (choice string
                         (restricted-sexp :match-alternatives (vectorp)))
                 ;; the vector designator can be more elaborate (using
                 ;; :inline) but I found they were harder to input in
                 ;; the customization buffer.
                 function))
  :group 'sw-complete)

(defcustom sw-complete-must-match-completion-map-alist
  '(("\t"   . SW-complete-complete)
    (" "    . SW-complete-complete-word)
    ("?"    . SW-complete-completion-help)
    ("\r"   . SW-complete-complete-and-exit)
    ("\n"   . SW-complete-complete-and-exit)
    ("\C-n" . SW-complete-next-method)
    ("\C-p" . SW-complete-previous-method)
    ("\C-s" . SW-complete-next-candidate)
    ("\C-r" . SW-complete-previous-candidate))
  "Alist of key bindings to override `minibuffer-local-must-match-map'.
These bindings are used when an exact match is required."
  :type  '(repeat
           (cons (choice string
                         (restricted-sexp :match-alternatives (vectorp)))
                 function))
  :group 'sw-complete)

(defun sw-complete-bind-alist (map src-alist)
  ""
  (mapcar #'(lambda (assoc) (define-key map (car assoc) (cdr assoc)))
          src-alist))

(add-hook
 'sw-complete-minibuffer-setup-internal-hook
 #'(lambda ()
     (let* ((old-map (current-local-map))
            (new-map (copy-keymap old-map)))
       (use-local-map new-map)
       (sw-complete-bind-alist
        new-map
        (if (eq (lookup-key old-map "\n")
                (lookup-key minibuffer-local-completion-map "\n"))
            ;; For FSF Emacs, we can simply get away with (eq old-map
            ;; minibuffer-local-completion-map), but XEmacs makes a copy
            ;; and defines a help-key binding in it.
            sw-complete-permissive-completion-map-alist
          sw-complete-must-match-completion-map-alist)))))


;;; Pre/Post-Command hooks
(defvar sw-complete-pre-command-hook nil
  "Hook variable to be run via `pre-command-hook'.")
(add-hook 'sw-complete-pre-command-hook
          #'(lambda ()
              (run-hooks (sw-complete-get :pre-command-hook))))


(defvar sw-complete-post-command-hook nil
  "Hook variable to be run via `post-command-hook'.")
(add-hook 'sw-complete-post-command-hook
          #'(lambda ()
              (run-hooks (sw-complete-get :post-command-hook))))

(add-hook
 'sw-complete-minibuffer-setup-internal-hook
 #'(lambda ()
     ;; setup PRE-COMMAND-HOOK
     (make-local-hook 'pre-command-hook)
     (add-hook 'pre-command-hook
               #'(lambda () (run-hooks 'sw-complete-pre-command-hook))
               nil                      ; nil means prepend
               t)                       ; t means a local hook

     ;; setup POST-COMMAND-HOOK
     (make-local-hook 'post-command-hook)
     (add-hook 'post-command-hook
               #'(lambda () (run-hooks 'sw-complete-post-command-hook))
               nil                      ; nil means prepend
               t)))                     ; t means a local hook


;;; Caching facility for `all-completions' clones
(defvar sw-complete-all-completions-result nil
  "Result of the last invocation of the method specific `all-completions'.
This value is also manipulated by `SW-complete-next-candidate' and
`SW-complete-previous-candidate'.")
(make-variable-buffer-local 'sw-complete-all-completions-result)

(defvar sw-complete-all-completions-args nil
  "List of args of the last invocation of the method specific `all-completions'.")
(make-variable-buffer-local 'sw-complete-all-completions-args)

(defvar sw-complete-all-completions-method nil
  "Method of the last invocation of the method specific `all-completions'.")
(make-variable-buffer-local 'sw-complete-all-completions-method)

(defmacro sw-complete-all-completions-with-cache (args &rest body)
  "Provide the method specific `all-completions' BODY with chaching capability."
  `(cond
    ((and (eq sw-complete-all-completions-method (sw-complete-current-method))
          (equal ,args sw-complete-all-completions-args))
     sw-complete-all-completions-result)

    (t
     (setq sw-complete-all-completions-method (sw-complete-current-method)
           sw-complete-all-completions-args   ,args
           sw-complete-all-completions-result (progn ,@body)))))

(add-hook 'sw-complete-minibuffer-setup-internal-hook
          #'(lambda () (setq sw-complete-all-completions-result nil
                             sw-complete-all-completions-args   nil
                             sw-complete-all-completions-method nil)))

(put 'sw-complete-all-completions-with-cache 'lisp-indent-function 'defun)


;;; Mode switch
;;;###autoload
(defun sw-complete-mode (&optional arg)
  ""
  (interactive "P")
  (setq sw-complete-mode (if (null arg)
                             (not sw-complete-mode)
                           (> (prefix-numeric-value arg) 0)))
  (cond
   (sw-complete-mode
    (add-hook 'minibuffer-setup-hook 'sw-complete-minibuffer-setup)
    (when (and (boundp 'icomplete-mode) icomplete-mode)
      (icomplete-mode -1)))             ; turn off icomplete-mode

   (t
    (remove-hook 'minibuffer-setup-hook 'sw-complete-minibuffer-setup)))

  (when (interactive-p)
    (message "sw-complete-mode: %s" (if sw-complete-mode "ON" "OFF"))))


;;;###autoload
(defun sw-complete-mode-turn-on ()
  ""
  (interactive)
  (sw-complete-mode 1))

;;;###autoload
(defun sw-complete-mode-turn-off ()
  ""
  (interactive)
  (sw-complete-mode -1))


;;; Minibuffer setup hook
(defcustom sw-complete-makes-way-for-these-commands
  '(iswitchb-buffer
    iswitchb-buffer-other-window
    iswitchb-display-buffer
    iswitchb-buffer-other-frame)
  "List of commands which should not be interfered with by sw-complete-mode."
  :type  '(repeat function)
  :group 'sw-complete)
(make-variable-buffer-local 'sw-complete-makes-way-for-these-commands)

(defvar sw-complete-active nil
  "Non-nil means sw-complete-mode is active in the current minibuffer.")
(make-variable-buffer-local 'sw-complete-active)

(defun sw-complete-applicable-p ()
  "Return non-nil if sw-complete-mode is applicable to the current minibuffer."
  (setq sw-complete-active
        (and sw-complete-mode
             (window-minibuffer-p (selected-window))
             (not executing-kbd-macro)
             (not (symbolp minibuffer-completion-table))
             minibuffer-completion-table
             (not (member this-command
                          sw-complete-makes-way-for-these-commands)))))

;;;###autoload
(defun sw-complete-minibuffer-setup ()
  "Run in minibuffer on activation via `minibuffer-setup-hook'."
  (when (sw-complete-applicable-p)
    (run-hooks 'sw-complete-minibuffer-setup-internal-hook)
    (run-hooks 'sw-complete-minibuffer-setup-hook)))


;;; *Completions* buffer
(defadvice choose-completion-delete-max-match
  ;; The original function is defined in FSF Emacs's simple.el,
  ;; and XEmacs's list-mode.el.
  ;; (Actually, this advise is not necessary for FSF Emacs, currently.)
  ;; This function is called when a user press RET or clicks a mouse button
  ;; in the "*Completions*" buffer.
  (around sw-complete last activate compile preactivate)
  "Delete appropriate piece of input string in the current minibuffer."
  (if sw-complete-active
      (delete-region (point-min) (point-max))
    ad-do-it))


;;; Minibuffer commands
(defun SW-complete-complete ()
  "Complete the minibuffer contents as far as possible."
  (interactive)
  (funcall (sw-complete-get :complete)))

(defun SW-complete-complete-word ()
  "Complete the minibuffer contents by at most a single word."
  (interactive)
  (funcall (sw-complete-get :complete-word)))

(defun SW-complete-complete-and-exit ()
  "If the minibuffer contents is a valid completion then exit.
Otherwise try to complete it."
  (interactive)
  (funcall (sw-complete-get :complete-and-exit)))

(defun SW-complete-exit-minibuffer ()
  "Exit the minibuffer."
  (interactive)
  (funcall (sw-complete-get :exit-minibuffer)))


(defun SW-complete-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (interactive)
  (funcall (sw-complete-get :completion-help)))


(defun SW-complete-next-method (&optional arg)
  "Change the completion method to the next one."
  (interactive "p")
  (unless arg (setq arg 1))
  (sw-complete-rotate-method-set arg))

(defun SW-complete-previous-method (&optional arg)
  "Change the completion method to the previous one."
  (interactive "p")
  (unless arg (setq arg 1))
  (sw-complete-rotate-method-set (- arg)))

(defun SW-complete-next-candidate (&optional arg)
  "Change the default completion candidate to the next one."
  (interactive "p")
  (unless arg (setq arg 1))
  (setq sw-complete-all-completions-result
        (sw-complete-rotate-list sw-complete-all-completions-result arg)))

(defun SW-complete-previous-candidate (&optional arg)
  "Change the default completion candidate to the previous one."
  (interactive "p")
  (unless arg (setq arg 1))
  (setq sw-complete-all-completions-result
        (sw-complete-rotate-list sw-complete-all-completions-result (- arg))))


;;; default behavior
(defmacro sw-complete-with-method-environment (&rest body)
  `(let ((minibuffer-commands-try-completion-function
          (sw-complete-get :try-completion))
         (minibuffer-commands-all-completions-function
          (sw-complete-get :all-completions))
         (minibuffer-commands-completion-help-sort-function
          'identity))
     (progn ,@body)))

(defcustom sw-complete-default-complete-and-exit-picks-first-candidate t
  ""
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask" ask))
  :group 'sw-complete)

(defcustom sw-complete-default-complete-and-exit-skips-confirmation t
  ""
  :type  'boolean
  :group 'sw-complete)

(defun sw-complete-default-complete-and-exit ()
  (when sw-complete-default-complete-and-exit-picks-first-candidate
    (let* ((input          (buffer-string))
           (min-chars      (symbol-value
                            (sw-complete-get :exhibit-with-this-many-chars)))
           (candidate-list (when (>= (length input) min-chars)
                             (funcall (sw-complete-get :all-completions)
                                      input
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)))
           (candidate   (car candidate-list))
           (pick sw-complete-default-complete-and-exit-picks-first-candidate))
      (when (and (cdr candidate-list)
                 (or (eq pick t)
                     (prog1 (y-or-n-p (format "Complete to %S?: " candidate))
                       (message nil))))
        (erase-buffer)
        (insert candidate)
        (throw 'exit nil))))

  (let ((minibuffer-completion-confirm
         (if sw-complete-default-complete-and-exit-skips-confirmation
             nil
           minibuffer-completion-confirm)))
    (sw-complete-with-method-environment
     (minibuffer-commands-complete-and-exit))))

(defcustom sw-complete-default-exit-minibuffer-picks-first-candidate t
  ""
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask" ask))
  :group 'sw-complete)

(defun sw-complete-default-exit-minibuffer ()
  (when sw-complete-default-exit-minibuffer-picks-first-candidate
    (let* ((input     (buffer-string))
           (min-chars (symbol-value
                       (sw-complete-get :exhibit-with-this-many-chars)))
           (candidate (car (when (>= (length input) min-chars)
                             (funcall (sw-complete-get :all-completions)
                                      input
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate))))
           (pick sw-complete-default-exit-minibuffer-picks-first-candidate))
      (when (and candidate
                 (or (eq pick t)
                     (prog1 (y-or-n-p (format "Complete to %S?: " candidate))
                       (message nil))))
        (erase-buffer)
        (insert candidate))))
  (throw 'exit nil))

;;; Completion
(defvar sw-complete-pre-command-input ""
  "")
(make-variable-buffer-local 'sw-complete-pre-command-input)

(defvar sw-complete-post-command-input ""
  "")
(make-variable-buffer-local 'sw-complete-post-command-input)

(defun sw-complete-pre-command-tidy ()
  ""
  (remove-text-properties (point-min)
                          (+ (point-min)
                             (length sw-complete-post-command-input))
                          '(face))
  (funcall (sw-complete-get :pre-command-tidy))
  (setq sw-complete-pre-command-input (buffer-string))
  (set-buffer-modified-p nil))

(setq debug nil)

(defun sw-complete-post-command-exhibit ()
  ""
  (setq debug (cons (buffer-modified-p) debug))
  (setq sw-complete-post-command-input (buffer-string))
  (when (funcall (sw-complete-get :exhibit-now-p))
    (funcall (sw-complete-get :post-command-exhibit)))
  (when (and (string= sw-complete-pre-command-input
                      sw-complete-post-command-input)
             (boundp 'deactivate-mark))
    (setq deactivate-mark nil)))

(defun sw-complete-default-pre-command-tidy ()
  "Remove completions display \(if any) prior to new user input.
Should be run in on the minibuffer `pre-command-hook'.
See `sw-complete-mode' and `minibuffer-setup-hook'."
  (let ((buffer-undo-list t))         ; prevent undo recording
    (delete-region (+ (point-min) (length sw-complete-post-command-input))
                   (point-max))))

;;; ------------------------------------------------------------------
;;; prefix match method
;;; ------------------------------------------------------------------

(defvar sw-complete-prefix-match-method
  '(
    ;; Options
    :exhibit-with-this-many-chars
    sw-complete-prefix-match-exhibit-with-this-many-chars
    :exhibit-delay-sec
    sw-complete-prefix-match-exhibit-delay-sec
    :exhibit-no-delay-with-this-many-chars
    sw-complete-prefix-match-exhibit-no-delay-with-this-many-chars
    :exhibit-no-delay-commands
    sw-complete-prefix-match-exhibit-no-delay-commands
    :exhibit-unique-match-face
    sw-complete-prefix-match-exhibit-unique-match-face
    :exhibit-multiple-match-face
    sw-complete-prefix-match-exhibit-multiple-match-face

    ;; Core functions
    :all-completions      sw-complete-prefix-match-all-completions
    :try-completion       try-completion

    ;; Hooks
    :method-enter-hook sw-complete-prefix-match-method-enter-hook
    :method-leave-hook sw-complete-prefix-match-method-leave-hook

    :pre-command-hook  sw-complete-prefix-match-pre-command-hook
    :post-command-hook sw-complete-prefix-match-post-command-hook

    ;; Commands
    :complete             (lambda () (sw-complete-with-method-environment
                                      (minibuffer-commands-complete)))
    :complete-word        (lambda () (sw-complete-with-method-environment
                                      (minibuffer-commands-complete-word)))
    :completion-help      (lambda () (sw-complete-with-method-environment
                                      (minibuffer-commands-completion-help)))
    :complete-and-exit    sw-complete-default-complete-and-exit
    :exit-minibuffer      sw-complete-default-exit-minibuffer
                          
    ;; Candidates exhibition
    :exhibit-now-p        sw-complete-prefix-match-exhibit-now-p
    :pre-command-tidy     sw-complete-default-pre-command-tidy
    :post-command-exhibit sw-complete-prefix-match-exhibit
    :get-determined-part  sw-complete-prefix-match-get-determined-part
    :get-alternatives     sw-complete-prefix-match-get-alternatives)
  "Property list for the prefix match completion method.")
(add-to-list 'sw-complete-method-set 'sw-complete-prefix-match-method)


(defgroup sw-complete-prefix-match-method nil
  ""
  :group 'sw-complete)

(defcustom sw-complete-prefix-match-exhibit-with-this-many-chars 2
  ""
  :type  'integer
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-exhibit-delay-sec .3
  ""
  :type  'number
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-exhibit-no-delay-with-this-many-chars 3
  ""
  :type  'integer
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-exhibit-no-delay-commands
  '(SW-complete-next-candidate
    SW-complete-previous-candidate
    SW-complete-next-method
    SW-complete-previous-method)
  ""
  :type  '(repeat function)
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-exhibit-unique-match-face
  'font-lock-comment-face
  ""
  :type 'face
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-exhibit-multiple-match-face
  'font-lock-function-name-face
  ""
  :type 'face
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-switch-with-this-many-no-match-chars 1
  ""
  :type 'integer
  :group 'sw-complete-prefix-match-method)

(defcustom sw-complete-prefix-match-switch-to-this-method-on-no-match
  'sw-complete-substring-match-method
  ""
  :type 'symbol
  :group 'sw-complete-prefix-match-method)


(defvar sw-complete-prefix-match-method-enter-hook nil
  "")
(add-hook 'sw-complete-prefix-match-method-enter-hook
          'sw-complete-display-method-name)

(defvar sw-complete-prefix-match-method-leave-hook nil
  "")

(defvar sw-complete-prefix-match-pre-command-hook nil
  "")
(add-hook 'sw-complete-prefix-match-pre-command-hook
          'sw-complete-pre-command-tidy)


(defvar sw-complete-prefix-match-post-command-hook nil
  "")
(add-hook 'sw-complete-prefix-match-post-command-hook
          'sw-complete-post-command-exhibit)

(defun sw-complete-prefix-match-all-completions (string table
                                                        &optional predicate)
  ""
  (sw-complete-all-completions-with-cache (list string table predicate)
    (sort (all-completions string table predicate) 'string<)))

(defun sw-complete-prefix-match-exhibit-now-p ()
  ""
  (and (not (input-pending-p))
       (> (point-max) sw-complete-prefix-match-exhibit-with-this-many-chars)
       (or (> (point-max)
              sw-complete-prefix-match-exhibit-no-delay-with-this-many-chars)
           (member this-command
                   sw-complete-prefix-match-exhibit-no-delay-commands)
           (sit-for sw-complete-prefix-match-exhibit-delay-sec))))

(defun sw-complete-prefix-match-get-determined-part (input completions)
  ""
  (if (null completions)
      ""
    (let ((try (try-completion input (mapcar 'list completions))))
      (substring try (length input)))))

(defun sw-complete-prefix-match-get-alternatives (input completions)
  ""
  (let ((len (length input)))
    (mapcar #'(lambda (item) (substring item len)) completions)))

(defvar sw-complete-prefix-match-exhibit-no-match 0)
(make-variable-buffer-local 'sw-complete-prefix-match-exhibit-no-match)

(add-hook 'sw-complete-prefix-match-method-enter-hook
          #'(lambda () (setq sw-complete-prefix-match-exhibit-no-match 0)))

(defun sw-complete-prefix-match-exhibit ()
  (save-excursion
    (goto-char (point-max))
    (let* ((buffer-undo-list t)         ; prevent undo recording
           (input        (buffer-string))
           (completions  (funcall (sw-complete-get :all-completions)
                                  input
                                  minibuffer-completion-table
                                  minibuffer-completion-predicate)))
      (cond
       ((null completions)
        (unless (string= sw-complete-pre-command-input
                         sw-complete-post-command-input)
          (setq sw-complete-prefix-match-exhibit-no-match
                (1+ sw-complete-prefix-match-exhibit-no-match)))
        (if (> sw-complete-prefix-match-exhibit-no-match
               sw-complete-prefix-match-switch-with-this-many-no-match-chars)
            (insert " [No match]")
          (sw-complete-rotate-method-set
           sw-complete-prefix-match-switch-to-this-method-on-no-match)
          (funcall
           (sw-complete-get
            :post-command-exhibit
            sw-complete-prefix-match-switch-to-this-method-on-no-match))))

       ((and (null (cdr completions)) (string= (car completions) input))
        (setq sw-complete-prefix-match-exhibit-no-match 0)
        (put-text-property 1 (point-max)
                           'face
                           sw-complete-prefix-match-exhibit-unique-match-face)
        (insert " [Sole completion]"))

       (t
        (setq sw-complete-prefix-match-exhibit-no-match 0)
        (let* ((determined (funcall (sw-complete-get :get-determined-part)
                                    input
                                    completions))
               (alternatives (funcall (sw-complete-get :get-alternatives)
                                      (concat input determined)
                                      completions))
               (face
                (if (cdr alternatives)
                    sw-complete-prefix-match-exhibit-multiple-match-face
                  sw-complete-prefix-match-exhibit-unique-match-face)))
          (when alternatives
            (put-text-property 1 (point-max) 'face face)
            (put-text-property 0 (length (car alternatives))
                               'face face (car alternatives))
            (when determined
              (put-text-property 0 (length determined)
                                 'face face determined)))
          (insert (concat
                   (if (zerop (length determined))
                       ""
                     (concat "[" determined "]"))
                   (if (or (null alternatives)
                           (and (null (cdr alternatives))
                                (string= (car alternatives) "")))
                       ""
                     (concat "{"
                             (mapconcat 'identity 
                                        alternatives
                                        ",")
                             "}"))))))))))

;;; ------------------------------------------------------------------
;;; substring match method
;;; ------------------------------------------------------------------
(defvar sw-complete-substring-match-method
  '(
    ;; Options
    :exhibit-with-this-many-chars
    sw-complete-substring-match-exhibit-with-this-many-chars
    :exhibit-delay-sec
    sw-complete-substring-match-exhibit-delay-sec
    :exhibit-no-delay-with-this-many-chars
    sw-complete-substring-match-exhibit-no-delay-with-this-many-chars
    :exhibit-no-delay-commands
    sw-complete-substring-match-exhibit-no-delay-commands
    :exhibit-unique-match-face
    sw-complete-substring-match-exhibit-unique-match-face
    :exhibit-multiple-match-face
    sw-complete-substring-match-exhibit-multiple-match-face
    
    ;; Core functions
    :try-completion       sw-complete-substring-match-try-completion
    :all-completions      sw-complete-substring-match-all-completions

    ;; Hooks
    :method-enter-hook    sw-complete-substring-match-method-enter-hook
    :method-leave-hook    sw-complete-substring-match-method-leave-hook

    :pre-command-hook     sw-complete-substring-match-pre-command-hook
    :post-command-hook    sw-complete-substring-match-post-command-hook

    ;; Commands
    :complete             sw-complete-substring-match-complete
    :complete-word        sw-complete-substring-match-complete-word
    :completion-help      (lambda () (sw-complete-with-method-environment
                                      (minibuffer-commands-completion-help)))
    :complete-and-exit    sw-complete-default-complete-and-exit
    :exit-minibuffer      sw-complete-default-exit-minibuffer
                          
    ;; Candidates exhibition
    :exhibit-now-p        sw-complete-substring-match-exhibit-now-p
    :pre-command-tidy     sw-complete-default-pre-command-tidy
    :post-command-exhibit sw-complete-substring-match-exhibit
    :get-determined-part  sw-complete-substring-match-get-determined-part
    :get-alternatives     sw-complete-substring-match-get-alternatives)
  "Property list for the substring match completion method.")

(defgroup sw-complete-substring-match-method nil
  ""
  :group 'sw-complete)

(defcustom sw-complete-substring-match-exhibit-with-this-many-chars 1
  ""
  :type  'integer
  :group 'sw-complete-substring-match-method)

(defcustom sw-complete-substring-match-exhibit-delay-sec .3
  ""
  :type  'number
  :group 'sw-complete-substring-match-method)

(defcustom sw-complete-substring-match-exhibit-no-delay-with-this-many-chars 3
  ""
  :type  'integer
  :group 'sw-complete-substring-match-method)

(defcustom sw-complete-substring-match-exhibit-no-delay-commands
  '(SW-complete-next-candidate
    SW-complete-previous-candidate
    SW-complete-next-method
    SW-complete-previous-method)
  ""
  :type  '(repeat function)
  :group 'sw-complete-substring-match-method)

(defcustom sw-complete-substring-match-exhibit-unique-match-face
  'font-lock-comment-face
  ""
  :type 'face
  :group 'sw-complete-substring-match-method)

(defcustom sw-complete-substring-match-exhibit-multiple-match-face
  'font-lock-function-name-face
  ""
  :type 'face
  :group 'sw-complete-substring-match-method)

(defvar sw-complete-substring-match-pre-command-hook nil
  "")
(add-hook 'sw-complete-substring-match-pre-command-hook
          'sw-complete-pre-command-tidy)

(defvar sw-complete-substring-match-post-command-hook nil
  "")
(add-hook 'sw-complete-substring-match-post-command-hook
          'sw-complete-post-command-exhibit)

(defvar sw-complete-substring-match-method-enter-hook nil
  "")
(add-hook 'sw-complete-substring-match-method-enter-hook
          'sw-complete-display-method-name)

(defvar sw-complete-substring-match-method-leave-hook nil
  "")

(defun sw-complete-substring-match-exhibit-now-p ()
  ""
  (and (not (input-pending-p))
       (> (point-max) sw-complete-substring-match-exhibit-with-this-many-chars)
       (or 
        (> (point-max)
           sw-complete-substring-match-exhibit-no-delay-with-this-many-chars)
        (member
         this-command
         sw-complete-substring-match-exhibit-no-delay-commands)
        (sit-for sw-complete-substring-match-exhibit-delay-sec))))

(defun sw-complete-substring-match-all-completions (string table
                                                           &optional predicate)
  ""
  (sw-complete-all-completions-with-cache (list string table predicate)
    (let ((case-fold-search completion-ignore-case)
          (regexp (regexp-quote string))
          list)
      (cond
       ((listp table)                   ; alist (or nil)
        (let ((rest table))
          (while rest
            (if (and (string-match regexp (caar rest))
                     (or (null predicate) (funcall predicate (car rest))))
                (setq list (cons (caar rest) list)))
            (setq rest (cdr rest))))
        (sort list 'string<))

       ((vectorp table)                 ; obarray
        (mapatoms
         #'(lambda (s)
             (if (and (string-match regexp (symbol-name s))
                      (or (null predicate) (funcall predicate s)))
                 (setq list (cons (copy-sequence (symbol-name s)) list))))
         table)
        (sort list 'string<))

       (t (error "argument TABLE should be either alist or vector type."))))))


(defun sw-complete-substring-match-try-completion (string table
                                                          &optional predicate)
  ""
  (let ((completions (sw-complete-substring-match-all-completions string
                                                                  table
                                                                  predicate)))
    (cond
     ((null completions)
      nil)

     ((null (cdr completions))
      (if (string= (car completions) string)
          t
        (car completions)))

     (t
      (let* ((regexp (regexp-quote string))
             (tails-alist (mapcar #'(lambda (item)
                                      (string-match regexp item)
                                      (list (substring item (match-end 0))))
                                  completions))
             (try (try-completion "" tails-alist)))
        (concat string try))))))


(defun sw-complete-substring-match-complete ()
  ""
  (sw-complete-with-method-environment
   (minibuffer-commands-complete)))


(defun sw-complete-substring-match-complete-word ()
  ""
  (sw-complete-with-method-environment
   (minibuffer-commands-complete-word)))


(defun sw-complete-substring-match-get-determined-part (input completions)
  ""
  (let* ((regexp (regexp-quote input))
         (alist  (mapcar
                  #'(lambda (item)
                      (string-match regexp item)
                      (list (substring item (match-end 0))))
                  completions)))
    (when alist
      (let ((try (try-completion "" alist)))
        (when (stringp try) try)))))


(defun sw-complete-substring-match-get-alternatives (input completions)
  ""
  (mapcar 'copy-sequence completions))

(defun sw-complete-substring-match-exhibit ()
  (save-excursion
    (goto-char (point-max))
    (let* ((buffer-undo-list t)         ; prevent undo recording
           (input        (buffer-string))
           (completions  (funcall (sw-complete-get :all-completions)
                                  input
                                  minibuffer-completion-table
                                  minibuffer-completion-predicate)))
      (cond
       ((null completions)
        (insert " [No match]"))

       ((and (null (cdr completions)) (string= (car completions) input))
        (put-text-property
         1 (point-max)
         'face
         sw-complete-substring-match-exhibit-unique-match-face)
        (insert " [Sole completion]"))

       (t
        (let* ((determined   (funcall (sw-complete-get :get-determined-part)
                                      input
                                      completions))
               (alternatives (funcall (sw-complete-get :get-alternatives)
                                      (concat input determined)
                                      completions))
               (u-face sw-complete-prefix-match-exhibit-unique-match-face)
                  
               (m-face sw-complete-prefix-match-exhibit-multiple-match-face))
          (when alternatives
            (put-text-property 1 (point-max) 'face u-face)
            (when determined
              (put-text-property 0 (length determined)
                                 'face u-face determined))
            (put-text-property 0 (length (car alternatives))
                               'face m-face (car alternatives))
            (string-match (regexp-quote (concat input determined))
                          (car alternatives))
            (put-text-property (match-beginning 0) (match-end       0)
                               'face u-face (car alternatives)))
          (insert (concat
                   (if (zerop (length determined))
                       ""
                     (concat "[" determined "]"))
                   (if (or (null alternatives)
                           (and (null (cdr alternatives))
                                (string= (car alternatives) "")))
                       ""
                     (concat "{"
                             (mapconcat 'identity 
                                        alternatives
                                        ",")
                             "}"))))))))))
               



(add-to-list 'sw-complete-method-set 'sw-complete-substring-match-method)

;;; completing-help.el support
(if (featurep 'completing-help)
    (add-to-list 'completing-help-commands 'SW-complete-completion-help)
  (add-hook 'completing-help-load-hook
            #'(lambda () (add-to-list 'completing-help-commands 
                                      'SW-complete-completion-help))))

(provide 'sw-complete)

;;; sw-complete.el ends here
