;;; bm-hcmplt.el --- facility to display descriptions of completions

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

;; While entering an argument to a command which supports completion
;; in the minibuffer, by pressing `?', you see possible completions
;; of what you've entered so far. This is one of the standard features
;; of Emacs.
;;
;; With this library, you can see not only possible completions, but
;; also their descriptions.
;;
;; This library explicitly supports a certain number of standard
;; commands, and be able to infer, to some extent, what is appropriate
;; as descriptions, based on knowledge of the type of information you
;; are entering, so your custom commands or other standard commands
;; might work right off the bat without requiring any configuration
;; on your part.


;; "Completion" in the minibuffer:

;; Some commands ask you to enter a name of something (e.g. a file
;; name to visit, a command name to execute, etc.) in the minibuffer
;; (which sits at the bottom line of Emacs.)
;;
;; If those commands support Emacs standard "completion" mechanism,
;; you can get Emacs to guess what you want to enter.
;;
;; Halfway entering the information, you press <TAB> to give Emacs
;; a cue to guess and then complete what you've already entered in
;; the minibuffer.
;;
;; Emacs tries to fill in the missing part of your input as far as
;; possible, but sometimes Emacs can't be sure what you want to enter
;; because there are some possible alternatives. In that case, Emacs
;; displays those alternatives ("completions" in Emacs jargon) in a
;; help window hoping you will give it further clues.
;; 
;; If you press `?' instead of <TAB>, Emacs immediately displays
;; completions in a help window without doing completion in the
;; minibuffer.
;;
;; See "(emacs)Completion" ("Completion" section of the GNU Emacs manual)
;; for the canonical explanation.


;; Typical scenario:

;; #1: Use with `M-x' (execute-extended-command)      see "(emacs)M-x"
;;
;;     When you want to invoke a command which is not bound to any
;;     key, (note this might imply that you're not that familiar with
;;     the command), you enter `M-x <COMMAND-NAME-YOU-WANT-TO-INVOKE>'.
;;
;;     While entering <COMMAND-NAME-YOU-WANT-TO-INVOKE> part, you
;;     can use completion. In the following description, we use 
;;     `apropos' command as an example.
;;     
;;     You enter `M-x aprop<TAB>'. => Emacs completes it to "apropos".
;;     You enter <TAB>             => Emacs says "Complete, but not unique".
;;
;;     You've learned that there're some commands whose name start with
;;     "apropos", and wonder what they are?
;;     
;;     You enter `?'               => Something like the following is 
;;                                    displayed in a help window.
;;
;;     ----------------------------------------------------------------
;;     Click mouse-2 on a completion to select it.                     
;;     In this buffer, type RET to select the completion near point.   
;;                                                                     
;;     Possible completions are:                                       
;;     apropos 
;;       <C-h C-a>                             
;;       Show all bound symbols whose names match REGEXP.
;;       With optional prefix ARG or if `apropos-do-all' is non-nil, a$
;;       symbols and key bindings, which is a little more time-consumi$
;;       Returns list of symbols and documentation found.
;;                                        
;;     apropos-command 
;;       <C-h a, menu-bar help-menu describe apropos-commands>
;;       Show commands (interactively callable functions) that match R$
;;       With optional prefix ARG, or if `apropos-do-all' is non-nil, $
;;       noninteractive functions.             
;;       If VAR-PREDICATE is non-nil, show only variables, and only th$
;;       satisfy the predicate VAR-PREDICATE.  
;;                                        
;;     apropos-documentation                                      
;;       Show symbols whose documentation contain matches for REGEXP.
;;     -----------------------------------------------------------------
;;       *note* If the frame of your Emacs is wider than 79 columns, you 
;;              probably see the whole descriptions without truncation.
;;
;;     You can either,
;;       click on an item (usually the middle button of your mouse),
;;       go into the help window (M-v) then select an item (<RET>),
;;       scroll the help window (M-C-v),
;;       or continue to enter in the minibuffer.
;;     See "(emacs)Completion" for other key bindings.


;; Requirement/compatibility:

;; Tested with FSF Emacs 20.6.2 and XEmacs 21.1.9.
;;
;; This library works with `icomplete.el' and `complete.el'
;; (both of which also enhance the completion mechanism.)
;;
;; I added support for, and tested with the following standard commands.
;;     bookmark-jump            (C-x r b)
;;     customize-group          (M-x customize-group, also on the menu-bar)
;;     customize-option         (M-x customize-option, also on the menu-bar)
;;     describe-function        (C-h f)
;;     describe-variable        (C-h v)
;;     execute-extended-command (M-x)
;;     find-function            (M-x find-function)
;;     find-variable            (M-x find-variable)
;;     setenv                   (M-x setenv)
;;     where-is                 (C-h w)
;;
;; Other commands might work with this library automagically.


;; Install:

;; * Put this file in one of the directories listed in `load-path'.
;;   You can see the contents of `load-path' by entering
;;   `M-x customize-option <RET> load-path'.
;;
;; * Enter `M-x byte-compile-file <RET>
;;          <DIR-YOU-PUT-THIS-FILE-IN>/bm-hcmplt.el <RET>'
;;   to byte-compile this file.
;;
;; * Put the following lines in your .emacs file.
;;
;;   (autoload 'bm-hcmplt-mode "bm-hcmplt"
;;             "Activate a facility to display descriptions of completions."
;;             t nil)
;;   (autoload 'bm-hcmplt-mode-turn-on "bm-hcmplt"
;;             "Turn on a facility to display descriptions of completions."
;;             t nil)
;;   (autoload 'bm-hcmplt-mode-turn-off "bm-hcmplt"
;;             "Turn off a facility to display descriptions of completions."
;;             t nil)
;;
;; * Restart Emacs or enter `M-x load-library <RET> bm-hcmplt'.


;; Activation:

;; * Enter `M-x bm-hcmplt-mode-turn-on' to activate this library.
;; * Enter `M-x bm-hcmplt-mode-turn-off' to deactivate this library.


;; Customization:

;; * Enter `M-x customize-group <RET> bm-hcmplt' to customize this library.
;;   You might need to enter `M-x load-library <RET> bm-hcmplt' in advance.


;; How this library works:

;; The key idea is to give `display-completion-list' a piece of "advice"
;; to display descriptions of completions.
;; I used "defadvice" facility of Emacs Lisp to do this.
;; See "(elisp)Advising Functions".
;;
;; The argument to `display-completion-list' is normally a list of
;; completions just returned by `all-completions'.
;; But it can also be a list whose element is a list of two strings, 
;; which is printed as if the strings were concatenated.
;;
;; Our "advice" takes the argument to `display-completion-list' then
;; tries to add appropriate descriptions, using
;; `bm-hcmplt-predicate-to-description-function-alist'.
;;
;;
;; When `display-completion-list' is called, this library's "advice" to
;; the function intercepts the call, then...
;;
;; 1: Calls the original `display-completion-list' unless the current
;;    value of `this-command' is a `member' of `bm-hcmplt-display-commands'.
;;
;; 2: Runs predicate functions which are the `car' of each element of
;;    `bm-hcmplt-predicate-to-description-function-alist' which looks like
;;       ((predicate1 . description-function1)
;;        (predicate2 . description-function2)
;;        ... ).
;;
;;    If a predicate returns non-nil, stop calling the predicates, 
;;    and then goto 3.
;;
;;    Calls the original `display-completion-list' after all of
;;    the predicates return `nil'.
;;
;; 3: Calls the description-function whose associated predicate returned
;;    non-nil, with the original argument to `display-completion-list'.
;;
;; 4: Calls the original `display-completion-list' with the return value
;;    of the description function.
;;


;;; Change Log:

;; Version 1.41 (18 May 2000):
;;  * Added `bm-hcmplt-get-keys' to display command key bindings, which
;;      was stolen from Ken Manheimer's icomplete.el.

;; Version 1.39 (18 May 2000):
;;  * Fixed some comment error.
;;  * Added `bm-hcmplt-load-hook'

;; Version 1.36 (16 May 2000):
;;  * Added some documentation.
;;  * Added support for `bookmark-jump', `customize-group', 
;;      `customize-option', `setenv', and 2 types of simply-structured alists.
;;  * Changed to display the whole documentation of ELisp objects.
;;  * Added a customization group for the default formatting mechanism.

;; Version 1.6 (06 May 2000):
;;  * Fixed `bm-hcmplt-function-wanted-p' and `bm-hcmplt-variable-wanted-p'
;;      to work properly.

;; Version 1.5 (06 May 2000):
;;  * First public release of this package.


;;; Code:

;;; customization ====================================================
(defgroup bm-hcmplt nil
  "Facility to display descriptions of completions.
While entering an argument to a command which supports completion
in the minibuffer, by pressing `?', you see possible completions
of what you've entered so far. This is one of the standard features
of Emacs.

With this library, you can see not only possible completions, but
also their descriptions.

This library explicitly supports a certain number of standard
commands, and be able to infer, to some extent, what is appropriate
as descriptions, based on knowledge of the type of information you
are entering, so your custom commands or other standard commands
might work right off the bat without requiring any configuration
on your part."
  :prefix "bm-"
  :group 'minibuffer)


(defcustom bm-hcmplt-mode nil
  "Toggle a facility to display descriptions of completions.
Setting this variable directly does not take effect;
use either \\[customize] or the commands `bm-hcmplt-mode', 
`bm-hcmplt-mode-turn-on', and `bm-hcmplt-mode-turn-off'."
  :set        #'(lambda (symbol value) (bm-hcmplt-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type       'boolean
  :group      'bm-hcmplt
  :require    'bm-hcmplt)


(defcustom bm-hcmplt-display-commands
  '(minibuffer-completion-help PC-completion-help)
  "Minibuffer commands which should display descriptions of completions.
The default commands are usually activated via `?' or `M-?' in the minibuffer.
You should specify commands implemented in ELisp which call
`display-completion-list' inside them.
If you are an XEmacs user and want to see descriptions when you enter
<TAB>, add `minibuffer-complete'."
  :type '(repeat function)
  :group 'bm-hcmplt)


(defgroup bm-hcmplt-default-format nil
  "Default format used to display descriptions of completions."
  :group 'bm-hcmplt)

(defcustom bm-hcmplt-completions-buffer-truncate-lines t
  "Non-nil means `truncate-lines' option is set for the completions buffer."
  :type  'boolean
  :group 'bm-hcmplt-default-format)


(defcustom bm-hcmplt-description-start-column 2
  "Column position at which all description lines start."
  :type  'number
  :group 'bm-hcmplt-default-format)


(defvar bm-hcmplt-min-line-width 40
  "Long enough width to prevent `display-completion-list' from item-cramming.
The default value is slightly larger than the one hardcoded in minibuf.c.")


(defcustom bm-hcmplt-description-delimiter
  (make-string bm-hcmplt-min-line-width ?\ )
  "Used to separate each record of completions."
  :type       'string
  :set        #'(lambda (symbol value)
                  (let* ((right-space-num (- bm-hcmplt-min-line-width
                                            (length value)))
                         (right-spaces (when (> right-space-num 0)
                                         (make-string right-space-num ?\ ))))
                    (set symbol (concat value right-spaces))))
  :initialize 'custom-initialize-default
  :group      'bm-hcmplt-default-format
  :require    'bm-hcmplt)


(defvar bm-hcmplt-predicate-to-description-function-alist
  '()
  "Alist which is the core of bm-hcmplt-mode.
Used in \"advice\" to `display-completion-list'.
Usually looks like the following:
    ((predicate1 . description-function1)
     (predicate2 . description-function2)
     ... )
")

;;; modification functions for
;;; `bm-hcmplt-predicate-to-description-function-alist'
(defun bm-hcmplt-add-pred-and-func (pred func)
  "Register PRED and FUNC in `bm-hcmplt-predicate-to-description-function-alist'.
Search is done using PRED as a key.
Replace old function associated with PRED with FUNC if PRED is already
registered.
Cons (PRED . FUNC) onto the rest if PRED is not registered."
  (let ((assoc (assoc pred bm-hcmplt-predicate-to-description-function-alist)))
    (if assoc
        (setcdr assoc func)
      (setq bm-hcmplt-predicate-to-description-function-alist
            (cons (cons pred func)
                  bm-hcmplt-predicate-to-description-function-alist)))))


(defun bm-hcmplt-delete-pred-and-func (pred)
  "Delete an association of PRED from `bm-hcmplt-predicate-to-description-function-alist'."
  (setq bm-hcmplt-predicate-to-description-function-alist
        (delete nil
                (mapcar
                 #'(lambda (assoc)
                     (if (eq (car assoc) pred)
                         nil
                       assoc))
                 bm-hcmplt-predicate-to-description-function-alist))))


;;;###autoload
(defun bm-hcmplt-mode (&optional prefix)
  "Activate a facility to display descriptions of completions.
Deactivate with negative universal argument.
This behavior is the same as that of `icomplete-mode'."
  (interactive "p")
  (or prefix (setq prefix 0))
  (cond
   ((>= prefix 0)
    (setq bm-hcmplt-mode t))
   (t (setq bm-hcmplt-mode nil))))


;;;###autoload
(defun bm-hcmplt-mode-turn-on ()
  "Turn on a facility to display descriptions of completions."
  (interactive)
  (bm-hcmplt-mode 1))


;;;###autoload
(defun bm-hcmplt-mode-turn-off ()
  "Turn off a facility to display descriptions of completions."
  (interactive)
  (bm-hcmplt-mode -1))


(defvar bm-hcmplt-completion-setup-hook
  nil
  "")

(add-hook 'bm-hcmplt-completion-setup-hook
          #'(lambda ()                  ; set/reset truncate-line option
              (with-current-buffer standard-output
                (setq truncate-lines
                      bm-hcmplt-completions-buffer-truncate-lines))))


;;; advice ===========================================================
(eval-when-compile                      ; suppress compiler warnings
  (defvar completion-highlight-first-word-only))

(defadvice display-completion-list (around bm-hcmplt activate preactivate)
  "Advice to add descriptions of completions."
  (if (or (not bm-hcmplt-mode)
          (not (member this-command bm-hcmplt-display-commands)))
      ad-do-it
    (let ((completion-highlight-first-word-only
           (when (boundp 'completion-highlight-first-word-only)
             completion-highlight-first-word-only)) ; for xemacs
          (completion-setup-hook (append completion-setup-hook nil))

          ;; create a playground
          (bm-hcmplt-completions-buffer-truncate-lines
           bm-hcmplt-completions-buffer-truncate-lines)
          (bm-hcmplt-description-start-column
           bm-hcmplt-description-start-column)
          (bm-hcmplt-min-line-width bm-hcmplt-min-line-width)
          (bm-hcmplt-description-delimiter bm-hcmplt-description-delimiter)
          
          (alist bm-hcmplt-predicate-to-description-function-alist)
          (completions (ad-get-arg 0)))
      (catch 'done
        (while alist
          (when (funcall (car (car alist)))
            (setq completion-highlight-first-word-only t)
            (add-hook 'completion-setup-hook
                      #'(lambda ()
                          (run-hooks 'bm-hcmplt-completion-setup-hook))
                      "APPEND AFTER COMPLETION-SETUP-FUNCTION")
            (ad-set-arg 0 (funcall (cdr (car alist)) completions))
            (throw 'done t))
          (setq alist (cdr alist))))
      ad-do-it)))


(defadvice minibuffer-completion-help (around bm-hcmplt activate preactivate)
  "Advice to emulate the original C function in Emacs Lisp.
We need to call `display-completion-list' within Emacs Lisp."
  (message "Making completion list...")
  (let ((completions (all-completions (buffer-string)
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)))
    (message nil)
    (if (null completions)
        ;; Sole purpose of this ad-do-it is to display " [No completions]"
        ad-do-it
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list (sort completions 'string<))))))


(defvar bm-hcmplt-completing-read-command
  nil
  "Command which is directly/indirectly calling `completing-read'.")

(defvar bm-hcmplt-completing-read-args
  nil
  "List of arguments to the current invocation of `completing-read'")

(defadvice completing-read (around bm-hcmplt activate preactivate)
  "Set the caller of `completing-read' to `bm-hcmplt-completing-read-caller'"
  (let ((bm-hcmplt-completing-read-command this-command)
        (bm-hcmplt-completing-read-args (ad-get-args 0)))
    ad-do-it))



;;; Utility functions to add descriptions ----------------------------
(defmacro bm-hcmplt-split-string (doc)
  `(split-string ,doc "[\f\n\r\v]+"))

(defun bm-hcmplt-format-description (item &optional desc &rest desc-rest)
  "Return a formatted string of DESC and DESC-REST.
Peculiarity of `display-completion-list':
 (display-completion-list
    ((\"Item1\" \"Description1\") (\"Item2\" \"Description2\")))

 * An item and its description is displayed like \"Item1Description1\".
 * A description shouldn't end with newline to prevent the indentation of 
   the following lines from getting messy.
 * (length (concat \"Item1\" \"Description\")) should be greater than
   35 .. 40 to prevent 2 \"ItemDescriptions\"s from getting crammed in
   the same line."
  (let* ((left-space      (make-string bm-hcmplt-description-start-column ?\ ))
         (right-space-num (- bm-hcmplt-min-line-width
                            bm-hcmplt-description-start-column
                            (length desc)))
         (formatted-desc  (concat
                           (if (> bm-hcmplt-description-start-column
                                  (length item))
                               (make-string
                                (- bm-hcmplt-description-start-column
                                   (length item)) ?\ )
                             (when (not (zerop (length desc)))
                               (concat " \n" left-space)))
                           desc
                           (when (> right-space-num 0)
                             (make-string right-space-num ?\ )))))
    (while desc-rest
      (setq right-space-num (- bm-hcmplt-min-line-width
                               bm-hcmplt-description-start-column
                               (length (car desc-rest)))

            formatted-desc  (concat formatted-desc
                                    "\n" left-space
                                    (car desc-rest)
                                    (when (> right-space-num 0)
                                      (make-string right-space-num ?\ )))

            desc-rest (cdr desc-rest)))

    (setq formatted-desc (concat formatted-desc
                                 "\n"
                                 bm-hcmplt-description-delimiter))))

(defun bm-hcmplt-add-descriptions (completions get-formatted-doc-func)
  "Change each element of COMPLETIONS to a list of the original element and its description.
Return COMPLETIONS."
  (let ((rest completions)
        item-str)
    (while rest
      (setq item-str (car rest))
      (setcar rest (list item-str (funcall get-formatted-doc-func item-str)))
      (setq rest (cdr rest)))
    completions))


;;; alist ((str . str) (str . str) ...) support ----------------------
(defun bm-hcmplt-alist-str-dot-str-p ()
  (and (consp minibuffer-completion-table)
       (stringp (cdar minibuffer-completion-table))))

(defun bm-hcmplt-add-alist-str-dot-str-descriptions (completions)
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (key)
       (bm-hcmplt-format-description
        key (bm-hcmplt-split-string
             (cdr (assoc key minibuffer-completion-table)))))))
  
(bm-hcmplt-add-pred-and-func 'bm-hcmplt-alist-str-dot-str-p
                             'bm-hcmplt-add-alist-str-dot-str-descriptions)


;;; alist ((str str ...) (str str ...) ...) support ------------------
(defun bm-hcmplt-alist-str-pair-p ()
  (and (consp minibuffer-completion-table)
       (consp (cdar minibuffer-completion-table))
       (stringp (car (cdar minibuffer-completion-table)))))

(defun bm-hcmplt-add-alist-str-pair-descriptions (completions)
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (key)
       (bm-hcmplt-format-description
        key (bm-hcmplt-split-string
             (car (cdr (assoc key minibuffer-completion-table))))))))
  
(bm-hcmplt-add-pred-and-func 'bm-hcmplt-alist-str-pair-p
                             'bm-hcmplt-add-alist-str-pair-descriptions)


;;; Emacs Lisp functions support -------------------------------------
(defvar bm-hcmplt-function-predicates
  '(byte-code-function-p commandp functionp subrp fboundp)
  "Set of predicates identifying various kinds of Emacs Lisp functions.")


(defun bm-hcmplt-function-wanted-p ()
  "Return t if the current minibuffer session seems to want a ELisp function."
  (member minibuffer-completion-predicate bm-hcmplt-function-predicates))


(defun bm-hcmplt-get-keys (func-name)
  "Return strings naming keys bound to `func-name', or nil if none.
I borrowed icomplete-get-keys from icomplete.el by Ken Manheimer.
If we're in the minibuffer, check the keymap of (other-buffer),
otherwise check that of (current-buffer).
STOLEN FROM icomplete.el"
  (if (commandp func-name)
    (save-excursion
      (let* ((sym (intern func-name))
	     (buf (if (active-minibuffer-window)
                      (if (featurep 'xemacs) ; sigh.
                          (other-buffer nil nil "VISIBLE-OK")
                        (other-buffer nil "VISIBLE-OK"))
                    (current-buffer)))
	     (map (save-excursion (set-buffer buf) (current-local-map)))
	     (keys (where-is-internal sym map)))
	(if keys
	    (concat "<"
		    (mapconcat 'key-description
			       (sort keys
				     #'(lambda (x y)
					 (< (length x) (length y))))
			       ", ")
		    ">"))))))

(defun bm-hcmplt-add-function-descriptions (completions)
  "Add descriptions to COMPLETIONS which is full of ELisp function names."
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (sym-name)
       (let ((doc (or (documentation (intern sym-name)) "")))
         (apply 'bm-hcmplt-format-description
                sym-name
                (bm-hcmplt-get-keys sym-name)
                (bm-hcmplt-split-string doc))))))


(bm-hcmplt-add-pred-and-func 'bm-hcmplt-function-wanted-p
                             'bm-hcmplt-add-function-descriptions)



;;; Emacs Lisp variables support -------------------------------------
(defvar bm-hcmplt-variable-predicates
  '(user-variable-p boundp)
  "Set of predicates identifying various kinds of Emacs Lisp variables.")


(defvar bm-hcmplt-variable-completing-read-commands
  '(customize-option     customize-option-other-window
    customize-variable   customize-variable-other-window))


(defun bm-hcmplt-variable-wanted-p ()
  "Return t if the current minibuffer session seems to want a ELisp variable."
  (or
   (member minibuffer-completion-predicate bm-hcmplt-variable-predicates)
   (member bm-hcmplt-completing-read-command
           bm-hcmplt-variable-completing-read-commands)))


(defun bm-hcmplt-add-variable-descriptions (completions)
  "Add descriptions to COMPLETIONS which is full of ELisp variable names."
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (sym-name)
       (let ((doc (or (documentation-property (intern sym-name)
                                              'variable-documentation)
                      "")))
         (apply 'bm-hcmplt-format-description
                sym-name
                (bm-hcmplt-split-string doc))))))


(bm-hcmplt-add-pred-and-func 'bm-hcmplt-variable-wanted-p
                             'bm-hcmplt-add-variable-descriptions)



;;; Emacs Lisp custom groups support ---------------------------------
(defvar bm-hcmplt-custom-group-completing-read-commands
  '(customize-group customize-group-other-window))


(defun bm-hcmplt-custom-group-wanted-p ()
  ""
  (member bm-hcmplt-completing-read-command
          bm-hcmplt-custom-group-completing-read-commands))


(defun bm-hcmplt-add-custom-group-descriptions (completions)
  ""
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (sym-name)
       (let ((doc (or (get (intern sym-name) 'group-documentation) "")))
         (apply 'bm-hcmplt-format-description
                sym-name
                (bm-hcmplt-split-string doc))))))


(bm-hcmplt-add-pred-and-func 'bm-hcmplt-custom-group-wanted-p
                             'bm-hcmplt-add-custom-group-descriptions)



;;; Emacs Lisp custom faces support -----------------------------------
(eval-when-compile                      ; suppress compiler warnings
  (autoload 'face-property "faces"))

(defvar bm-hcmplt-custom-face-predicates
  '(custom-facep find-face)
  "")

(defun bm-hcmplt-custom-face-wanted-p ()
  ""
  (or (member minibuffer-completion-predicate bm-hcmplt-custom-face-predicates)
      (eq (nth 5 bm-hcmplt-completing-read-args) 'face-history)))


(defun bm-hcmplt-add-custom-face-descriptions (completions)
  ""
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (sym-name)
       (let* ((symbol (intern sym-name))
              (doc (or (if (fboundp 'face-property) ; for xemacs
                           (face-property (find-face symbol) 'doc-string)
                         (get symbol 'face-documentation))
                       "")))
         (apply 'bm-hcmplt-format-description
                sym-name
                (bm-hcmplt-split-string doc))))))


(bm-hcmplt-add-pred-and-func 'bm-hcmplt-custom-face-wanted-p
                             'bm-hcmplt-add-custom-face-descriptions)



;;; Bookmark.el support ----------------------------------------------
(eval-when-compile (require 'bookmark))
(defun bm-hcmplt-bookmark-wanted-p ()
  ""
  (and (boundp 'bookmark-alist)
       (eq minibuffer-completion-table bookmark-alist)))

(defun bm-hcmplt-add-bookmark-descriptions (completions)
  ""
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (bookmark-name)
       (apply 'bm-hcmplt-format-description
              bookmark-name
              (bookmark-get-filename bookmark-name)
              (bookmark-get-annotation bookmark-name)))))

(bm-hcmplt-add-pred-and-func 'bm-hcmplt-bookmark-wanted-p
                             'bm-hcmplt-add-bookmark-descriptions)

;;; env.el support ---------------------------------------------------
(defun bm-hcmplt-environment-variable-wanted-p ()
  (eq bm-hcmplt-completing-read-command 'setenv))

(defun bm-hcmplt-add-environment-variable-descriptions (completions)
  (bm-hcmplt-add-descriptions
   completions
   #'(lambda (env-name)
       (bm-hcmplt-format-description env-name
                                     (concat "=" (getenv env-name))))))

(bm-hcmplt-add-pred-and-func 'bm-hcmplt-environment-variable-wanted-p
                             'bm-hcmplt-add-environment-variable-descriptions)



;;; Load Hook
(defvar bm-hcmplt-load-hook nil
  "Hook to run at the end of loading bm-hcmplt.")

(run-hooks 'bm-hcmplt-load-hook)


(provide 'bm-hcmplt)

;;; bm-hcmplt.el ends here
