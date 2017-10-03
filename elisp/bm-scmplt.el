;;; bm-scmplt.el --- substring completion mechanism in the minibuffer

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
;;
;;

;;; Code:

(defgroup bm-scmplt nil
  "Show completions dynamically in minibuffer."
  :prefix "bm-scmplt-"
  :group 'minibuffer)

;;;User Customization variables
(defcustom bm-scmplt-mode nil
  "Toggle incremental minibuffer completion.
As text is typed into the minibuffer, prospective completions are indicated 
in the minibuffer.
Setting this variable directly does not take effect;
use either \\[customize] or the function `bm-scmplt-mode'."
  :set (lambda (symbol value)
	 (bm-scmplt-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'bm-scmplt
  :require 'bm-scmplt)

(defcustom bm-scmplt-compute-delay .3
  "*Completions-computation stall, used only with large-number
completions - see `bm-scmplt-delay-completions-threshold'."
  :type 'number
  :group 'bm-scmplt)

(defcustom bm-scmplt-delay-completions-threshold 400
  "*Pending-completions number over which to apply bm-scmplt-compute-delay."
  :type 'integer
  :group 'bm-scmplt)

(defcustom bm-scmplt-max-delay-chars 3
  "*Maximum number of initial chars to apply bm-scmplt compute delay."
  :type 'integer
  :group 'bm-scmplt)

(defcustom bm-scmplt-show-key-bindings t
  "*If non-nil, show key bindings as well as completion for sole matches."
  :type 'boolean
  :group 'bm-scmplt)

(defcustom bm-scmplt-minibuffer-setup-hook nil
  "*Bm-Scmplt-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff bm-scmplt will be active.
It is intended for use in customizing bm-scmplt for interoperation
with other packages.  For instance:

  \(add-hook 'bm-scmplt-minibuffer-setup-hook
	    \(function
	     \(lambda ()
	       \(make-local-variable 'resize-minibuffer-window-max-height)
	       \(setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
icompletion is occurring."
  :type 'hook
  :group 'bm-scmplt)


;;; Initialization

(defvar bm-scmplt-eoinput 1
  "Point where minibuffer input ends and completion info begins.")
(make-variable-buffer-local 'bm-scmplt-eoinput)

(defvar bm-scmplt-pre-command-hook nil
  "Incremental-minibuffer-completion pre-command-hook.

Is run in minibuffer before user input when `bm-scmplt-mode' is non-nil.
Use `bm-scmplt-mode' function to set it up properly for incremental
minibuffer completion.")
(add-hook 'bm-scmplt-pre-command-hook 'bm-scmplt-tidy)

(defvar bm-scmplt-post-command-hook nil
  "Incremental-minibuffer-completion post-command-hook.

Is run in minibuffer after user input when `bm-scmplt-mode' is non-nil.
Use `bm-scmplt-mode' function to set it up properly for incremental
minibuffer completion.")
(add-hook 'bm-scmplt-post-command-hook 'bm-scmplt-exhibit)


;;;###autoload
(defun bm-scmplt-mode (&optional prefix)
  "Activate incremental minibuffer completion for this Emacs session.
Deactivates with negative universal argument."
  (interactive "p")
  (or prefix (setq prefix 0))
  (cond ((>= prefix 0)
	 (setq bm-scmplt-mode t)
	 ;; The following is not really necessary after first time -
	 ;; no great loss.
	 (add-hook 'minibuffer-setup-hook 'bm-scmplt-minibuffer-setup))
	(t (setq bm-scmplt-mode nil))))


(defun bm-scmplt-simple-completing-p ()
  "Non-nil if current window is minibuffer that's doing simple completion.

Conditions are:
   the selected window is a minibuffer,
   and not in the middle of macro execution,
   and minibuffer-completion-table is not a symbol (which would
       indicate some non-standard, non-simple completion mechanism,
       like file-name and other custom-func completions)."

  (and (window-minibuffer-p (selected-window))
       (not executing-kbd-macro)
       (not (symbolp minibuffer-completion-table))))


;;;###autoload
(defun bm-scmplt-minibuffer-setup ()
  "Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'."
  (cond ((and bm-scmplt-mode (bm-scmplt-simple-completing-p))
	 (make-local-hook 'pre-command-hook)
	 (add-hook 'pre-command-hook
		   (function (lambda ()
			       (run-hooks 'bm-scmplt-pre-command-hook)))
		   nil t)
	 (make-local-hook 'post-command-hook)
	 (add-hook 'post-command-hook
		   (function (lambda ()
			       (run-hooks 'bm-scmplt-post-command-hook)))
		   nil t)
	 (run-hooks 'bm-scmplt-minibuffer-setup-hook))))

;;; Completion


(defun bm-scmplt-tidy ()
  "Remove completions display \(if any) prior to new user input.
Should be run in on the minibuffer `pre-command-hook'.  See `bm-scmplt-mode'
and `minibuffer-setup-hook'."
  (if (bm-scmplt-simple-completing-p)
      (if (and (boundp 'bm-scmplt-eoinput)
	       bm-scmplt-eoinput)

	  (if (> bm-scmplt-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq bm-scmplt-eoinput (point-max))
	    (let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	      (delete-region bm-scmplt-eoinput (point-max))))

	;; Reestablish the local variable 'cause minibuffer-setup is weird:
	(make-local-variable 'bm-scmplt-eoinput)
	(setq bm-scmplt-eoinput 1))))


(defun bm-scmplt-exhibit ()
  "Insert bm-scmplt completions display.
Should be run via minibuffer `post-command-hook'.  See `bm-scmplt-mode'
and `minibuffer-setup-hook'."
  (if (bm-scmplt-simple-completing-p)
      (let ((contents (buffer-substring (point-min)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'bm-scmplt-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'bm-scmplt-eoinput))
	  (setq bm-scmplt-eoinput (point))
                                        ; Insert the match-status information:
	  (if (and (> (point-max) 1)
		   (or
		    ;; Don't bother with delay after certain number of chars:
		    (> (point-max) bm-scmplt-max-delay-chars)
		    ;; Don't delay if alternatives number is small enough:
		    (if minibuffer-completion-table
			(cond ((numberp minibuffer-completion-table)
			       (< minibuffer-completion-table
				  bm-scmplt-delay-completions-threshold))
			      ((sequencep minibuffer-completion-table)
			       (< (length minibuffer-completion-table)
				  bm-scmplt-delay-completions-threshold))
			      ))
		    ;; Delay - give some grace time for next keystroke, before
		    ;; embarking on computing completions:
		    (sit-for bm-scmplt-compute-delay)))
	      (insert-string
	       (bm-scmplt-completions contents
				      minibuffer-completion-table
				      minibuffer-completion-predicate
				      (not
				       minibuffer-completion-confirm))))))))


(defun bm-scmplt-all-completions (string collection &optional predicate)
  (let ((case-fold-search completion-ignore-case)
        (regexp (regexp-quote string))
        list)
    (cond
     ((null collection))                ; do nothing
     ((consp collection)
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
     (t (error "%s" "bm-scmplt-all-completions: logic error.")))
    list))


(defvar bm-scmplt-current-candidates '()
  "")

(defvar bm-scmplt-current-name nil
  "")

(defun bm-scmplt-get-current-candidates (name table predicate)
  "Return a current completion candidates."
  (if (and (stringp bm-scmplt-current-name)
           (string= bm-scmplt-current-name name))
      bm-scmplt-current-candidates
    (setq bm-scmplt-current-name name)
    (setq bm-scmplt-current-candidates (bm-scmplt-all-completions name
                                                                  table
                                                                  predicate))))

(defgroup bm-scmplt nil
  ""
  :group 'extensions
  :group 'convenience)


(defcustom bm-scmplt-use-fonts t
  "*Non-nil means use font-lock fonts for showing first match."
  :type 'boolean
  :group 'bm-scmplt)

(defun bm-scmplt-completions (name table predicate require-match)
  "Return the string that is displayed after the user's text.
Modified from `icomplete-completions'."
  (let ((candidates (bm-scmplt-get-current-candidates name table predicate))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
	first)

    (if (and bm-scmplt-use-fonts candidates)
        ;; font-lock
	(progn
	  (setq first (car candidates))
	  (setq first (format "%s" first))
	  (put-text-property 0 (length first) 'face
			     (if (= (length candidates) 1) 
				 'font-lock-comment-face
			       'font-lock-function-name-face)
			     first) 
	  (setq candidates  (cons first (cdr candidates)))))

    (cond ((null candidates) (format " %sNo match%s"
				open-bracket-determined
				close-bracket-determined))

	  ((null (cdr candidates))		;one match
	   (concat (if (and (> (length (car candidates))
			       (length name)))
		       (concat open-bracket-determined
			       ;; when there is one match, show the 
			       ;; matching buffer name in full
			       (car candidates)
			       close-bracket-determined)
		     "")
		   (if (not bm-scmplt-use-fonts) " [Matched]")))
	  (t				;multiple matches
	   (let* (
		  ;;(most (try-completion name table predicate))
		  (most nil)
		  (most-len (length most))
		  most-is-exact
		  first
		  (alternatives
		   (apply
		    (function concat)
		    (cdr (apply
			  (function nconc)
			  (mapcar '(lambda (com)
				     (if (= (length com) most-len)
					 ;; Most is one exact match,
					 ;; note that and leave out
					 ;; for later indication:
					 (progn
					   (setq most-is-exact t)
					   ())
				       (list ","
					     (substring com
							most-len))))
				  candidates))))))

	     (concat

	      ;; put in common completion item -- what you get by
	      ;; pressing tab
	      (if (> (length iswitchb-common-match-string) (length name))
		  (concat open-bracket-determined
			  (substring iswitchb-common-match-string 
				     (length name))
			  close-bracket-determined))
	      ;; end of partial matches...

	      ;; think this bit can be ignored.
	      (and (> most-len (length name))
		   (concat open-bracket-determined
			   (substring most (length name))
			   close-bracket-determined))
	      
	      ;; list all alternatives
	      open-bracket-prospects
	      (if most-is-exact
		  (concat "," alternatives)
		alternatives)
	      close-bracket-prospects))))))












(if bm-scmplt-mode
    (bm-scmplt-mode 1))








(provide 'bm-scmplt)

;;; bm-scmplt.el ends here
