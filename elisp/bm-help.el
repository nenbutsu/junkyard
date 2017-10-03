;;; bm-help.el --- enhancements to standard `help.el'

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: tools

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
;; enhancements to standard `help.el'

;;; Code:

(unless (featurep 'xemacs)

;;; Bound some keys in help-map

  ;; help-char (\C-h) + "\C-A" -> apropos
  (let* ((key "\C-A")
	 (old (lookup-key help-map key))
	 (new 'apropos))
    (cond
     ((eq old new));; do nothing
     ((null old)
      (define-key help-map key new)
      (message "bm-help: Bound %s to `%s' in `help-map'" key new))
     (t
      (error "bm-help: %s in help-map is already bound to %s." key old))))


  ;; help-char (\C-h) + "A" -> apropos-variable
  (let* ((key "A")
	 (old (lookup-key help-map key))
	 (new 'apropos-variable))
    (cond
     ((eq old new));; do nothing
     ((null old)
      (define-key help-map key new)
      (message "bm-help: Bound %s to `%s' in `help-map'" key new))
     (t
      (error "bm-help: %s in help-map is already bound to %s." key old))))


     

;;; Override standard help-for-help (\C-h \C-h) to provide categorized help 
;;; message. see help.el
  (eval-when-compile (require 'help-macro))
  (make-help-screen help-for-help
		    "a b c C f F C-f i I k C-k l L m n p s t v w C-c C-d C-n C-p C-w; ? for help:"
		    "You have typed %THIS-KEY%, the help character.  Type a Help option:
\(Use SPC or DEL to scroll through this text.  Type \\<help-map>\\[help-quit] to exit the Help command.)


Info
  i      info.
         The GNU info documentation reader.

  C-i    info-lookup-symbol.
         Display the definition of a specific symbol as found in the manual
         for the language this buffer is written in.


Emacs Lisp library
  p      finder-by-keyword.
         Find packages matching a given topic keyword.


Emacs Lisp symbol
  C-a    apropos
         Give a regexp, and see a list of symbols that match that regexp.
         *note* non-standard binding

  a      apropos-command.
         search only command.
         With prefix argument, search all functions (not only commands).

  A      apropos-variable
         search only user options (veriables for users to set).
         With prefix argument, search all variables.
         *note* non-standard binding

  f      describe-function.
         Type a function name and get documentation of it.

  C-f    Info-goto-emacs-command-node.
         Type a function name; it takes you to the Info node for that command.

  v      describe-variable.
         Type name of a variable;
         it displays the variable's documentation and value.


Key bindings
  b      describe-bindings.
         Display table of all key bindings.

  c      describe-key-briefly.
         Type a command key sequence;
	 it prints the function name that sequence runs.

  k      describe-key.
         Type a command key sequence; it displays the full documentation.

  C-k    Info-goto-emacs-key-command-node.
         Type a command key sequence;
	 it takes you to the Info node for the command bound to that key.

  w      where-is.
         Type command name; it prints which keystrokes invoke that command.


I18N
  C      describe-coding-system.
         This describes either a specific coding system
         (if you type its name) or the coding systems currently in use
	 (if you type just RET).

  I      describe-input-method.
         Describe a specific input method (if you type its name) or 
         the current input method (if you type just RET).

  L      describe-language-environment.
         This describes either the a specific language environment
         (if you type its name) or the current language environment
         (if you type just RET).

  h      view-hello-file.
         Display the HELLO file which illustrates various scripts.


Mode
  m      describe-mode.
         Print documentation of current minor modes, and 
         the current major mode, including their special commands.

Syntax
  s      describe-syntax.
         Display contents of syntax table, plus explanations


FAQ/Tutorial
  F      view-emacs-FAQ.
         Display the frequently asked questions file.

  t      help-with-tutorial.
         Select the Emacs learn-by-doing tutorial.


Emacs News
  n      view-emacs-news.
         Display news of recent Emacs changes.

  C-n    Display news of recent Emacs changes.


Debug
  l      view-lossage.
         Show last 100 characters you typed.

Misc
  C-c    Display Emacs copying permission (General Public License).
  C-d    Display Emacs ordering information.
  C-p    Display information about the GNU project.
  C-w    Display information on absence of warranty for GNU Emacs."
		    help-map)




) ;; (unless (featurep 'xemacs)





(provide 'bm-help)

;;; bm-help.el ends here
