;;; minibuffer-commands-ad.el --- minibuffer completion commands in elisp

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
(eval-when-compile (require 'advice))
(require 'minibuffer-commands)

(defadvice minibuffer-complete
  (around minibuffer-commands-ad last activate compile preactivate)
  "Emulate the original C primitive function in elisp."
  (minibuffer-commands-complete))

(defadvice minibuffer-complete-word
  (around minibuffer-commands-ad last activate compile preactivate)
  "Emulate the original C primitive function in elisp."
  (minibuffer-commands-complete-word))

(defadvice minibuffer-complete-and-exit
  (around minibuffer-commands-ad last activate compile preactivate)
  "Emulate the original C primitive function in elisp."
  (minibuffer-commands-complete-and-exit))

(defadvice minibuffer-completion-help
  (around minibuffer-commands-ad last activate compile preactivate)
  "Emulate the original C primitive function in elisp."
  (minibuffer-commands-completion-help))

(provide 'minibuffer-commands-ad)

;;; minibuffer-commands-ad.el ends here
