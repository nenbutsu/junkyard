;;; GNU Emacs initialization file

;; Copyright (C) 2014, 2017 Yuji Minejima <bmonkey@nifty.com>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision$

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



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
;;(global-set-key [kanji] 'toggle-input-method)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/islelisp"))


;; 固定等幅フォント
(set-face-attribute 'fixed-pitch    nil :family "ＭＳ ゴシック")
;; 可変幅フォント
(set-face-attribute 'variable-pitch nil :family "ＭＳ ゴシック")

(add-to-list 'default-frame-alist '(font . "ＭＳ ゴシック-12"))

(set-face-font 'font-lock-comment-face       "ＭＳ ゴシック-12")
(set-face-font 'font-lock-string-face        "ＭＳ ゴシック-12")
(set-face-font 'font-lock-keyword-face       "ＭＳ ゴシック-12")
(set-face-font 'font-lock-builtin-face       "ＭＳ ゴシック-12")
(set-face-font 'font-lock-function-name-face "ＭＳ ゴシック-12")
(set-face-font 'font-lock-variable-name-face "ＭＳ ゴシック-12")
(set-face-font 'font-lock-type-face          "ＭＳ ゴシック-12")
(set-face-font 'font-lock-constant-face      "ＭＳ ゴシック-12")
(set-face-font 'font-lock-warning-face       "ＭＳ ゴシック-12")

(setq-default line-spacing 2)

;;; Frame size & position
(setq default-frame-alist
      (append (list
               ;;'(foreground-color . "black")
               ;;'(background-color . "LemonChiffon")
               ;;'(background-color . "gray")
               ;;'(border-color . "black")
               ;;'(mouse-color . "white")
               ;;'(cursor-color . "black")
               '(width . 100)
               '(height . 60)
               '(top . 0)
               '(left . 500)
               )
              default-frame-alist))
(tool-bar-mode 0)


(require 'bm-util)
(require 'bm-find-elisp)
(global-set-key [f8]  'bm-switch-to-scratch-buffer)
(global-set-key [(control tab)] 'bm-switch-to-other-buffer)


;;; "global key binding"
;;(global-set-key [(delete)] 'delete-char)
;;(global-set-key "\C-x\C-q" 'toggle-read-only)
(global-set-key [?\C-x ?\C-2] 'split-window-vertically)

(global-set-key "\C-xk"  'bm-kill-current-buffer)
(global-set-key "\C-xE"  'bm-exec-script-buffer)
(global-set-key "\C-x\M-e" 'eval-buffer)
(global-set-key "\M-]" 'delete-other-windows)
(global-set-key "\M-d" 'bm-delete-token)

(define-key ctl-x-map "F" 'find-function)
(define-key ctl-x-4-map "F" 'find-function-other-window)
(define-key ctl-x-5-map "F" 'find-function-other-frame)
(define-key ctl-x-map "K" 'find-function-on-key)
(define-key ctl-x-map "V" 'find-variable)
(define-key ctl-x-4-map "V" 'find-variable-other-window)
(define-key ctl-x-5-map "V" 'find-variable-other-frame)

(unless (lookup-key global-map "\C-xrd")
  (global-set-key "\C-xrd" 'delete-rectangle))

(let ((map (make-sparse-keymap)))
    (global-set-key (if window-system [(control ?,)] [(control ?c)]) map)
    (define-key map "=" 'bm-show-code-mode)
    (define-key map "e" 'bm-find-elisp-symbol)
    (define-key map "\C-e" 'bm-find-elisp-file)
    (define-key map "p" 'bm-perl-find)
    (define-key map "P" 'bm-perl-grep-perldoc)
    (define-key map "c" 'iman)
    (define-key map "r" 'bm-rfc-find)
    (define-key map "y" 'bm-syntax-show-descriptor)
    (define-key map "b" 'bookmark-jump)
    (define-key map "v" 'quick-calc)
    (define-key map "j" 'goto-line)
    (define-key map "l" '(lambda () (interactive) (recenter 0)))
    (define-key map "\C-l" '(lambda () (interactive) (recenter -1)))
    (define-key map "a" 'bm-insert-info-link)
    (define-key map "n" 'bm-goto-link)
    (define-key map "s" 'bm-shell)
    (define-key map "8" 'bm-tab8)
    (define-key map "4" 'bm-tab4)
    (define-key map "0" 'bm-kill-buffer-and-window)
    (define-key map "i" 'bm-right-justify-from-here)
    (define-key map "h" 'hyperspec-lookup)
    (define-key map "d" 'islisp-hyperdraft)
    )


(setq-default indent-tabs-mode nil)

;;; Minor modes, buffer local options
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\)\\'" . nxml-mode)
              auto-mode-alist))

;;; mcomplete
(require 'mcomplete)
(turn-on-mcomplete-mode)
(put 'bookmark-jump 'mcomplete-mode '(:exhibit-start-chars 0))
(put 'sgml-insert-element 'mcomplete-mode '(:exhibit-start-chars 0))

(require 'completing-help)
(turn-on-completing-help-mode)

(require 'eldoc)

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
	  #'(lambda ()
                ;;;(show-paren-mode t)
	      (setq indent-tabs-mode nil) ; don't use tabs for indentations
	      (define-key emacs-lisp-mode-map [return] 'newline-and-indent)
	      (eldoc-mode t)))

;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook
	  #'(lambda ()
                ;;;(show-paren-mode t)
	      (define-key lisp-interaction-mode-map
		[return] 'newline-and-indent)
	      (eldoc-mode t)))

;; Bookmarks
(setq bookmark-sort-flag nil)

;; hyperspec
(eval-after-load "hyperspec" '(load "hyperspec-addon"))
(require 'hyperspec)
(setq common-lisp-hyperspec-root "file:///C:/Program%20Files%20%28x86%29/LispWorks/lib/6-1-0-0/manual/online/CLHS/")
(require 'hyperdraft)

(setq browse-url-browser-function 'browse-url-firefox)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mcomplete-prefix-method-alternative-part-face ((t (:foreground "dark slate gray"))))
 '(mcomplete-prefix-method-fixed-part-face ((t (:foreground "dark slate gray" :weight bold))))
 '(mcomplete-substr-method-alternative-part-face ((t (:foreground "forest green"))))
 '(mcomplete-substr-method-fixed-part-face ((t (:foreground "forest green" :weight bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode . "K&R")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
