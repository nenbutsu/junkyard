;;; test-menu.el --- test Emacs menu functions and easymenu.el

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.1 $
;; Keywords: local

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


;;; Code:
(eval-when-compile
  (defvar default-menubar)
  (defvar current-menubar)
  (autoload 'popup-menu "" "" t nil)
  (autoload 'tmenu-menu-execute "tmenu" "" t nil)
  )
   

(defun Test-menu-dummy-command ()
  (interactive)
  "Do nothing.")


;;; easymenu.el example ==================================================

(defconst test-menu-easymenu-spec
  '("Easy-Menu"
    ["toggle" Test-menu-dummy-command :style toggle :selected nil]
    ["radio1" Test-menu-dummy-command :style radio  :selected t]
    ["radio2" Test-menu-dummy-command :style radio  :selected nil]
    ["radio3" Test-menu-dummy-command :style radio  :selected nil]
    ["Item1" Test-menu-dummy-command]
    ["Item2" Test-menu-dummy-command]
    ["Item3" Test-menu-dummy-command]
    ("Sub menu"
     ["toggle" Test-menu-dummy-command :style toggle :selected nil]
     ["radio1" Test-menu-dummy-command :style radio  :selected t]
     ["radio2" Test-menu-dummy-command :style radio  :selected nil]
     ["radio3" Test-menu-dummy-command :style radio  :selected nil]
     ["Item1" Test-menu-dummy-command]
     ["Item2" Test-menu-dummy-command])
    )
  "Menu specification for easymenu.el")

(defvar Test-menu-easymenu-command nil
  "Place holder symbol for `easy-menu-define'.
`easy-menu-define' sets the following slots of this symbol.
symbol-value is Emacsen specific menu spec.
symbol-function is a command showing a popup menu.")

(defun test-menu-easymenu (&optional arg)
  "Add a menu to the menu bar and bind S-mouse-2 to a popup menu using easymenu.el."
  (interactive "P")
  (require 'easymenu)
  (let ((mouse-key (if (featurep 'xemacs) [(shift button2)] [S-mouse-2])))
    (cond
     (arg
      (global-unset-key [menu-bar Easy-Menu])
      (global-unset-key mouse-key))
     (t
      (easy-menu-define Test-menu-easymenu-command
                        global-map
                        "Test-Menu-Easymenu"
                        test-menu-easymenu-spec)
      (global-set-key mouse-key 'Test-menu-easymenu-command)
      ;; XEmacs needs this to add easymenu to the menu bar.
      ;; FSF Emacs does nothing except precalculating equivalent key bindings.
      (easy-menu-add test-menu-easymenu-spec)))))

;(force-mode-line-update))



;;; XEmacs native menu example =============================================
(when (featurep 'xemacs)
  (defvar test-menu-original-default-menubar default-menubar)
  (defvar test-menu-original-current-menubar current-menubar)

  (defconst test-menu-XEmacs-spec
    '("My-Menu"
      ["%_toggle" Test-menu-dummy-command :style toggle :selected nil]
      ["radio%_1" Test-menu-dummy-command :style radio  :selected t]
      ["radio2" Test-menu-dummy-command :style radio  :selected nil]
      ["radio3" Test-menu-dummy-command :style radio  :selected nil]
      ["Item1" Test-menu-dummy-command]
      ["Item2" Test-menu-dummy-command]
      ["Item3" Test-menu-dummy-command]
      ("Sub menu"
       :included t
       :accelerator ?x
       :filter (lambda (list) (setq debug (apply 'list list)) list)
       ["toggle" Test-menu-dummy-command :style toggle :selected nil]
       ["radio1" Test-menu-dummy-command :style radio  :selected t]
       ["radio2" Test-menu-dummy-command :style radio  :selected nil]
       "--"
       ["radio3" Test-menu-dummy-command :style radio  :selected nil]
       ["Item1" Test-menu-dummy-command]
       ["Item2" Test-menu-dummy-command])
      ))

  (defun test-menu-XEmacs (&optional arg)
    ""
    (interactive "P")
    (cond
     (arg
      (setq current-menubar test-menu-original-current-menubar))
     (t (setq current-menubar (cons test-menu-XEmacs-spec
                                    test-menu-original-current-menubar))
        (global-set-key [(shift button3)]
          #'(lambda ()
              (interactive)
              (popup-menu test-menu-XEmacs-spec)))
        (global-set-key [f6] #'(lambda ()
                                 (interactive)
                                 (popup-menu test-menu-XEmacs-spec)))))))
        

;;; FSF Emacs native menu example ===========================================
(unless (featurep 'xemacs)
  (defconst test-menu-FSF-spec
    '(keymap
      "My-Menu"
      (?1 menu-item
          "toggle" Test-menu-dummy-command  :accelerator ?a
          :button (:toggle quote nil))
      (?2 menu-item
          "radio1" Test-menu-dummy-command :button (:radio  . t))
      (?3 menu-item
          "radio2" Test-menu-dummy-command :button (:radio  quote nil))
      (?4 menu-item
          "radio3" Test-menu-dummy-command :button (:radio  quote nil))
      (?5 menu-item "Item1" Test-menu-dummy-command)
      (?6 menu-item "Item2" Test-menu-dummy-command)
      (?7 menu-item "Item3" Test-menu-dummy-command)
      (?8 menu-item
          "Sub menu"
          (keymap
           "Sub menu"
           (?1 menu-item
               "toggle" Test-menu-dummy-command :button (:toggle quote nil))
           (?2 menu-item
               "radio1" Test-menu-dummy-command :button (:radio  . t))
           (?3 menu-item
               "radio2" Test-menu-dummy-command :button (:radio  quote nil))
           (?4 menu-item
               "radio3" Test-menu-dummy-command :button (:radio  quote nil))
           (?5 menu-item "Item1" Test-menu-dummy-command)
           (?6 menu-item "Item2" Test-menu-dummy-command))
          :filter (lambda (item)
                    (let ((map (apply 'list item)))
                      (setcdr (nthcdr 1 map) (nthcdr 3 map))
                      map))))
    "Menu keymap for FSF Emacs.")

  (defun test-menu-FSF (&optional arg)
    ""
    (interactive "P")
    (cond
     (arg
      (global-unset-key [f4])
      (global-unset-key [f6])
      (global-unset-key [menu-bar My-Menu]))

     (t (global-set-key [f4] test-menu-FSF-spec) ; text based interface
        (global-set-key [menu-bar My-Menu]
                        (list 'menu-item "My-Menu" test-menu-FSF-spec))
        (global-set-key [S-mouse-3]
                        test-menu-FSF-spec)
        (global-set-key [f6]
                        #'(lambda ()
                            (interactive)
                            (x-popup-menu t test-menu-FSF-spec))))))
  
  (defvar simple-popup
    '("TITLE"
      ("PANE1"
       ("P1Item1" . Test-menu-dummy-command)
       ("P1Item2" . Test-menu-dummy-command)
       ("P1Item3" . ("SUB"
                     ("SUB1" . Test-menu-dummy-command)
                     ("SUB2" . Test-menu-dummy-command)
                     ("SUB3" . Test-menu-dummy-command))))
      ("PANE2"
       ("P2Item1" . Test-menu-dummy-command)
       ("P2Item2" . Test-menu-dummy-command)
       ("P2Item3" . Test-menu-dummy-command))
      ))

  (defun test-menu-popup ()
    (interactive)
    (x-popup-menu t simple-popup))
  (defun test-menu-popup-tmenu ()
    (interactive)
    (require 'tmenu)
    (tmenu-menu-execute simple-popup))
  )

(defun test-menu-tmenu-popup ()
  (interactive)
  (require 'tmenu)
  (tmenu-menu-execute
   '("My Popup menu"
     ["%_Find file at point"        find-file-at-point]
     ["Find %_function at point"    find-function-at-point]
     ["Find %_variable at point"    find-variable-at-point]
     ["Brows %_url at point" browse-url]))
  )

(global-set-key [f1] 'test-menu-tmenu-popup)
(easy-menu-define a nil ""
       '("My Popup menu"
         ["%_Find file at point"        find-file-at-point]
         ["Find %_function at point"    find-function-at-point]
         ["Find %_variable at point"    find-variable-at-point]
         ["Brows %_url at point" browse-url]))
(global-set-key [mouse-1] 'a)


(provide 'test-menu)

;;; test-menu.el ends here
