;;; bm-wp.el --- white paper

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

(global-set-key [f2] 'bm-wp-do-it)
(global-set-key [f1] 'bm-wp-check-it)
(global-set-key [f4] 'bm-wp-grep)

(defun bm-wp-check-it ()
  (interactive)
  ;先頭空白ぬき
  (goto-char (point-min))
  (query-replace-regexp
       "^　\\(（\\|[ア-ン]　\\|〔\\|○　\\|[0-9]　\\|[図表][0-9]\\)\\(.*\\)"
       "\\1\\2")

  ;単位
  (goto-char (point-min))
  (while (search-forward-regexp
	  "ＨＣ\\|ｋｍ\\|ｍｍ\\|ｃｍ\\|ｍ\\|ｍｇ|ｋｇ\\|ｇ\\|ｐｐｍ\\|ＮＯｘ\\|ＣＯ"
	  nil t)
    (y-or-n-p "単位->半角?")
    (japanese-hankaku-region
     (match-beginning 0)
     (match-end 0)))

  (goto-char (point-min))
  (query-replace "末満" "未満")

  (goto-char (point-min))
  (query-replace "夕" "タ")
)

(defvar bm-wp-grep-files nil)
(defvar bm-wp-grep-dir nil)

(defun bm-wp-grep (regexp dir)
  "Grep"
  (interactive "sGrep regexp: \nDDirectory: ")
  (setq dir (directory-file-name (expand-file-name dir)))
  (let (buff file)
    (if (or (not (equal dir bm-wp-grep-dir))
	    (null bm-wp-grep-files)
	    (not (y-or-n-p (format "Resume from %s " (car bm-wp-grep-files)))))
	(setq bm-wp-grep-dir   dir
	      bm-wp-grep-files (directory-files dir nil "E.*\\.[0-9A-Fa-f]+")))

    (while bm-wp-grep-files
      (setq file (car bm-wp-grep-files)
	    buff (find-file-noselect file))
      (message (format "Searching %s" file))
      (switch-to-buffer buff)
      (goto-char (point-min))
      (while (and (re-search-forward regexp nil t)
		  (set-mark (match-beginning 0))
		  (y-or-n-p "Next :")))
      (kill-buffer buff)
      (setq bm-wp-grep-files (cdr bm-wp-grep-files)))
    ))

(defun bm-wp-do-it ()
  (interactive)
  (goto-char (point-min))
  (query-replace "し・" "い")

  (goto-char (point-min))
  (query-replace-regexp "\\(…\\|　\\)\\1+" "\\1")

  (goto-char (point-min))
  (query-replace "へル" "ヘル")

  (goto-char (point-min))
  (query-replace "黒へノレ" "黒ヘル")

  (goto-char (point-min))
  (query-replace "ルァビプ" "ルアビブ")

  (goto-char (point-min))
  (query-replace-regexp "コン...." "コンピュータ")

  (goto-char (point-min))
  (query-replace-regexp "\\([^坂手人小子ぁ-ん]\\)口" "\\1ロ")

  (goto-char (point-min))
  (query-replace "　\n" "\n")

  (goto-char (point-min))
  (query-replace-regexp "\\([０-９]\\)\\(〇\\|。\\)" "\\1０")

  (goto-char (point-min))
  (query-replace-regexp "\\(〇\\|。\\)\\([０-９]\\)" "０\\2")

  (goto-char (point-min))
  (query-replace-regexp "し、\\(る\\|た\\)" "い\\1")

  ;漢字と漢字の間の全角空白削除
  ;(goto-char (point-min))
  ;(query-replace-regexp "\\(\\cC\\)　\\(\\cC\\)" "\\1\\2")

  ;〓削除
  (goto-char (point-min))
  (query-replace-regexp "〓+" "")

  ; 行頭の全角（）に囲まれた数字、注は、前後に全角空白
  (goto-char (point-min))
  (query-replace-regexp "^　*（\\([０-９]+\\|注\\)）\\([^　]\\)"
			"　（\\1）　\\2")

  ; 。の後、改行されているときは、次の行で字下げ
  (goto-char (point-min))
  (query-replace-regexp "\\(。\n\\)\\([^　]\\)" "\\1　\\2")

  ; 表、図の前後に全角空白
  (goto-char (point-min))
  (query-replace-regexp "^　*\\([表図][０-９]+−[０-９]+\\)\\([^　０-９]\\)"
			"　\\1　\\2")

  ; ○の前後に全角空白
  (goto-char (point-min))
  (query-replace-regexp "^　*○\\([^　]\\)"
			"　○　\\1")

  ; ］を」へ変換
  (goto-char (point-min))
  (query-replace-regexp "］" "」")
  (goto-char (point-min))
  (query-replace-regexp "［" "「")

  ; 題の次は、字下げ
  (goto-char (point-min))
  (query-replace-regexp
   "^\\(　\\([０-９]\\|（[０-９ア-ン]+）\\).*\n\\)\\([^　]\\)"
   "\\1　\\3")

  ; カタカナ題の次は、字下げ
  (goto-char (point-min))
  (query-replace-regexp
   "^\\(　\\cK　.*\n\\)\\([^　]\\)"
   "\\1　\\2")


  (goto-char (point-min))
  (query-replace-regexp "^\\(　（[０-９昭].*\n\\)\\([^　]\\)" "\\1★\\2")

  ; ファイル末尾整理
  (goto-char (point-min))
  (query-replace-regexp "\n\\(　*\n?\\)+\\'"     "\n")

  (goto-char (point-min))
  (query-replace-regexp "\\(\\cH\\|\\cK\\|\\cC\\)\n\\'" "\\1")

  (goto-char (point-min))
  (message "漢字一")
  (search-forward "一" nil t 10000)
  
  )



;;; Code:









(provide 'bm-wp)

;;; bm-wp.el ends here
