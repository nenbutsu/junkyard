;;; bm-wp-idx.el --- white paper index

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

(global-set-key [f2] 'bm-wp-idx-do-it)


(defvar bm-wp-idx-loop-files
  
)

(defun bm-wp-idx-loop ()
)

(defun bm-wp-idx-do-it ()
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

  ; ］を」へ変換
  (goto-char (point-min))
  (query-replace-regexp "］" "」")
  (goto-char (point-min))
  (query-replace-regexp "［" "「")

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









(provide 'bm-wp-idx)

;;; bm-wp-idx.el ends here
