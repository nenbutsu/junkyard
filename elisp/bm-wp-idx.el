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
  (query-replace "����" "��")

  (goto-char (point-min))
  (query-replace-regexp "\\(��\\|��\\)\\1+" "\\1")

  (goto-char (point-min))
  (query-replace "�إ�" "�إ�")

  (goto-char (point-min))
  (query-replace "���إΥ�" "���إ�")

  (goto-char (point-min))
  (query-replace "�륡�ӥ�" "�륢�ӥ�")

  (goto-char (point-min))
  (query-replace-regexp "����...." "����ԥ塼��")

  (goto-char (point-min))
  (query-replace-regexp "\\([^���;��Ҥ�-��]\\)��" "\\1��")

  (goto-char (point-min))
  (query-replace "��\n" "\n")

  (goto-char (point-min))
  (query-replace-regexp "\\([��-��]\\)\\(��\\|��\\)" "\\1��")

  (goto-char (point-min))
  (query-replace-regexp "\\(��\\|��\\)\\([��-��]\\)" "��\\2")

  (goto-char (point-min))
  (query-replace-regexp "����\\(��\\|��\\)" "��\\1")

  ;�����ȴ����δ֤����Ѷ�����
  ;(goto-char (point-min))
  ;(query-replace-regexp "\\(\\cC\\)��\\(\\cC\\)" "\\1\\2")

  ;�����
  (goto-char (point-min))
  (query-replace-regexp "��+" "")

  ; ��Ƭ�����ѡʡˤ˰Ϥޤ줿��������ϡ���������Ѷ���
  (goto-char (point-min))
  (query-replace-regexp "^��*��\\([��-��]+\\|��\\)��\\([^��]\\)"
			"����\\1�ˡ�\\2")

  ; ���θ塢���Ԥ���Ƥ���Ȥ��ϡ����ιԤǻ�����
  (goto-char (point-min))
  (query-replace-regexp "\\(��\n\\)\\([^��]\\)" "\\1��\\2")

  ; �Ϥ�פ��Ѵ�
  (goto-char (point-min))
  (query-replace-regexp "��" "��")
  (goto-char (point-min))
  (query-replace-regexp "��" "��")

  ; �ե�������������
  (goto-char (point-min))
  (query-replace-regexp "\n\\(��*\n?\\)+\\'"     "\n")

  (goto-char (point-min))
  (query-replace-regexp "\\(\\cH\\|\\cK\\|\\cC\\)\n\\'" "\\1")

  (goto-char (point-min))
  (message "������")
  (search-forward "��" nil t 10000)
  
  )



;;; Code:









(provide 'bm-wp-idx)

;;; bm-wp-idx.el ends here
