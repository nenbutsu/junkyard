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
  ;��Ƭ����̤�
  (goto-char (point-min))
  (query-replace-regexp
       "^��\\(��\\|[��-��]��\\|��\\|����\\|[0-9]��\\|[��ɽ][0-9]\\)\\(.*\\)"
       "\\1\\2")

  ;ñ��
  (goto-char (point-min))
  (while (search-forward-regexp
	  "�ȣ�\\|���\\|���\\|���\\|��\\|���|���\\|��\\|����\\|�Σϣ�\\|�ã�"
	  nil t)
    (y-or-n-p "ñ��->Ⱦ��?")
    (japanese-hankaku-region
     (match-beginning 0)
     (match-end 0)))

  (goto-char (point-min))
  (query-replace "����" "̤��")

  (goto-char (point-min))
  (query-replace "ͼ" "��")
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

  ; ɽ���ޤ���������Ѷ���
  (goto-char (point-min))
  (query-replace-regexp "^��*\\([ɽ��][��-��]+��[��-��]+\\)\\([^����-��]\\)"
			"��\\1��\\2")

  ; ������������Ѷ���
  (goto-char (point-min))
  (query-replace-regexp "^��*��\\([^��]\\)"
			"������\\1")

  ; �Ϥ�פ��Ѵ�
  (goto-char (point-min))
  (query-replace-regexp "��" "��")
  (goto-char (point-min))
  (query-replace-regexp "��" "��")

  ; ��μ��ϡ�������
  (goto-char (point-min))
  (query-replace-regexp
   "^\\(��\\([��-��]\\|��[��-����-��]+��\\).*\n\\)\\([^��]\\)"
   "\\1��\\3")

  ; ����������μ��ϡ�������
  (goto-char (point-min))
  (query-replace-regexp
   "^\\(��\\cK��.*\n\\)\\([^��]\\)"
   "\\1��\\2")


  (goto-char (point-min))
  (query-replace-regexp "^\\(����[��-����].*\n\\)\\([^��]\\)" "\\1��\\2")

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









(provide 'bm-wp)

;;; bm-wp.el ends here
