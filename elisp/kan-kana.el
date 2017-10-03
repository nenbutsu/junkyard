;;; kan-kana.el --- kanji to kana

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
;; ��ɸ�� ����ɬ�ȡץڡ���197

;;; Code:


(defvar kan-kana-alist
  '(("�˸�" "����")
    ("����" "������")
    ("����" "������")
    ("������" "������")
    ("˰���ޤ�" "�����ޤ�")
    ("����" "���ʤ�")
    ("��¿" "���ޤ�")
    (";���" "���ޤ��")
    ("ͽ��" "���餫����")
    ("ͭ��" "����")
    ("�ߤ�" "����")
    ("����" "���뤤��")
    ("ʻ����" "���碌��")
    ("ǡ��" "������")
    ("ǡ����" "������")
    ("�̤�" "���������")
    ("�졹" "��������")
    ("����" "����")
    ("���դ�" "���äѤ���")
    ("̤��" "���ޤ�")
    ("����" "������")
    ("�" "������")
    ("�����" "�����")
    ("���줿��" "���줿��")
    ("����ޤ�" "����ޤ�")
    ("����" "�����")
    ("������" "������")
    ("����" "�������")
    ("������" "������")
    ("���餯" "�����餯")
    ("�ơ�" "���Τ���")
    ("��������" "���Τ�����")
    ("����" "���Τ�����")
    ("��" "���Τ�")
    ("���" "���Ӥ�������")
    ("θ��" "�����Ѥ���")
    ("�ڤ�" "�����")
    ("�¤���" "��������")
    ("��" "��" "��")
    ("�Ѥä�" "�����ä�")
    ("���餺" "������餺")
    ("˵��" "�������")
    ("���" "����")
    ("����" "���ʤ�")
    ("�դߤ�" "���󤬤ߤ�")
    ("�ˤ��" "������")
    ("������" "��������")
    ("�ˤ�" "����")
    ("ǡ��" "���Ȥ�")
    ("�칹" "���Ȥ���")
    ("���" "���Ȥ�")
    ("���" "���Ȥ�")
    ("�̤ä�" "�����Τܤä�")
    ("ή��" "������")
    ("��ᤷ" "�����ᤷ")
    ("�͡�" "���ޤ���")
    ("����" "���餵��")
    ("�����" "��������")
    ("�ä�" "���Ф餯")
    ("�¡�" "���֤���")
    ("����" "���礻��")
    ("��ʬ" "�����֤�")
    ("����" "���Ǥ�")
    ("¨��" "���ʤ��")
    ("�ܤ�" "���٤��餯")
    ("�޳�" "���ä���")
    ("����" "��������")
    ("â��" "������")
    ("ľ����" "��������")
    ("�㤨��" "���Ȥ���")
    ("�١�" "���Ӥ���")
    ("¿ʬ" "���֤�")
    ("���ߤ�" "���ʤߤ�")
    ("����" "���ʤߤ�")
    ("����" "���礦��")
    ("���" "�Ĥ���")
    ("�ͤޤ�" "�Ĥޤ�")
    ("�����" "�Ǥ���")
    ("����ޤ�" "�Ǥ��ޤ�")
    ("����" "�Ȥ��Ƥ�")
    ("���礨��" "�Ȥꤢ����")
    ("��괺����" "�Ȥꤢ����")
    ("�贺����" "�Ȥꤢ����")
    ("ǵ��" "�ʤ���")
    ("����" "�ʤ�")
    ("������" "�ʤˤ���")
    ("��´" "�ʤˤȤ�")
    ("�¤Ӥ�" "�ʤ�Ӥ�")
    ("����" "�ʤ��")
    ("���" "�ˤ狼��")
    ("�̤�����" "�Ϥ�����")
    ("�Ӥ�" "�ϤʤϤ�")
    ("�������" "�դ��路��")
    ("����" "�ޤ���")
    ("�褺" "�ޤ�")
    ("�ס�" "�ޤ��ޤ�")
    ("��" "�ޤ�")
    ("���ʤ�" "�ߤʤ�")
    ("���魯" "�ߤʤ�")
    ("̵�Ǥ�" "���ߤ�")
    ("̵��" "����")
    ("��¿��" "��ä���")
    ("����" "������")
    ("���" "��äȤ�")
    ("���" "��äѤ�")
    ("����" "��Ϥ�")
    ("����" "�����")
    ("�Τ�" "�椨��")
    (";�פ�" "�褱����")
    ("���" "��ä���")
    ("ʬ����" "�狼��")
    ("Ƚ��" "�狼��")
    ("���" "�狼��")
    ("�ۤ���" "�來�ޤ���")
    )
  "")

(defun kan-kana-make-regex (&optional alist)
  (unless alist (setq alist kan-kana-alist))
  (let ((regex (caar alist)))
    (while (progn (setq alist (cdr alist)) alist)
      (setq regex (concat regex "\\|" (caar alist))))
    regex))

(defun kan-kana-change ()
  ""
  (interactive)
  (re-search-forward (kan-kana-make-regex)))
  

(global-set-key [f4] 'kan-kana-change)


(provide 'kan-kana)

;;; kan-kana.el ends here
