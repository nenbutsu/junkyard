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
;; 「標準 校正必携」ページ197

;;; Code:


(defvar kan-kana-alist
  '(("嗚呼" "ああ")
    ("彼奴" "あいつ")
    ("敢て" "あえて")
    ("敢えて" "あえて")
    ("飽くまで" "あくまで")
    ("貴方" "あなた")
    ("数多" "あまた")
    ("余りに" "あまりに")
    ("予め" "あらかじめ")
    ("有る" "ある")
    ("在る" "ある")
    ("或は" "あるいは")
    ("併せて" "あわせて")
    ("如何" "いかが")
    ("如何が" "いかが")
    ("徒に" "いたずらに")
    ("一々" "いちいち")
    ("何時" "いつ")
    ("一杯に" "いっぱいに")
    ("未だ" "いまだ")
    ("色々" "いろいろ")
    ("種々" "いろいろ")
    ("いれる" "入れる")
    ("いれたら" "入れたら")
    ("いれます" "入れます")
    ("謂わば" "いわば")
    ("於いて" "おいて")
    ("概ね" "おおむね")
    ("於ける" "おける")
    ("恐らく" "おそらく")
    ("各々" "おのおの")
    ("自ずから" "おのずから")
    ("自ら" "おのずから")
    ("己" "おのれ")
    ("夥しい" "おびただしい")
    ("慮る" "おもんぱかる")
    ("及び" "および")
    ("疎かに" "おろそかに")
    ("箇" "カ" "か")
    ("却って" "かえって")
    ("拘らず" "かかわらず")
    ("傍ら" "かたわら")
    ("且つ" "かつ")
    ("彼方" "かなた")
    ("鑑みる" "かんがみる")
    ("極めて" "きわめて")
    ("下さい" "ください")
    ("極く" "ごく")
    ("如く" "ごとく")
    ("殊更" "ことさら")
    ("殊に" "ことに")
    ("毎に" "ごとに")
    ("遡って" "さかのぼって")
    ("流石" "さすが")
    ("定めし" "さだめし")
    ("様々" "さまざま")
    ("更々" "さらさら")
    ("次第に" "しだいに")
    ("暫く" "しばらく")
    ("渋々" "しぶしぶ")
    ("所詮" "しょせん")
    ("随分" "ずいぶん")
    ("既に" "すでに")
    ("即ち" "すなわち")
    ("須く" "すべからく")
    ("折角" "せっかく")
    ("沢山" "たくさん")
    ("但し" "ただし")
    ("直ちに" "ただちに")
    ("例えば" "たとえば")
    ("度々" "たびたび")
    ("多分" "たぶん")
    ("因みに" "ちなみに")
    ("因に" "ちなみに")
    ("丁度" "ちょうど")
    ("遂に" "ついに")
    ("詰まり" "つまり")
    ("出来る" "できる")
    ("出来ます" "できます")
    ("到底" "とうてい")
    ("取り合えず" "とりあえず")
    ("取り敢えず" "とりあえず")
    ("取敢えず" "とりあえず")
    ("乃至" "ないし")
    ("何故" "なぜ")
    ("何しろ" "なにしろ")
    ("何卒" "なにとぞ")
    ("並びに" "ならびに")
    ("何で" "なんで")
    ("俄に" "にわかに")
    ("果たして" "はたして")
    ("甚だ" "はなはだ")
    ("相応しい" "ふさわしい")
    ("正に" "まさに")
    ("先ず" "まず")
    ("益々" "ますます")
    ("迄" "まで")
    ("見なす" "みなす")
    ("見倣す" "みなす")
    ("無闇に" "むやみに")
    ("無論" "むろん")
    ("滅多に" "めったに")
    ("勿論" "もちろん")
    ("尤も" "もっとも")
    ("専ら" "もっぱら")
    ("最早" "もはや")
    ("諸々" "もろもろ")
    ("故に" "ゆえに")
    ("余計に" "よけいに")
    ("歴と" "れっきと")
    ("分かる" "わかる")
    ("判る" "わかる")
    ("解る" "わかる")
    ("弁える" "わきまえる")
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
