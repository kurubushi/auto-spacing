# auto-spacing

日本語と英語の間にスペースを**その場で**挿入する．
日本語入力メソッドは SKK を想定している．


## 使い方

```elisp
(el-get-bundle auto-spacing
  :type    github
  :pkgname "kurubushi/auto-spacing")
(auto-spacing-mode t)
```

TeX mode で $ と日本語の間にスペースを追加しようとする場合，
$ の入力は `TeX-insert-dollar` 関数を経由して行われるため，
これを `auto-spacing-self-insert-command-list` に追加する．

```elisp
;; $ と日本語の間にもスペース
(setq auto-spacing-english-regexp (rx (in "a-zA-Z0-9$")))
;; TeX-insert-dollar を追加
(add-to-list 'auto-spacing-self-insert-command-list 'TeX-insert-dollar)
;; bxcjkjatype では ~ が四分アキ
(setq auto-spacing-separator "~")
```
