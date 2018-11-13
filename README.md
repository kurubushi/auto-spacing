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

PDFTeX などを使う場合，次のようにするとよさそう．

```elisp
;; $ と日本語の間にもスペース
(setq auto-spacing-english-regexp (rx (in "a-zA-Z0-9$")))
;; bxcjkjatype では ~ が四分アキ
(setq auto-spacing-separator "~")
```
