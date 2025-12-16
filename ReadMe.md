# これは何
macのkey入力をフックしてimeの切り替えを行うemacs拡張機能

# 内部構造
大まかに２つのブロックからなる
１つは、Emacsのダイナミックモジュールを使って、macのイベントを取得してlisp側のフックを読み出すライブラリ
イベントの取得にはaddLocalMonitorForEvents(matching:handler:)を使う
２つめは、１から呼ばれたフックを定義するelisp関数

最終的にMELPAに登録して使えるモジュールを目指す、なのでコンパイル方法やコーディングルールはGNUに準拠する

# インストールとビルド

このパッケージはEmacsのダイナミックモジュールを使用しているため、使用前にコンパイルが必要です。

1. リポジトリをクローンします。
2. 以下のコマンドでモジュールをビルドします。

```bash
make
```

または、Emacs内から以下のコマンドを実行します。

`M-x ime-hook-mac-build-module`

# 設定例

```elisp
(add-to-list 'load-path "/path/to/ime-hook-mac")
(require 'ime-hook-mac)

;; フック関数の定義
(defun my-ime-hook-handler (keycode modifiers)
  (message "Key: %d, Modifiers: %d" keycode modifiers))

(add-hook 'ime-hook-mac-functions #'my-ime-hook-handler)

;; モニターの開始
(ime-hook-mac-enable)

;; 現在の入力ソースIDを取得 (例: "com.apple.keylayout.US")
(ime-hook-mac-get-input-source)

;; 入力ソースIDを指定して変更
(ime-hook-mac-set-input-source "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese");; 現在の入力ソースIDを取得 (例: "com.apple.keylayout.US")
(ime-hook-mac-get-input-source)

;; 利用可能な入力ソースIDの一覧を取得
(ime-hook-mac-get-input-source-list)

;; 入力ソースIDを指定して変更
(ime-hook-mac-set-input-source "com.apple.keylayout.ABC")
```
