# mac-ime

IMEパッチの入っていないEmacsでIMEを快適に使うための拡張機能です。
macOSのキー入力イベントをフックして、プリフィックスキーが押されたり、ミニバッファの入力時などにIMEの切り替えや制御を行います。
Emacsのダイナミックモジュール機能を利用してOSのIMEを制御しているためIMEパッチなしのEmacsでもストレスなく使用することができます。

## 特徴

- **キーイベントのフック**: `NSEvent` を監視し、特定のキー入力や修飾キーの状態変化をEmacs側で検知します。
- **IME制御**: 現在の入力ソース（IME）の取得や変更が可能です。
- **高速な動作**: Objective-Cで書かれたダイナミックモジュールにより、低レイテンシでイベントを処理します。

## 必要要件

- macOS
- Emacs 27.1 以上 (ダイナミックモジュールサポートが有効であること)
- Clang (ビルド用)

## インストールとビルド

ダイナミックモジュールをファットバイナリーで作成しているため、AppleシリコンでもX86搭載のMacでもリポジトリ内の (`.so` ファイル)が使えるのでビルドは不要です。

リポジトリのクローンのみで使用可能です。

1. リポジトリをクローンします。

```bash
git clone https://github.com/ma0001/mac-ime.git
```

## 設定例

`init.el` などに以下の設定を追加してください。

基本設定は以下だけです

```elisp
;; ロードパスの追加 (リポジトリのパスに合わせて変更してください)
(add-to-list 'load-path "/path/to/mac-ime")
(require 'mac-ime)

;; input methodgを"mac-ime"に設定
(setq default-input-method "mac-ime")

;; モジュールの有効化 (イベント監視の開始)
(mac-ime-enable)
```

本機能が有効になるのはinput methodが"mac-ime"の場合です。
C-\ (toggle-input-method) や cmd-space などで日本語入力状態にすることによりIMEの自動オフと復帰動作を行うようになります。


### プリフィックスキー入力時のIMEオフ

日本語入力が有効な状態でC-xなどのプリフィックスキーを入力するとRoman入力に切り替わり、コマンド実行後は元に戻ります。

デフォルトのプリフィックスキー定義は以下の通りです
- C-x
- C-c
- C-h
- M-g
- M-s
- Esc

特別なキーバイディングでプリフィックスキーが変わっている場合などはmac-ime-modifier-action-tableを(mac-ime-enable)前に設定することにより変えることができます。
```elisp
;; controlキーの修飾で 'z' (keycode 6) をプレフィックスとして扱いたい場合の例
(setq mac-ime-modifier-action-table
      '((control . (mac-ime-kVK_ANSI_X mac-ime-kVK_ANSI_C mac-ime-kVK_ANSI_H 6))
        (meta . (mac-ime-kVK_ANSI_G))
        (nomodifier . (mac-ime-kVK_Escape))))
```

metaや、control以外のキーを使うようなキーでも制御したい場合はmac-ime-prefix-keysを設定することで可能です。mac-ime-debug-levelを1に設定することによりキーコードがメッセージに出力されるのでそれを参考に設定してください。

```elisp
;; C-j (keycode: 38, modifiers: 262401) をプレフィックスキーとして登録する例
(setq mac-ime-prefix-keys
      '((38 . 262401)))
```


### ミニバッファ入力時のIMEオフ

ミニバッファへの入力時にRoman入力となり入力後に元に戻すようにするため、`mac-ime-auto-deactivate-functions` に指定してある関数では、関数実行前にRoman入力とし関数実行後に戻す処理を追加しています。

デフォルトで設定している関数は以下の通りです。

- `read-string`
- `read-char`
- `read-event`
- `read-char-exclusive`
- `read-char-choice`
- `read-no-blanks-input`
- `read-from-minibuffer`
- `completing-read`
- `y-or-n-p`
- `yes-or-no-p`
- `map-y-or-n-p`

また、`read-string` や `read-from-minibuffer` などの `inherit-input-method` 引数を持つ関数については、その引数が non-nil の場合はバッファのinput-methodが"mac-ime"かを確認し、その場合のみmacのIMEをオンするように制御しています。
設定を変更する場合は、関数シンボルのみ、または `(関数シンボル . inherit-input-methodの引数インデックス)` の形式で指定します。


また、C-uのようにコマンド実行後のキーでRoman入力として次のコマンド実行前に戻す必要があるものについては、変数mac-ime-temporary-deactivate-functionsに指定しています。（universal-argumentではC-u後の最初のキー入力しかRomanとならないため、その後の数字入力で繰り返し呼ばれるuniversal-argument--modeを登録しています）

### IMEの入力モード判定

本モジュールではmacOSの入力ソースがRomanなのか日本語などの非Romanなのかを判定する必要があるためinput source IDを正規表現を使って判定を行っています。もし特殊なIMEを使っていてこの判断が正しく動作しない場合はmac-ime-no-ime-input-source-regexpで正しくRomanを判断できるように設定してください。現在使用可能なinput source IDは(mac-ime-get-input-source-list)で取得できます。

## 暫定処理

### minibufferのIMEがONになることがある

日本語入力中にM-%で日本語の置換をした後にC-x bなどでminibaffer表示すると日本語入力の状態になる

原因：

minibufferで日本語入力すると、minibufferのcurrent-input-methodはmac-imeになる。
この状態でread-from-minibufferがINHERIT-INPUT-METHOD nilで呼ばれると、バッファ移動前にmacのIMEを一旦英語入力とするが、
その後にminibufferへの切り替えが発生する。
その時window-selection-change-functions のフックでmac-ime-update-stateの処理でIMEがバッファに合わせて日本語入力になってしまう

暫定対策：

バッファが切り替わってからIMEを英語にしたいが難しい。
mac-ime--ignore-input-source-changeが有効な間は、バッファ変更時のIME更新処理をしないようにする
また、カレントバッファが英語の状態でminibufferに切り替わった時にも日本語に切り替わらないように、すでに英語の状態でも英語に切り替える処理を行いmac-ime--ignore-input-source-changeを有効にする

## BUGS

### C-x C-x 後すぐにIME状態が復帰しない

C-x C-xのようにキーバインドの最後がプリフィックスキーの場合、コマンド実施後に再度IMEオフ状態になってしまします。

原因：

キーイベントをポーリングで処理しているため、プリフィックスキーの処理がどうしても遅れてしまいます。C-x C-x(exchange-point-and-mark)の実行後に最後のプリフィックスキーをコマンド実行後のプリフィックスキー入力と認識してしまいIMEをオフにしてしまいます。



## 提供される関数

### 基本操作

- `(mac-ime-enable)`: イベントモニターを開始し、ポーリングタイマーを起動します。
- `(mac-ime-disable)`: イベントモニターとタイマーを停止します。

### IME操作

- `(mac-ime-get-input-source)`: 現在の入力ソースIDを取得します (例: `"com.apple.keylayout.US"`).
- `(mac-ime-set-input-source SOURCE-ID)`: 指定したIDの入力ソースに変更します。
- `(mac-ime-get-input-source-list)`: 利用可能な入力ソースIDのリストを取得します。

## カスタマイズ

- `mac-ime-prefix-keys`: IME無効化のトリガーとなるプレフィックスキーの設定。
- `mac-ime-auto-deactivate-functions`: 実行時に自動的にIMEを無効化する関数のリスト。デフォルトではミニバッファ入力時などにIMEをオフにします。
- `mac-ime-temporary-deactivate-functions`: 実行前に一時的にIMEを無効化し、コマンド終了後に元の状態に戻す関数のリスト。デフォルトでは `universal-argument` などが含まれます。

