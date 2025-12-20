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

Appleシリコン搭載のMacであればリポジトリ内のダイナミックモジュール (`.so` ファイル)が使えるのでビルドは不要です。

必要であれば以下の手順で作成してください。

1. リポジトリをクローンします。

```bash
git clone https://github.com/ma0001/mac-ime.git
cd mac-ime
```

2. `make` コマンドを実行してモジュールをビルドします。

```bash
make
```
成功すると `mac-ime-module.so` が生成されます。

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
上記設定が完了したら、C-\ (toggle-input-method) や cmd-space などで日本語入力状態にした状態でC-xなどのプリフィックスキーを入力するとRoman入力に切り替わり、コマンド実行後は元に戻ります。

ミニバッファへの入力時にRomen入力となり入力後に元に戻すようにするため、mac-ime-auto-deactivate-functionsに指定してある関数では、関数実行前にRoman入力とし関数実行後に戻す処理を追加しています。mac-ime-auto-deactivate-functionsの設定を変更することにより動作させる関数を変えることができます。

また、C-uのようにコマンド実行後のキーでRoman入力として次のコマンド実行前に戻す必要があるものについては、変数mac-ime-temporary-deactivate-functionsに指定しています。（universal-argumentではC-u後の最初のキー入力しかRomanとならないため、その後の数字入力で繰り返し呼ばれるuniversal-argument--modeを登録しています）

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

本モジュールではmacOSの入力ソースがRomanなのか日本語などの非Romanなのかを判定する必要があるためinput source IDを正規表現を使って判定を行っています。もし特殊なインプットメソッドを使っていてこの判断が正しく動作しない場合はmac-ime-no-ime-input-source-regexpで正しくRomanを判断できるように設定してください。現在使用可能なinput source IDは(mac-ime-get-input-source-list)で取得できます。


## 提供される関数

### 基本操作

- `(mac-ime-enable)`: イベントモニターを開始し、ポーリングタイマーを起動します。
- `(mac-ime-disable)`: イベントモニターとタイマーを停止します。

### IME操作

- `(mac-ime-get-input-source)`: 現在の入力ソースIDを取得します (例: `"com.apple.keylayout.US"`).
- `(mac-ime-set-input-source SOURCE-ID)`: 指定したIDの入力ソースに変更します。
- `(mac-ime-get-input-source-list)`: 利用可能な入力ソースIDのリストを取得します。

## カスタマイズ

- `mac-ime-poll-interval`: イベントをポーリングする間隔（秒）。デフォルトは `0.05` です。
- `mac-ime-functions`: キーイベント発生時に呼び出されるフック。引数として `(keycode modifiers)` を受け取ります。
- `mac-ime-prefix-keys`: IME無効化のトリガーとなるプレフィックスキーの設定。

