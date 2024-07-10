language-name = 日本語

# AUR Packages (-A)
A-install-deps = 依存パッケージを確認中…
A-install-repo-pkgs = Pacmanの依存パッケージ：
A-install-aur-pkgs = AURのパッケージ:
A-install-path-comp = { $path }の最後のパス・コンポネントを抽出できなかった。
A-install-ignored = { $file } は「無視パッケージ」と指定されている。それでも続行する？

A-build-prep = ビルド・フォルダを準備中…
A-build-pkg = { $pkg } をビルド中・・・
A-build-diff = ビルドファイルの差分を表示？
A-build-hotedit-pkgbuild = PKGBUILDを編集？
A-build-hotedit-install = .installを編集？
A-build-fail = ビルドが失敗した。
A-build-e-pkgctl = chroot環境でのビルドが失敗した。
A-build-e-makepkg = makepkgが失敗した。
A-build-e-edit = { $file } の編集が失敗した。
A-build-e-tarball = { $file } の移動が失敗した。
A-build-e-filename = { $file } のファイル名を抽出できなかった。
A-build-e-copies = ビルドファイルをコピーできなかった。
A-build-pkglist = { $dir } から、makepkgの出力ファイルを発見できなった。
A-build-pull = 最新のコミットをダウンロードできなった。やや古いバージョンがビルドされる可能性あり！
A-build-continue = それでも続行する？

A-i-repo = リポジトリ
A-i-version = バージョン
A-i-status = パッケージ状態
A-i-maintainer = 管理者
A-i-proj-url = プロジェクト
A-i-aur-url = パッケージページ
A-i-license = ライセンス
A-i-group = グループ
A-i-provides = 供給
A-i-depends = 従属パッケージ
A-i-make = コンパイル時の
A-i-opt = 任意の
A-i-check = テスト用の
A-i-votes = 投票数
A-i-pop = 人気
A-i-desc = 概要
A-i-keywords = キーワード
A-i-submitted = 最初投稿
A-i-updated = 更新

A-u-fetch-info = パッケージ情報をダウンロード中…
A-u-comparing = バージョンを比較中…
A-u-no-upgrades = アップグレードは必要ない。
A-u-to-upgrade = アップグレードするAURパッケージ：
A-u-git = 再ビルドするVCSパッケージ:

A-w = git clone中…

A-y-refreshing = ローカルAURパッケージのgit cloneを更新中…
A-y = 以下のパッケージをpullできなかった：

# Snapshots (-B)
B-saved = パッケージ状態の保存に成功した。
B-clean = 古いスナップショットを削除？
B-none = 使えるスナップショットが見つからない。
B-select = 復帰するスナップショットを選択：

# Cache (-C)
C-size = 現在のキャッシュの大きさ: { $size }

C-b-file = { $target } が既存してフォルダではない。
C-b-nonempty = { $target } は存在するが空ではない！
C-b-target = キャッシュのバックアップ先： { $target }
C-b-curr = pwdの読み取りに失敗した。

C-i-latest = 最新
C-i-created = 作成時刻
C-i-installed = インストール時刻
C-i-sig = シグネチャ
C-i-size = アーカイブの大きさ
C-i-avail = 使えるバージョン

C-c-keep = パッケージ・ファイルは { $pkgs } 個保存されます。 残りは全部削除されます。
C-c-freed = { $bytes } を削除した。

C-downgrade-which = { $pkg } のどのバージョンにする？

C-y-no-work = キャッシュは既に最新。
C-y-which-cache = アーカイブファイルをどのキャッシュにダウンロードする？
C-t-invalids = 不要なアーカイブを削除中…

# Logs (-L)
L-first = 初インストール
L-upgrades = アップグレード回数
L-recent = 近況
L-search-err = { $cmd } でのログの検索が失敗した。
L-view-err = ALPMログを開けなかった。

# Orphans (-O)
O-adopt = { $pkg } は「explicitly installed」状態にした。
O-explicit-err = { $pkg } を「explicitly installed」にできなかった。

# Opening Pages (open)
open-err = { $url }を開けなかった。

# System Statistics (stats)
stats-local = 言語データをロードできなかった。
stats-host = ホスト
stats-user = ユーザー
stats-distro = ディストロ
stats-editor = エディタ
stats-pkgs = インストール済みパッケージ数
stats-aura-cache = 専用キャッシュ
stats-pacman-cache = システムキャッシュ
stats-aura-build = ビルドキャッシュ
stats-tmp = /tmp

# System Validation (check)
check-start = システムを検査中…
check-missing-exec = 修正案： { $exec } をインストールする事。
check-env = 環境
check-env-editor = EDITOR は設定されている？
check-env-editor-exec = EDITOR ({ $exec }) は実行できる？
check-env-editor-vi = 予備の vi が実行できる？
check-env-installed = { $exec } はインストールされて実行できる？
check-env-lang = { $cmd } の出力に LANG の値が見つかる? ({ $lang })
check-env-lang-fix = 修正案： { $lang } が含まれるように { $file } を更新する事。
check-env-lang-fix2 = 修正案: LANG を設定する事!
check-env-lang-known = Auraには、 LANG に合う翻訳がある？
check-env-java-bin = Java のツールなど、インストール済み？
check-env-java-bin-fix = 修正案： { $pkg } のインストールを検討する事。
check-env-java-set = Java 環境は設定済み？
check-env-java-set-fix = 修正案： { $cmd } を参考。
check-pconf = Pacman 設定 (/etc/pacman.conf)
check-pconf-par = 並行ダウンロードは設定済み？
check-pconf-par-fix = 修正案： { $setting } はオフ、もしくは「１」に設定されている。 { $set } を設定するとターボールのダウンロードが速くなる。
check-pconf-ignores = 「無視パッケージ」が重なっていない？
check-pconf-ignores-fix = 以下のパッケージが pacman.conf でも Aura の設定でも「無視」にされている：
check-pconf-pacnew = .pacnew ファイルは更新されている？
check-pconf-pacnew-broken = Error: { $fd } の実行が完全に失敗した。
check-pconf-pacnew-old = { $path } は .pacnew より { $days } 日間古い。

check-aconf = Aura 設定
check-aconf-aura-exists = Aura の設定ファイルが存在する？
check-aconf-aura-exists-fix = 修正案: { $cmd } を検討する事。
check-aconf-aura-parse = Aura の設定ファイルはパースできる？
check-aconf-old-dirs = Aura の前バージョンのフォルダが残っていないか？
check-aconf-old-conf = Aura の前バージョンの設定ファイルが残っていないか？
check-mconf = Makepkg 設定 (/etc/makepkg.conf)
check-mconf-packager = PACKAGER は設定済み？
check-mconf-packager-fix = 修正案： /etc/makepkg.conf 内で { $cmd } を設定する事。
check-snapshots = パッケージ・スナップショット
check-snapshot-usable = 全 snapshots のターボールが手元にある？
check-snapshot-usable-fix = 修正案: { $command } で古い・使えないスナップショットを削除する事。
check-cache = ターボール・キャッシュ
check-cache-exists = 全キャッシュは実在する？
check-cache-tarballs = 手元のターボールは全部有効？
check-cache-tarballs-fix = 修正案: { $command } で壊れたターボールを削除する事。
check-cache-missing = インストール済みの全システム・パッケージに相当するターボールがキャッシュで見つかる？
check-cache-missing-fix = 修正案: { $command } で足りていないターボールを再ダウンロードする事。
check-cache-missing-for = インストール済みの全AURパッケージに相当するターボールがキャッシュで見つかる？
check-cache-missing-for-fix = 修正案: { $cmd } で足りていないものを確認し手動で再ダウンロードする事。
check-pkgs = パッケージ状態
check-pkgs-old = 依存ではないパッケージが最近更新されている？
check-pkgs-old-warn = { $pkg } が最後更新されたのは { $days } 日前。

# Thanks
thanks-you = Aura のご利用、誠にありがとうございます。
thanks-colin = Aura by Colin Woodbury, 2012 - 2024
thanks-pacman = しっかりした基盤を作って下さって、 Pacman と Arch のチームに感謝。
thanks-everyone = Auraの貢献者、寄付者、ユーザーにも、ありがとうございます。
thanks-logo = Aura のロゴは Cristiano Vitorino 作。
thanks-translators = Aura の翻訳者：

# Configuration (conf)
conf-toml-err = 現在の設定をなぜか書き込めなかった。

# Dependencies (deps)
deps-io = 依存画像を作成できなかった。

# Runtime Environment
env-missing-editor = EDITOR はパスにない。
env-pconf = pacman.conf をパースできなかった。

# Pacman Calls
pacman-external = pacman の呼び出しが完全に失敗した。
pacman-u = pacman -U が失敗した。
pacman-s = pacman -S が失敗した。
pacman-misc = ある pacman の呼び出しがなぜか失敗した。

# Aura-specific Directories
dir-mkdir = { $dir } が新しく作れなkった。
dir-home = Aura の設定フォルダを特定できなかった。
dir-cache = Aura のキャッシュフォルダを特定できなかった。

# Dependency Resolution
dep-exist = { $pkg } は存在しない。
dep-exist-par = { $par } の依存 { $pkg } は存在しない。
dep-graph = 依存グラフはなぜか妙な形になっている。
dep-cycle = 依存サイクルが発見された： { $cycle }
dep-multi = 依存計算で複数のエラーが発生した。

# Git Operations
git-diff = git diff が失敗した： { $file }
git-hash = gitのハッシュの読み込みが失敗した。
git-pull = git pull が失敗した： { $dir }
git-clone = git clone が失敗した： { $dir }
git-io = git の呼び出しがなぜか失敗した。

# Faur Calls
faur-fetch = メタデータのサーバーの呼び出しが完全に失敗した： { $pkg }
faur-unknown = 未知のパッケージ： { $pkg }
faur-too-many = 予想より Faur から結果がですぎた： { $pkg }

# Common Errors
err-alpm = ALPM のハンドルを開けなかった。
err-config-path = Aura の設定ファイルの在処を特定できなった。
err-curl = CURL がなぜか失敗した: { $err }
err-file-del = 削除に失敗した： { $file }
err-file-open = ファイルとして開けなかった： { $file }
err-file-write = 書き込みが失敗した： { $file }
err-json-decode = JSON の読み込みが失敗した： { $url }
err-json-write = JSON の書き込みが失敗した： { $file }
err-mutex = ある Mutex が腐敗してしまった。
err-none-exist = 指定されたパッケージのどれも存在しない。
err-pool-create = ALPM の接続プールを作れなかった。
err-pool-get = 接続プールからハンドルをもらえなかった。
err-read-dir = フォルダの読み込みが失敗した： { $dir }
err-srcinfo = .SRCINFO のパースが失敗した： { $file }
err-sudo = 権限を上げられなかった。
err-time-conv = タイムスタンプを変換できなかった。
err-time-format = 時刻の文字列のフォーマットが失敗した。
err-user-input = ユーザーの入力がなぜか失敗した。
err-utf8 = UTF-8 変換が失敗した。
err-write = stdout への書き込みがなぜか失敗した。

# Common Fields
common-yes = はい
common-no = いいえ
common-name = 名前
common-done = 完了。
common-total = 合計
common-no-packages = パッケージは何も指定されていない。
common-no-work = やる事は何もない。
common-cancelled = 取り消し。
common-replace = { $new } を代わりに { $old } は削除可能。

# Misc.
proceed = 実行？
proceed-affirmative = y
proceed-affirmative-alt = Y
proceed-negative = n
