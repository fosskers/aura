-- Library for AURA output in different languages.

module AuraLanguages where

data Language = English | Japanese deriving (Eq,Enum,Show)

allLanguages :: [Language]
allLanguages = [English ..]

english :: Language
english = English

japanese :: Language
japanese = Japanese

-- Backticks
bt :: String -> String
bt cs = "`" ++ cs ++ "`"

-- AuraLib functions
buildPackagesMsg1 :: Language -> String -> String
buildPackagesMsg1 English p  = "Building " ++ bt p ++ "..."
buildPackagesMsg1 Japanese p = p ++ "を作成中"

buildPackagesMsg2 :: Language -> String -> String
buildPackagesMsg2 English p  = "Well, building " ++ bt p ++ " failed."
buildPackagesMsg2 Japanese p = bt p ++ "の作成は失敗したようだ。"

buildPackagesMsg3 :: Language -> String
buildPackagesMsg3 English  = "Dumping makepkg output in "
buildPackagesMsg3 Japanese = "抑えていたmakepkgの出力を受け取る用意・・・"

buildPackagesMsg4 :: Language -> String
buildPackagesMsg4 English  = "Also, the following weren't built:"
buildPackagesMsg4 Japanese = "ちなみに下記のパッケージも作成されなかった："

buildPackagesMsg5 :: Language -> String
buildPackagesMsg5 English  = "Some packages may have built properly."
buildPackagesMsg5 Japanese = "今のは失敗したけど前に作成のできたやつ" ++
                             "があるかもしれない。"

buildPackagesMsg6 :: Language -> String
buildPackagesMsg6 English  = "Would you like to install them? [y/n]"
buildPackagesMsg6 Japanese = "できたやつのインストールを続行する？ [y/n]"

buildPackagesMsg7 :: Language -> String
buildPackagesMsg7 English  = "So be it."
buildPackagesMsg7 Japanese = "分かった。脱出！"

getRealPkgConflictsMsg1 :: Language -> String -> String -> String -> String
getRealPkgConflictsMsg1 English name rec req =
    "The dependency " ++ bt name ++ " demands version " ++ bt req ++ ",\n" ++
    "but the most recent version is " ++ bt rec ++ "."
getRealPkgConflictsMsg1 Japanese name rec req =
    "パッケージ" ++ bt name ++ "はバージョン" ++ bt req ++ "を要するが" ++
    "一番最新のバージョンは" ++ bt rec ++ "。"

getRealPkgConflictsMsg2 :: Language -> String -> String
getRealPkgConflictsMsg2 English p = 
    bt p ++ " is an ignored package! See your `pacman.conf` file."
getRealPkgConflictsMsg2 Japanese p =
    bt p ++ "は無視されるパッケージ！`pacman.conf`を参考に。"

getVirtualConflictsMsg1 :: Language -> String -> String
getVirtualConflictsMsg1 English p =
    bt p ++ " exists in NO WAY as a package or as one provided by another!"
getVirtualConflictsMsg1 Japanese p =
    bt p ++ "はパッケージでもないし、他のパッケージにも提供されていない！"
                                     
getVirtualConflictsMsg2 :: Language -> String -> String -> String
getVirtualConflictsMsg2 English p pro =
    bt pro ++ " provides " ++ bt p ++ ", but " ++ bt pro ++
    " is an ignored package."
getVirtualConflictsMsg2 Japanese p pro =
    bt p ++ "は" ++ bt pro ++ "に提供されているが、" ++ bt pro ++
    "は無視されるパッケージ。"

reportNonPackagesMsg1 :: Language -> String
reportNonPackagesMsg1 English  = "The following are not packages:"
reportNonPackagesMsg1 Japanese = "下記はパッケージではない："

reportIgnoredPackagesMsg1 :: Language -> String
reportIgnoredPackagesMsg1 English  = "The following packages will be ignored:"
reportIgnoredPackagesMsg1 Japanese = "下記のパッケージは無視される："

-- aura functions
executeOptsMsg1 :: Language -> String
executeOptsMsg1 English  = "Conflicting flags given!"
executeOptsMsg1 Japanese = "矛盾しているオプションあり。"

installPackagesMsg1 :: Language -> String
installPackagesMsg1 English  = "Dependency checking failed for these reasons:"
installPackagesMsg1 Japanese = "Dependenciesの確認は以下の理由で失敗した："

displayPkgbuildMsg1 :: Language -> String
displayPkgbuildMsg1 English  = "Make sure to thoroughly check PKGBUILDs " ++
                               "before installing packages."
displayPkgbuildMsg1 Japanese = "パッケージをインストールする前は必ず" ++
                               "PKGBUILDの内容を確認した方がいい。"

displayPkgbuildMsg2 :: Language -> String -> String
displayPkgbuildMsg2 English pkg  = "Showing PKGBUILD for " ++ bt pkg ++ "..."
displayPkgbuildMsg2 Japanese pkg = bt pkg ++ "のPKGBUILDは出力される。"

displayPkgbuildMsg3 :: Language -> String -> String
displayPkgbuildMsg3 English pkg  = bt pkg ++ " does not exist."
displayPkgbuildMsg3 Japanese pkg = bt pkg ++ "は存在しない。"

displayOutputLanguagesMsg1 :: Language -> String
displayOutputLanguagesMsg1 English = "The following languages are available:"
displayOutputLanguagesMsg1 Japanese = "auraは以下の言語に対応している："

-- Pacman functions
syncDatabaseMsg1 :: Language -> String
syncDatabaseMsg1 English  = "Syncing package database..."
syncDatabaseMsg1 Japanese = "パッケージのデータベースを同期・・・"