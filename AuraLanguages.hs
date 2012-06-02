-- Library for AURA output in different languages.

module AuraLanguages where

data Language = English | Japanese deriving (Eq,Enum,Show)

allLanguages :: [Language]
allLanguages = [English ..]

english :: Language
english = English

japanese :: Language
japanese = Japanese

-- AURPackages functions
buildPackagesMsg1 :: Language -> String -> String
buildPackagesMsg1 English p  = "Building `" ++ p ++ "`..."
buildPackagesMsg1 Japanese p = p ++ "を作成中"

buildPackagesMsg2 :: Language -> String -> String
buildPackagesMsg2 English p  = "Well, building " ++ p ++ " failed."
buildPackagesMsg2 Japanese p = p ++ "の作成は失敗したようだ。"

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

handleNonPackagesMsg1 :: Language -> String
handleNonPackagesMsg1 English  = "The following are not packages:"
handleNonPackagesMsg1 Japanese = "下記はパッケージではない："

-- aura functions
displayPkgbuildMsg1 :: Language -> String
displayPkgbuildMsg1 English  = "Make sure to thoroughly check PKGBUILDs " ++
                               "before installing packages."
displayPkgbuildMsg1 Japanese = "パッケージをインストールする前は必ず" ++
                               "PKGBUILDの内容を確認した方がいい。"

displayPkgbuildMsg2 :: Language -> String -> String
displayPkgbuildMsg2 English pkg  = "Showing PKGBUILD for `" ++ pkg ++ "`..."
displayPkgbuildMsg2 Japanese pkg = pkg ++ "のPKGBUILDは出力される。"

displayPkgbuildMsg3 :: Language -> String
displayPkgbuildMsg3 English  = "That package does not exist."
displayPkgbuildMsg3 Japanese = "そのパッケージは存在しない。"

displayOutputLanguagesMsg1 :: Language -> String
displayOutputLanguagesMsg1 English  = "The following languages are available:"
displayOutputLanguagesMsg1 Japanese = "auraは以下の言語に対応している："