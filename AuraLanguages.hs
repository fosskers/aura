-- Library for AURA output in different languages.

module AuraLanguages where

data Language = Eng | Jap deriving (Eq)

english :: Language
english = Eng

japanese :: Language
japanese = Jap

buildPackagesMsg1 :: Language -> String -> String
buildPackagesMsg1 Eng p = "Building `" ++ p ++ "`..."
buildPackagesMsg1 Jap p = p ++ "を作成中"

buildPackagesMsg2 :: Language -> String -> String
buildPackagesMsg2 Eng p = "Well, building " ++ p ++ " failed."
buildPackagesMsg2 Jap p = p ++ "の作成は失敗したようだ。"

buildPackagesMsg3 :: Language -> String
buildPackagesMsg3 Eng = "Dumping makepkg output in "
buildPackagesMsg3 Jap = "抑えていたmakepkgの出力を受け取る用意・・・"

buildPackagesMsg4 :: Language -> String
buildPackagesMsg4 Eng = "Also, the following weren't built:"
buildPackagesMsg4 Jap = "ちなみに下記のパッケージも作成されなかった："

buildPackagesMsg5 :: Language -> String
buildPackagesMsg5 Eng = "Some packages may have built properly."
buildPackagesMsg5 Jap = "今のは失敗したけど前に作成のできたやつ" ++
                        "があるかもしれない。"

buildPackagesMsg6 :: Language -> String
buildPackagesMsg6 Eng = "Would you like to install them? [y/n]"
buildPackagesMsg6 Jap = "できたやつのインストールを続行する？ [y/n]"

buildPackagesMsg7 :: Language -> String
buildPackagesMsg7 Eng = "So be it."
buildPackagesMsg7 Jap = "分かった。脱出！"

handleNonPackagesMsg1 :: Language -> String
handleNonPackagesMsg1 Eng = "The following are not packages:"
handleNonPackagesMsg1 Jap = "下記はパッケージではない："