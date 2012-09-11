-- Library for AURA output in different languages.

{- AURA TRANSLATORS - The best people ever!
Chris Warrick (Polish)
-}

module AuraLanguages where

import Data.Maybe (fromJust)

import Shell (cyan, yellow, green, red, blue)

data Language = English
              | Japanese
              | Polish
                deriving (Eq,Enum,Show)

translatorsAndLangs :: [(Language,[String])]
translatorsAndLangs = zip allLanguages translators

translators :: [[String]]
translators = [ [ "Aura Translators:"
                , " Chris \"Kwpolska\" Warrick (Polish)" ]
              , [ "Auraの翻訳者："
                , "Chris \"Kwpolska\" Warrick（ポーランド語）" ]
              , [ "Tłumacze Aury:"
                , " Chris \"Kwpolska\" Warrick (polski)" ] ]

allLanguages :: [Language]
allLanguages = [English ..]

english :: Language
english = English

japanese :: Language
japanese = Japanese

polish :: Language
polish = Polish

-- Wrap a String in backticks
bt :: String -> String
bt cs = "`" ++ cyan cs ++ "`"

--------------------
-- AuraLib functions
--------------------
mustBeRootMsg1 :: Language -> String
mustBeRootMsg1 English  = "You have to use " ++ bt "sudo" ++ " for that."
mustBeRootMsg1 Japanese = bt "sudo" ++ "を使わないとそれができない！"
mustBeRootMsg1 Polish   =
    "Musisz użyć " ++ bt "sudo" ++ ", żeby to zrobić."

buildPackagesMsg1 :: Language -> String -> String
buildPackagesMsg1 English p  = "Building " ++ bt p ++ "..."
buildPackagesMsg1 Japanese p = bt p ++ "を作成中・・・"
buildPackagesMsg1 Polish p   = "Budowanie " ++ bt p ++ "..."

checkHotEditMsg1 :: Language -> String -> String
checkHotEditMsg1 English p  =
    "Would you like to edit the PKGBUILD of " ++ bt p ++ "?"
checkHotEditMsg1 Japanese p = bt p ++ "のPKGBUILDを編成？"
checkHotEditMsg1 Polish p   = "Czy chcesz edytować PKGBUILD " ++ bt p ++ "?"

buildFailMsg1 :: Language -> String -> String
buildFailMsg1 English p  = "Well, building " ++ bt p ++ " failed."
buildFailMsg1 Japanese p = bt p ++ "の作成は失敗したようだ。"
buildFailMsg1 Polish p   =
    "Budowanie " ++ bt p ++ " zakończyło się niepowodzeniem."

buildFailMsg2 :: Language -> String
buildFailMsg2 English  = "Also, the following weren’t built:"
buildFailMsg2 Japanese = "ちなみに下記のパッケージも作成されなかった："
buildFailMsg2 Polish   =
    "Dodatkowo, następujące pakiety nie zostały zbudowane:"

buildFailMsg3 :: Language -> String
buildFailMsg3 English  = "Some packages may have built properly."
buildFailMsg3 Japanese = "今のは失敗したけど前に作成のできたやつ" ++
                             "があるかもしれない。"
buildFailMsg3 Polish   = "Niektóre pakiety mogły zostać zbudowane " ++
                         "prawidłowo."

buildFailMsg4 :: Language -> String
buildFailMsg4 English  = "Would you like to install them?"
buildFailMsg4 Japanese = "できたやつのインストールを続行する？"
buildFailMsg4 Polish   = "Czy chcesz je zainstalować?"

displayBuildErrorsMsg1 :: Language -> String
displayBuildErrorsMsg1 English  = "Dumping makepkg output in "
displayBuildErrorsMsg1 Japanese = "抑えていたmakepkgの出力を受け取る用意・・・"
displayBuildErrorsMsg1 Polish   = "Wyjście makepkg:"

getDepsToInstallMsg1 :: Language -> String
getDepsToInstallMsg1 English  = "No AUR packages specified for install."
getDepsToInstallMsg1 Japanese = "パッケージは一つも指摘されていない。"
getDepsToInstallMsg1 Polish   = "Nie podano pakietów z AUR do zainstalowania."

getRealPkgConflictsMsg1 :: Language -> String -> String -> String -> String
getRealPkgConflictsMsg1 English name rec req =
    "The dependency " ++ bt name ++ " demands version " ++ bt req ++ ",\n" ++
    "but the most recent version is " ++ bt rec ++ "."
getRealPkgConflictsMsg1 Japanese name rec req =
    "パッケージ" ++ bt name ++ "はバージョン" ++ bt req ++ "を要するが" ++
    "一番最新のバージョンは" ++ bt rec ++ "。"
getRealPkgConflictsMsg1 Polish name rec req =
    "Zależność " ++ bt name ++ " powinna być w wersji " ++ bt req ++
    ",\n" ++ "ale najnowsza wersja to " ++ bt rec ++ "."

getRealPkgConflictsMsg2 :: Language -> String -> String
getRealPkgConflictsMsg2 English p =
    bt p ++ " is an ignored package! See your `pacman.conf` file."
getRealPkgConflictsMsg2 Japanese p =
    bt p ++ "は無視されるパッケージ！`pacman.conf`を参考に。"
getRealPkgConflictsMsg2 Polish p  =
    bt p ++ " jest ignorowany! Sprawdź plik `pacman.conf`."

getVirtualConflictsMsg1 :: Language -> String -> String
getVirtualConflictsMsg1 English p  =
    bt p ++ " exists in NO WAY as a package or as one provided by another!"
getVirtualConflictsMsg1 Japanese p =
    bt p ++ "はパッケージでもないし、他のパッケージにも提供されていない！"
getVirtualConflictsMsg1 Polish p   =
    bt p ++ " nie istnieje jako pakiet lub jako pakiet dostarczany przez inny!"


getVirtualConflictsMsg2 :: Language -> String -> String -> String
getVirtualConflictsMsg2 English p pro =
    bt pro ++ " provides " ++ bt p ++ ", but " ++ bt pro ++
    " is an ignored package."
getVirtualConflictsMsg2 Japanese p pro =
    bt p ++ "は" ++ bt pro ++ "に提供されているが、" ++ bt pro ++
    "は無視されるパッケージ。"
getVirtualConflictsMsg2 Polish p pro =
    bt pro ++ " dostarcza " ++ bt p ++ ", ale " ++ bt pro ++
    " jest ignorowany."

-- Fix this up. Inconsistent variable names too.
getVirtualConflictsMsg3 :: Language -> String -> String -> String -> String ->
                           String
getVirtualConflictsMsg3 English d dVer pro proVer =
    "The dependency " ++ bt d ++ " demands version " ++ bt dVer ++
    " but its providing package " ++ bt pro ++ " gives version " ++ bt proVer
getVirtualConflictsMsg3 Japanese d dVer pro proVer =
    "仮のパッケージ" ++ bt d ++ "はバージョン" ++ bt dVer ++ "を要するが、" ++
    "それを提供する" ++ bt pro ++ "はバージョン" ++ bt proVer ++
    "しか提供しない"
getVirtualConflictsMsg3 Polish d dVer pro proVer =
    "Zależność " ++ bt d ++ " powinna być w wersji " ++ bt dVer ++
    ", ale pakiet dostarczający (" ++ bt pro ++ ") jest w wersji " ++
    bt proVer

-----------------
-- aura functions
-----------------
executeOptsMsg1 :: Language -> String
executeOptsMsg1 English  = "Conflicting flags given!"
executeOptsMsg1 Japanese = "矛盾しているオプションあり。"
executeOptsMsg1 Polish   = "Niektóre flagi są w konflikcie ze sobą!"

-- Packages should not be built if the user is logged in as root!
trueRootCheckMsg1 :: Language -> String
trueRootCheckMsg1 English  =
    "You should never build packages as the true root. Are you okay with this?"
trueRootCheckMsg1 Japanese =
    "本当のrootユーザーとしてパッケージを作成するのが危険。続行？"
trueRootCheckMsg1 Polish   =
    "Nigdy nie powinieneś budować pakietów jako root. Na pewno kontynuować?"

-- This is for when the user decides to refrain from building afterall.
trueRootCheckMsg2 :: Language -> String
trueRootCheckMsg2 English  = "You’ve done the right thing."
trueRootCheckMsg2 Japanese = "よしよし。"
trueRootCheckMsg2 Polish   = "Postąpiłeś słusznie."

installPackagesMsg1 :: Language -> String
installPackagesMsg1 English  = "Dependency checking failed for these reasons:"
installPackagesMsg1 Japanese = "従属パッケージの確認は以下の理由で失敗した："
installPackagesMsg1 Polish   =
    "Sprawdzanie zależności nie powiodło się z następujących powodów:"

installPackagesMsg2 :: Language -> String
installPackagesMsg2 English  = "No valid packages specified."
installPackagesMsg2 Japanese = "適当なパッケージを入力してください。"
installPackagesMsg2 Polish   = "Nie podano prawidłowych pakietów."

installPackagesMsg3 :: Language -> String
installPackagesMsg3 English  = "Continue?"
installPackagesMsg3 Japanese = "続行？"
installPackagesMsg3 Polish   = "Kontynuować?"

installPackagesMsg4 :: Language -> String
installPackagesMsg4 English  = "Installation manually aborted."
installPackagesMsg4 Japanese = "続行は意図的に阻止された。"
installPackagesMsg4 Polish   =
    "Instalacja została przerwana przez użytkownika."

installPackagesMsg5 :: Language -> String
installPackagesMsg5 English  = "Determining dependencies..."
installPackagesMsg5 Japanese = "従属パッケージを確認中・・・"
installPackagesMsg5 Polish   = "Ustalanie zależności..."

installPackagesMsg6 :: Language -> String
installPackagesMsg6 English  = "Building failed."
installPackagesMsg6 Japanese = "パッケージ作成は失敗した。"
installPackagesMsg6 Polish   = "Budowanie nie powiodło się."

reportNonPackagesMsg1 :: Language -> String
reportNonPackagesMsg1 English  = "The following are not packages:"
reportNonPackagesMsg1 Japanese = "下記はパッケージではない："
reportNonPackagesMsg1 Polish   = "To nie są pakiety:"

reportIgnoredPackagesMsg1 :: Language -> String
reportIgnoredPackagesMsg1 English  = "The following packages will be ignored:"
reportIgnoredPackagesMsg1 Japanese = "下記のパッケージは無視される："
reportIgnoredPackagesMsg1 Polish   = "Poniższe pakiety zostaną zignorowane:"

reportPkgsToInstallMsg1 :: Language -> String
reportPkgsToInstallMsg1 English  = "Dependencies from repositories:"
reportPkgsToInstallMsg1 Japanese = "Pacmanの従属パッケージ："
reportPkgsToInstallMsg1 Polish  = "Zależności z repozytoriów:"

reportPkgsToInstallMsg2 :: Language -> String
reportPkgsToInstallMsg2 English  = "AUR dependencies:"
reportPkgsToInstallMsg2 Japanese = "AURの従属パッケージ："
reportPkgsToInstallMsg2 Polish   = "Zależności z AUR:"

reportPkgsToInstallMsg3 :: Language -> String
reportPkgsToInstallMsg3 English  = "Main AUR packages:"
reportPkgsToInstallMsg3 Japanese = "主なAURパッケージ："
reportPkgsToInstallMsg3 Polish   = "Pakiety z AUR:"

-- Needs a Polish translation.
reportPkgsToUpgradeMsg1 :: Language -> String
reportPkgsToUpgradeMsg1 English  = "AUR Packages to upgrade:"
reportPkgsToUpgradeMsg1 Japanese = "アップグレードするAURパッケージ："
reportPkgsToUpgradeMsg1 Polish   = reportPkgsToUpgradeMsg1 English

reportBadDowngradePkgsMsg1 :: Language -> String
reportBadDowngradePkgsMsg1 English  =
    "The following aren’t installed, and thus can’t be downgraded:"
reportBadDowngradePkgsMsg1 Japanese =
    "このパッケージは最初からインストールしていないので、格下げはできない。"
reportBadDowngradePkgsMsg1 Polish   =
    "Poniższe pakeity nie są zainstalowane, i nie mogą być zainstalowane w starszej wersji:"

upgradeAURPkgsMsg1 :: Language -> String
upgradeAURPkgsMsg1 English  = "Fetching package information..."
upgradeAURPkgsMsg1 Japanese = "パッケージ情報をダウンロード中・・・"
upgradeAURPkgsMsg1 Polish   = "Pobieranie informacji o pakietach..."

upgradeAURPkgsMsg2 :: Language -> String
upgradeAURPkgsMsg2 English  = "Comparing package versions..."
upgradeAURPkgsMsg2 Japanese = "バージョンを比較中・・・"
upgradeAURPkgsMsg2 Polish   = "Porównywanie wersji pakietów..."

upgradeAURPkgsMsg3 :: Language -> String
upgradeAURPkgsMsg3 English  = "No AUR package upgrades necessary."
upgradeAURPkgsMsg3 Japanese = "アップグレードは必要ない。"
upgradeAURPkgsMsg3 Polish   = "Nie jest wymagana aktualizacja pakietów z AUR."

upgradeAURPkgsMsg4 :: Language -> String -> String
upgradeAURPkgsMsg4 English p  = "Got " ++ bt p ++ "."
upgradeAURPkgsMsg4 Japanese p = bt p ++ "、OK."
upgradeAURPkgsMsg4 Polish p   = "Otrzymano" ++ bt p ++ "."

downloadTarballsMsg1 :: Language -> String -> String
downloadTarballsMsg1 English p  =
    "Downloading " ++ bt p ++ " source tarball..."
downloadTarballsMsg1 Japanese p =
    bt p ++ "のソースコードのターボールをダウンロード中・・・"
downloadTarballsMsg1 Polish p   =
    "Pobieranie paczki źródłowej " ++ bt p ++ "..."

displayPkgbuildMsg1 :: Language -> String -> String
displayPkgbuildMsg1 English pkg  = bt pkg ++ " does not exist."
displayPkgbuildMsg1 Japanese pkg = bt pkg ++ "は存在しない。"
displayPkgbuildMsg1 Polish pkg   = bt pkg ++ " nie istnieje."

removeMakeDepsAfterMsg1 :: Language -> String
removeMakeDepsAfterMsg1 English  = "Removing unneeded make dependencies..."
removeMakeDepsAfterMsg1 Japanese = "あと片付け。必要ないパッケージを削除："
removeMakeDepsAfterMsg1 Polish   =
    "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."

getDowngradeChoiceMsg1 :: Language -> String -> String
getDowngradeChoiceMsg1 English p =
    "What version of " ++ bt p ++ " do you want?"
getDowngradeChoiceMsg1 Japanese p =
    bt p ++ "はどのバージョンにする？"
getDowngradeChoiceMsg1 Polish p   =
    "Którą wersję pakietu " ++ bt p ++ " zainstalować?"

backupCacheMsg1 :: Language -> String
backupCacheMsg1 English  = "No backup location given."
backupCacheMsg1 Japanese = "バックアップ先を入力してください。"
backupCacheMsg1 Polish   = "Nie podano lokalizacji kopii zapasowych."

backupCacheMsg2 :: Language -> String
backupCacheMsg2 English  = "You must be root to backup the cache."
backupCacheMsg2 Japanese = "rootじゃないとバックアップはできない。"
backupCacheMsg2 Polish   = "Musisz być rootem, by zrobić kopię zapasową pamięci podręcznej."

backupCacheMsg3 :: Language -> String
backupCacheMsg3 English  = "The backup location does not exist."
backupCacheMsg3 Japanese = "バックアップ先は存在しない。"
backupCacheMsg3 Polish  = "Lokalizacja kopii zapasowych nie istnieje."

backupCacheMsg4 :: Language -> String -> String
backupCacheMsg4 English dir  = "Backing up cache to " ++ bt dir
backupCacheMsg4 Japanese dir = "キャッシュのバックアップ先：" ++ bt dir
backupCacheMsg4 Polish dir   = "Tworzenie kopii zapasowej pamięci podręcznej w " ++ bt dir

backupCacheMsg5 :: Language -> Int -> String
backupCacheMsg5 English n  = "Package files to backup: " ++ bt (show n)
backupCacheMsg5 Japanese n = "パッケージのファイル数：" ++ bt (show n)
backupCacheMsg5 Polish n   = "Pliki będące częścią kopii zapasowej: " ++ bt (show n)

backupCacheMsg6 :: Language -> String
backupCacheMsg6 English  = "Proceed with backup?"
backupCacheMsg6 Japanese = "バックアップを実行する？"
backupCacheMsg6 Polish   = "Kontynuować tworzenie kopii zapasowej?"

backupCacheMsg7 :: Language -> String
backupCacheMsg7 English  = "Backup manually aborted."
backupCacheMsg7 Japanese = "バックアップは意図的に阻止された。"
backupCacheMsg7 Polish   = "Tworzenie kopii zapasowej zostało przerwane przez użytkownika."

backupCacheMsg8 :: Language -> String
backupCacheMsg8 English  = "Backing up. This may take a few minutes..."
backupCacheMsg8 Japanese = "バックアップ中。数分かかるかもしれない。"
backupCacheMsg8 Polish   = "Tworzenie kopii zapasowej. To może potrwać kilka minut..."

copyAndNotifyMsg1 :: Language -> Int -> String
copyAndNotifyMsg1 English n  = "Copying #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Japanese n = "#[" ++ cyan (show n) ++"]をコピー中・・・"
copyAndNotifyMsg1 Polish n   = "Kopiowanie #[" ++ cyan (show n) ++ "]"

preCleanCacheMsg1 :: Language -> String -> String
preCleanCacheMsg1 English n  = bt n ++ " is not a number."
preCleanCacheMsg1 Japanese n = bt n ++ "は数字はない。"
preCleanCacheMsg1 Polish n   = bt n ++ " nie jest liczbą."

cleanCacheMsg1 :: Language -> String
cleanCacheMsg1 English  = "Invalid number given."
cleanCacheMsg1 Japanese = "入力の数字は適切ではない。"
cleanCacheMsg1 Polish  = "Nieprawidłowa liczba."

cleanCacheMsg2 :: Language -> String
cleanCacheMsg2 English  = "This will delete the ENTIRE package cache."
cleanCacheMsg2 Japanese = "パッケージ・キャッシュは完全に削除される。"
cleanCacheMsg2 Polish   = "To usunie WSZYSTKIE pliki z pamięci podręcznej."

cleanCacheMsg3 :: Language -> Int -> String
cleanCacheMsg3 English n  = bt (show n) ++ " of each package file will be kept."
cleanCacheMsg3 Japanese n = "パッケージ・ファイルは" ++ bt (show n) ++
                            "個保存される。"
cleanCacheMsg3 Polish n   = bt (show n) ++ " wersji każdego pakietu zostanie zachowane."

cleanCacheMsg4 :: Language -> String
cleanCacheMsg4 English  = "The rest will be deleted. Okay?"
cleanCacheMsg4 Japanese = "残りは全部削除される。承知する？"
cleanCacheMsg4 Polish   = "Wszystko inne zostanie usunięte. Na pewno?"

cleanCacheMsg5 :: Language -> String
cleanCacheMsg5 English  = "Cache cleaning manually aborted."
cleanCacheMsg5 Japanese = "削除の続行は意図的に阻止された。"
cleanCacheMsg5 Polish   = "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."

cleanCacheMsg6 :: Language -> String
cleanCacheMsg6 English  = "Cleaning package cache..."
cleanCacheMsg6 Japanese = "パッケージ・キャッシュを掃除中・・・"
cleanCacheMsg6 Polish   = "Czyszczenie pamięci podręcznej..."

logLookUpMsg1 :: Language -> String -> String
logLookUpMsg1 English p  = yellow "Package" ++ "        : " ++ p
logLookUpMsg1 Japanese p = yellow "パッケージ" ++ "　　　　　：" ++ p
logLookUpMsg1 Polish p   = yellow "Pakiet" ++ "         : " ++ p

logLookUpMsg2 :: Language -> String -> String
logLookUpMsg2 English date  = yellow "First Install" ++ "  : " ++ date
logLookUpMsg2 Japanese date = yellow "初インストール" ++ "　　　：" ++ date
logLookUpMsg2 Polish date   = yellow "Pierwsza instalacja" ++ ": " ++ date

logLookUpMsg3 :: Language -> Int -> String
logLookUpMsg3 English upgrades  =
    yellow "Upgrades" ++ "       : " ++ show upgrades
logLookUpMsg3 Japanese upgrades =
    yellow "アップグレード回数" ++ "　：" ++ show upgrades
logLookUpMsg3 Polish upgrades   =
    yellow "Aktualizacje" ++ "   : " ++ show upgrades

logLookUpMsg4 :: Language -> String
logLookUpMsg4 English  = yellow "Recent Actions" ++ " :"
logLookUpMsg4 Japanese = yellow "近況" ++ "　　　　　　　　："
logLookUpMsg4 Polish   = yellow "Ostatnie akcje" ++ " :"

reportNotInLogMsg1 :: Language -> String
reportNotInLogMsg1 English  = "These have not appeared in the log file:"
reportNotInLogMsg1 Japanese = "logファイルには出ていない："
reportNotInLogMsg1 Polish   = "Tych pakietów nie ma w dzienniku:"

manpageMsg :: Language -> String
manpageMsg English  = "See the aura man page for aura option details."
manpageMsg Japanese = "選択肢の詳しいことは、auraのman pageまで。"
manpageMsg Polish   =
    "W podręczniku man dla aura znajduje się więcej informacji o opcjach Aury."

displayOutputLanguagesMsg1 :: Language -> String
displayOutputLanguagesMsg1 English  = "The following languages are available:"
displayOutputLanguagesMsg1 Japanese = "auraは下記の言語に対応している："
displayOutputLanguagesMsg1 Polish   = "Następujące języki są dostępne:"

-- The `lookup` will never fail.
translatorMsg :: Language -> [String]
translatorMsg lang = fromJust $ lookup lang translatorsAndLangs

----------------------
-- AuraFlags functions
----------------------
inheritedOperTitle :: Language -> String
inheritedOperTitle English  = "Inherited Pacman Operations"
inheritedOperTitle Japanese = "Pacmanからの引継選択肢"
inheritedOperTitle Polish   = "Operacje z Pacmana"

auraOperTitle :: Language -> String
auraOperTitle English  = "Aura Only Operations:"
auraOperTitle Japanese = "Auraだけの選択肢："
auraOperTitle Polish  = "Operacje Aury:"

aurSy :: Language -> String
aurSy English  = green "Perform actions involving the [A]UR.\n" ++
                 "Default action installs from the AUR."
aurSy Japanese = green "[A]URに関連する処理\n" ++
                 "デフォルトでAURからインストール"
aurSy Polish   = green "Wykonuje akcje związane z [A]UR.\n" ++
                 "Domyślnie instaluje pakiety z AUR."

downG :: Language -> String
downG English  = red "Perform actions involving the package [C]ache.\n" ++
                 "Default action downgrades given packages."
downG Japanese = red "キャッシュに関連する処理\n" ++
                 "デフォルトでパッケージをダウングレード"
downG Polish   = red "Wykonuje akcje związane z pamięcią podręczną ([C]ache) pakietów.\n" ++
                 "Domyślnie instaluje starsze wersje podanych pakietów."

viewL :: Language -> String
viewL English  = cyan "Perform actions involving the pacman [L]ogfile.\n" ++
                 "Default action opens the log for read-only viewing."
viewL Japanese = cyan "[L]ogfileに関連する処理\n" ++
                 "デフォルトでlogfileを閲覧用に開く"
viewL Polish   =
    cyan "Wykonuje akcje związane z dziennikiem ([L]ogiem) pacmana.\n" ++
    "Domyślnie otwiera log w trybie tylko do odczytu."

orpha :: Language -> String
orpha English  = blue "Perform actions involving [O]rphan packages.\n" ++
                 "Default action lists all orphan packages."
orpha Japanese = blue "必要とされていない従属パッケージに関する処理\n" ++
                 "デフォルトでその従属パッケージの名前を出力"
orpha Polish   =
    blue "Wykonuje akcje związane z [O]sieroconymi pakietami.\n" ++
    "Domyślnie wyświetla wszystkie osierocone pakiety."
