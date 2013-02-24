-- Library for Aura output in different languages.
-- All normal restrictions on line length do not apply for this file, and this file only.

{- AURA TRANSLATORS - The best people ever!
Chris "Kwpolska" Warrick | Polish
Denis Kasak              | Croatian
Fredrik Haikarainen      | Swedish
Lukas Niederbremer       | German
Alejandro Gómez          | Spanish
Henry "Ingvij" Kupty     | Portuguese
Ma Jiehong               | French
Kyrylo Silin             | Russian
Bob Valantin             | Italian
Filip Brcic              | Serbian
-}

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Languages where

import Aura.Colour.Text (cyan, green, red, blue, yellow)

---

data Language = English
              | Japanese
              | Polish
              | Croatian
              | Swedish
              | German
              | Spanish
              | Portuguese
              | French
              | Russian
              | Italian
              | Serbian
                deriving (Eq,Enum,Read,Show)

translators :: [String]
translators = [ " Chris \"Kwpolska\" Warrick"
              , " Denis Kasak"
              , " Fredrik Haikarainen"
              , " Lukas Niederbremer"
              , " Alejandro Gómez"
              , " Henry \"Ingvij\" Kupty" 
              , " Ma Jiehong"
              , " Kyrylo Silin" 
              , " Bob Valantin"
              , " Filip Brcic" ]

-- These need updating!
languageNames :: Language -> [String]
languageNames English    = [ "Polish","Croatian","Swedish","German","Spanish","Portuguese","French","Russian", "Italian", "Serbian" ]
languageNames Japanese   = [ "ポーランド語","クロアチア語","スウェーデン語","ドイツ語","スペイン語","ポルトガル語","フランス語","ロシア語", "", "" ]
languageNames Polish     = [ "polski","chorwacki","szwedzki","niemiecki","hiszpański","portugalski","francuski","rosyjski", "", "" ]
languageNames Croatian   = [ "poljski","hrvatski","švedski","njemački","španjolski","portugalski","francuski","ruski", "", "" ]
languageNames Swedish    = [ "polska","kroatiska","svenska","tyska","spanska","portugisiska", "", "" ]
languageNames German     = [ "Polnisch","Kroatisch","Schwedisch","Deutsch","Spanisch","Portugiesisch", "", "" ]
languageNames Spanish    = [ "Polaco","Croata","Sueco","Alemán","Español","Portugués", "", "" ]
languageNames Portuguese = [ "Polonês","Croata","Sueco","Alemão","Espanhol","Português", "", "" ]
languageNames French     = [ "Polonais","Croate","Suedois","Alemand","Espagnol","Portugais", "Français", "Russe", "", "" ]
languageNames Russian    = [ "Польский","Хорватский","Шведский","Немецкий","Испанский","Португальский", "Русский", "", "" ]
languageNames Italian    = [ "Polacco", "Croato", "Svedese", "Tedesco", "Spagnolo", "Portoghese", "Francese", "Russo", "Italiano", "" ]
languageNames Serbian    = [ "Пољски","Хрватски","Шведски","Немачки","Шпански","Португалски","Француски","Руски","Италијански","Српски" ]

translatorMsgTitle :: Language -> String
translatorMsgTitle English    = "Aura Translators:"
translatorMsgTitle Japanese   = "Auraの翻訳者："
translatorMsgTitle Polish     = "Tłumacze Aury:"
translatorMsgTitle Croatian   = "Aura Prevoditelji:"
translatorMsgTitle Swedish    = "Aura Översättare:"
translatorMsgTitle German     = "Aura Übersetzer:"
translatorMsgTitle Spanish    = "Traductores de Aura:"
translatorMsgTitle Portuguese = "Tradutores de Aura:"
translatorMsgTitle French     = "Traduction d'Aura :"
translatorMsgTitle Russian    = "Переводчики Aura:"
translatorMsgTitle Italian    = "Traduttori di Aura:"
translatorMsgTitle Serbian    = "Преводиоци Аура:"

translatorMsg :: Language -> [String]
translatorMsg lang = title : names
    where title = translatorMsgTitle lang
          names = zipWith appendLang translators $ languageNames lang
          appendLang n l = n ++ " (" ++ l ++ ")"

allLanguages :: [Language]
allLanguages = [English ..]

-- Wrap a String in backticks
bt :: String -> String
bt cs = "`" ++ cyan cs ++ "`"

whitespace :: Language -> Char
whitespace Japanese = '　'  -- \12288
whitespace _ = ' '          -- \32

langFromEnv :: String -> Language
langFromEnv ('j':'a':_) = Japanese
langFromEnv ('p':'l':_) = Polish
langFromEnv ('h':'r':_) = Croatian
langFromEnv ('s':'v':_) = Swedish
langFromEnv ('d':'e':_) = German
langFromEnv ('e':'s':_) = Spanish
langFromEnv ('p':'t':_) = Portuguese
langFromEnv ('f':'r':_) = French
langFromEnv ('r':'u':_) = Russian
langFromEnv ('i':'t':_) = Italian
langFromEnv ('s':'r':_) = Serbian
langFromEnv _           = English

--------------------
-- Core functions
--------------------
-- NEEDS TRANSLATION
checkDBLock_1 :: Language -> String
checkDBLock_1 Japanese = "パッケージデータベースが今閉鎖状態。開放したらキーを押して続行をどうぞ。"
checkDBLock_1 _        = "The package database is locked. Press enter when it's unlocked to continue."

mustBeRootMsg1 :: Language -> String
mustBeRootMsg1 English    = "You have to use " ++ bt "sudo" ++ " for that."
mustBeRootMsg1 Japanese   = bt "sudo" ++ "を使わないとそれができない！"
mustBeRootMsg1 Polish     = "Musisz użyć " ++ bt "sudo" ++ ", żeby to zrobić."
mustBeRootMsg1 Croatian   = "Morate koristiti" ++ bt "sudo" ++ "za ovu radnju."
mustBeRootMsg1 Swedish    = "Du måste använda " ++ bt "sudo" ++ " för det."
mustBeRootMsg1 German     = "Sie müssen dafür " ++ bt "sudo" ++ " benutzen."
mustBeRootMsg1 Spanish    = "Tienes que utilizar " ++ bt "sudo" ++ " para eso."
mustBeRootMsg1 Portuguese = "Utilize " ++ bt "sudo" ++ "para isso."
mustBeRootMsg1 French     = "Vous devez utiliser " ++ bt "sudo" ++ " pour ça."
mustBeRootMsg1 Russian    = "Необходимо использовать " ++ bt "sudo" ++ " для этого."
mustBeRootMsg1 Italian    = "È necessario utilizzare " ++ bt "sudo" ++ " per questo."
mustBeRootMsg1 Serbian    = "Морате да користите " ++ bt "sudo" ++ " за ову радњу."

buildPackagesMsg1 :: Language -> String -> String
buildPackagesMsg1 English    p = "Building " ++ bt p ++ "..."
buildPackagesMsg1 Japanese   p = bt p ++ "を作成中・・・"
buildPackagesMsg1 Polish     p = "Budowanie " ++ bt p ++ "..."
buildPackagesMsg1 Croatian   p = "Gradim " ++ bt p ++ "..."
buildPackagesMsg1 Swedish    p = "Bygger paket " ++ bt p ++ "..."
buildPackagesMsg1 German     p = "Baue Paket " ++ bt p ++ "..."
buildPackagesMsg1 Spanish    p = "Construyendo " ++ bt p ++ "..."
buildPackagesMsg1 Portuguese p = "Compilando " ++ bt p ++ "..."
buildPackagesMsg1 French     p = "Construction de " ++ bt p ++ "…"
buildPackagesMsg1 Russian    p = "Сборка " ++ bt p ++ "..."
buildPackagesMsg1 Italian    p = "Compilazione di " ++ bt p ++ "..."
buildPackagesMsg1 Serbian    p = "Градим " ++ bt p ++ "..."

checkHotEditMsg1 :: Language -> String -> String
checkHotEditMsg1 English    p = "Would you like to edit the PKGBUILD of " ++ bt p ++ "?"
checkHotEditMsg1 Japanese   p = bt p ++ "のPKGBUILDを編成？"
checkHotEditMsg1 Polish     p = "Czy chcesz edytować PKGBUILD " ++ bt p ++ "?"
checkHotEditMsg1 Croatian   p = "Želite li izmjeniti PKGBUILD " ++ bt p ++ "?"
checkHotEditMsg1 Swedish    p = "Vill du ändra PKGBUILD-filen ifrån " ++ bt p ++ "?"
checkHotEditMsg1 German     p = "Möchten Sie die PKGBUILD-Datei für " ++ bt p ++ " bearbeiten?"
checkHotEditMsg1 Spanish    p = "¿Te gustaría editar el PKGBUILD de " ++ bt p ++ "?"
checkHotEditMsg1 Portuguese p = "Desejaria editar o PKGBUILD de " ++ bt p ++ "?"
checkHotEditMsg1 French     p = "Voulez-vous éditer le PKGBUILD de " ++ bt p ++ " ?"
checkHotEditMsg1 Russian    p = "Отредактировать PKGBUILD пакета " ++ bt p ++ "?"
checkHotEditMsg1 Italian    p = "Volete modificare il PKGBUILD di " ++ bt p ++ "?"
checkHotEditMsg1 Serbian    p = "Желите ли да измените PKGBUILD за " ++ bt p ++ "?"

buildFailMsg1 :: Language -> String -> String
buildFailMsg1 English    p = "Well, building " ++ bt p ++ " failed."
buildFailMsg1 Japanese   p = bt p ++ "の作成は失敗したようだ。"
buildFailMsg1 Polish     p = "Budowanie " ++ bt p ++ " zakończyło się niepowodzeniem."
buildFailMsg1 Croatian   p = "Izgradnja " ++ bt p ++ " nije uspjela."
buildFailMsg1 Swedish    p = "Det gick inte att bygga paketet " ++ bt p ++ "."
buildFailMsg1 German     p = "Bauen von " ++ bt p ++ " ist fehlgeschlagen."
buildFailMsg1 Spanish    p = "La construcción de " ++ bt p ++ " ha fallado."
buildFailMsg1 Portuguese p = "Falha na compilação do pacote " ++ bt p ++ "."
buildFailMsg1 French     p = "Bon, la construction de " ++ bt p ++ " a échouée."
buildFailMsg1 Russian    p = "Что ж, сборка " ++ bt p ++ " не удалась."
buildFailMsg1 Italian    p = "La compilazione di " ++ bt p ++ "è fallita."
buildFailMsg1 Serbian    p = "Изградња пакета " ++ bt p ++ " није успела."

buildFailMsg2 :: Language -> String
buildFailMsg2 English    = "Also, the following weren’t built:"
buildFailMsg2 Japanese   = "ちなみに下記のパッケージも作成されなかった："
buildFailMsg2 Polish     = "Dodatkowo, następujące pakiety nie zostały zbudowane:"
buildFailMsg2 Croatian   = "Dodatno, ni sljedeće nije izgrađeno:"
buildFailMsg2 Swedish    = "Det gick heller inte att bygga följande paket:"
buildFailMsg2 German     = "Die folgenden Pakete wurden zusätzlich nicht gebaut:"
buildFailMsg2 Spanish    = "Los siguientes paquetes no se han construido:"
buildFailMsg2 Portuguese = "Os pacotes a seguir não foram compilados:"
buildFailMsg2 French     = "En outre, les paquets suivants n'ont pu être construits :"
buildFailMsg2 Russian    = "К тому же следующие пакеты не были собраны:"
buildFailMsg2 Italian    = "Inoltre non è stato possibile cotruire i seguenti pacchetti:"
buildFailMsg2 Serbian    = "Такође, ни следећи пакети нису изграђени::"

buildFailMsg3 :: Language -> String
buildFailMsg3 English    = "However, these packages were successfully built:"
buildFailMsg3 Japanese   = "しかし、以下のパッケージファイルは無事作成された："
buildFailMsg3 Polish     = "Następujące pakiety zostały zbudowane pomyślnie:"
buildFailMsg3 Croatian   = "Neki paketi su možda izgrađeni uspješno."
buildFailMsg3 Swedish    = "Vissa paket kanske har byggts ordentligt (Osäker)."
buildFailMsg3 German     = "Diese Pakete wurden wiederrum erfolgreich gebaut:"
buildFailMsg3 Spanish    = "Sin embargo, los siguientes paquetes se han construido:"
buildFailMsg3 Portuguese = "Entretanto, os seguintes pacotes compilaram com sucesso:"
buildFailMsg3 French     = "Cependant, les paquets suivants ont été construits avec succès :"
buildFailMsg3 Russian    = "Однако эти пакеты были успешно собраны:"
buildFailMsg3 Italian    = "Comunque questi pacchetti sono stato compilati con successo:"
buildFailMsg3 Serbian    = "Међутим, ови пакети су успешно изграђени:"

buildFailMsg4 :: Language -> String
buildFailMsg4 English    = "Would you like to install them?"
buildFailMsg4 Japanese   = "できたやつのインストールを続行する？"
buildFailMsg4 Polish     = "Czy chcesz je zainstalować?"
buildFailMsg4 Croatian   = "Želite li ih instalirati?"
buildFailMsg4 Swedish    = "Vill du installera dem?"
buildFailMsg4 German     = "Möchten sie diese installieren?"
buildFailMsg4 Spanish    = "¿Te gustaría instalarlos?"
buildFailMsg4 Portuguese = "Gostaria de instalá-los?"
buildFailMsg4 French     = "Voulez-vous les installer ?"
buildFailMsg4 Russian    = "Желаете ли вы их установить?"
buildFailMsg4 Italian    = "Volete installarli?"
buildFailMsg4 Serbian    = "Желите ли их инсталирати?"

buildFailMsg5 :: Language -> String
buildFailMsg5 English    = "Building failed."
buildFailMsg5 Japanese   = "パッケージ作成は失敗した。"
buildFailMsg5 Polish     = "Budowanie nie powiodło się."
buildFailMsg5 Croatian   = "Izgradnja nije uspjela."
buildFailMsg5 Swedish    = "Gick inte att bygga paket."
buildFailMsg5 German     = "Bauen fehlgeschlagen."
buildFailMsg5 Spanish    = "La construcción falló."
buildFailMsg5 Portuguese = "Falha na compilação."
buildFailMsg5 French     = "Construction ratée."
buildFailMsg5 Russian    = "Сборка не удалась."
buildFailMsg5 Italian    = "Compilazione fallita."
buildFailMsg5 Serbian    = "Изградња пакета није успела."

displayBuildErrorsMsg1 :: Language -> String
displayBuildErrorsMsg1 English    = "Dumping makepkg output in "
displayBuildErrorsMsg1 Japanese   = "抑えていたmakepkgの出力を受け取る用意・・・"
displayBuildErrorsMsg1 Polish     = "Wyjście makepkg zostanie wypisane za "
displayBuildErrorsMsg1 Croatian   = "Zapisujem makepkg ispis u "
displayBuildErrorsMsg1 Swedish    = "Dumpar makepkgs utskrift i "
displayBuildErrorsMsg1 German     = "Schreibe makepkg Ausgabe in "
displayBuildErrorsMsg1 Spanish    = "Volcando la salida de makepkg en "
displayBuildErrorsMsg1 Portuguese = "Despejando a saída do makepkg em "
displayBuildErrorsMsg1 French     = "Redirection de la sortie de makepkg dans "
displayBuildErrorsMsg1 Russian    = "Вывод makepkg записывается в "
displayBuildErrorsMsg1 Italian    = "Salvataggio dell'output di makepkg in "
displayBuildErrorsMsg1 Serbian    = "Уписујем излаз makepkg-а у "

-- Is this still used?
getDepsToInstallMsg1 :: Language -> String
getDepsToInstallMsg1 English    = "No AUR packages specified for install."
getDepsToInstallMsg1 Japanese   = "パッケージは一つも指摘されていない。"
getDepsToInstallMsg1 Polish     = "Nie podano pakietów z AUR do zainstalowania."
getDepsToInstallMsg1 Croatian   = "Nijedan AUR paket nije specificiran za instalaciju."
getDepsToInstallMsg1 Swedish    = "Inga AUR-paket är valda för installation."
getDepsToInstallMsg1 German     = "Keine AUR Pakete zur Installation vermerkt."
getDepsToInstallMsg1 Spanish    = "No se han especificado paquetes de AUR para instalar."
getDepsToInstallMsg1 Portuguese = "Nenhum pacote AUR foi especificado para instalação."
getDepsToInstallMsg1 French     = "Aucun paquet AUR n'a été spécifié pour l'installation."
getDepsToInstallMsg1 Russian    = "Пакеты AUR для установки не указаны."
getDepsToInstallMsg1 Italian    = "Nessun pacchetto di AUR specificato per l'installazione."
getDepsToInstallMsg1 Serbian    = "Ниједан AUR пакет није специфициран за инсталацију."

getRealPkgConflictsMsg1 :: Language -> String -> String -> String -> String
getRealPkgConflictsMsg1 English    p r d = "The dependency " ++ bt p ++ " demands version " ++ bt d ++ "but the most recent version is " ++ bt r ++ "."
getRealPkgConflictsMsg1 Japanese   p r d = "パッケージ" ++ bt p ++ "はバージョン" ++ bt d ++ "を要するが" ++ "一番最新のバージョンは" ++ bt r ++ "。"
getRealPkgConflictsMsg1 Polish     p r d = "Zależność " ++ bt p ++ " powinna być w wersji " ++ bt d ++ ", ale najnowsza wersja to " ++ bt r ++ "."
getRealPkgConflictsMsg1 Croatian   p r d = "Zavisnost " ++ bt p ++ " zahtjeva verziju " ++ bt d ++ ", a najnovija dostupna verzija je " ++ bt r ++ "."
getRealPkgConflictsMsg1 Swedish    p r d = "Beroendepaketet " ++ bt p ++ " kräver version " ++ bt d ++ "men den senaste versionen är " ++ bt r ++ "."
getRealPkgConflictsMsg1 German     p r d = "Die Abhängigkeit " ++ bt p ++ " verlangt Version " ++ bt d ++ "aber die neuste Version ist " ++ bt r ++ "."
getRealPkgConflictsMsg1 Spanish    p r d = "La dependencia " ++ bt p ++ " duiere la versión " ++ bt d ++ "pero la versión más reciente es " ++ bt r ++ "."
getRealPkgConflictsMsg1 Portuguese p r d = "A dependência " ++ bt p ++ " exige a versão " ++ bt d ++ "mas a versão mais recente é " ++ bt r ++ "."
getRealPkgConflictsMsg1 French     p r d = bt p ++ " est une dépendance nécessitant une version " ++ bt d ++ ", mais la plus récente est la " ++ bt r ++ "."
getRealPkgConflictsMsg1 Russian    p r d = "Зависимость " ++ bt p ++ " требует версию " ++ bt d ++ ", однако самой последней версией является " ++ bt r ++ "."
getRealPkgConflictsMsg1 Italian    p r d = "La dipendenza " ++ bt p ++ " richiede la versione " ++ bt d ++ "ma la versione disponibile è " ++ bt r ++ "."
getRealPkgConflictsMsg1 Serbian    p r d = "Зависност " ++ bt p ++ " захтева верзију " ++ bt d ++ ", али најновија верзија је " ++ bt r ++ "."

getRealPkgConflictsMsg2 :: Language -> String -> String
getRealPkgConflictsMsg2 English    p = bt p ++ " is an ignored package! See your `pacman.conf` file."    
getRealPkgConflictsMsg2 Japanese   p = bt p ++ "は無視されるパッケージ！`pacman.conf`を参考に。"
getRealPkgConflictsMsg2 Polish     p = bt p ++ " jest ignorowany! Sprawdź plik `pacman.conf`."
getRealPkgConflictsMsg2 Croatian   p = bt p ++ " je ignoriran paket! Pogledajte svoj `pacman.conf`."
getRealPkgConflictsMsg2 Swedish    p = bt p ++ " är ett ignorerat paket! Kolla din `pacman.conf`-fil."
getRealPkgConflictsMsg2 German     p = bt p ++ " ist ein ignoriertes Paket! Siehe /etc/pacman.conf."
getRealPkgConflictsMsg2 Spanish    p = "¡" ++ bt p ++ " es un paquete ignorado! Revisa tu fichero `pacman.conf`."
getRealPkgConflictsMsg2 Portuguese p = bt p ++ " é um pacote ignorado conforme configuração em `pacman.conf`!"
getRealPkgConflictsMsg2 French     p = "Le paquet " ++ bt p ++ " est ignoré. Vous devriez jeter un œil à votre `pacman.conf`."
getRealPkgConflictsMsg2 Russian    p = "Пакет " ++ bt p ++ " игнорируется! Проверьте ваш файл `pacman.conf`."
getRealPkgConflictsMsg2 Italian    p = bt p ++ " è un pacchetto ignorato, controllare `pacman.conf`."
getRealPkgConflictsMsg2 Serbian    p = "Пакет " ++ bt p ++ " је игнорисан! Видите ваш фајл „pacman.conf“."

getVirtualConflictsMsg1 :: Language -> String -> String
getVirtualConflictsMsg1 English    p = bt p ++ " exists in NO WAY as a package or as one provided by another!"
getVirtualConflictsMsg1 Japanese   p = bt p ++ "はパッケージでもないし、他のパッケージにも提供されていない！"
getVirtualConflictsMsg1 Polish     p = bt p ++ " nie istnieje jako pakiet lub jako pakiet dostarczany przez inny!"
getVirtualConflictsMsg1 Croatian   p = bt p ++ " ne postoji kao paket niti ga bilo koji paket pruža!"
getVirtualConflictsMsg1 Swedish    p = bt p ++ " existerar varken som ett paket eller som ett tillhandahållet av ett annat!"
getVirtualConflictsMsg1 German     p = bt p ++ " existiert nicht als Paket oder als Bereitstellung eines anderen!"
getVirtualConflictsMsg1 Spanish    p = "¡" ++ bt p ++ " no existe como paquete ni es provisto por ninguno!"
getVirtualConflictsMsg1 Portuguese p = bt p ++ " não existe como um pacote e não é provido por nenhum!"
getVirtualConflictsMsg1 French     p = bt p ++ " n'est ni un paquet existant, ni un paquet fourni par un autre !"
getVirtualConflictsMsg1 Russian    p = bt p ++ " никоим образом не существует в виде пакета или пакета, " ++ " предоставленного другим пакетом!"
getVirtualConflictsMsg1 Italian    p = bt p ++ " non esiste e non è distribuito da nessun'altro."
getVirtualConflictsMsg1 Serbian    p = bt p ++ " не постоји као пакет нити га други пакет пружа!"

getVirtualConflictsMsg2 :: Language -> String -> String -> String
getVirtualConflictsMsg2 English    p pro = bt pro ++ " provides " ++ bt p ++ ", but " ++ bt pro ++ " is an ignored package."
getVirtualConflictsMsg2 Japanese   p pro = bt p ++ "は" ++ bt pro ++ "に提供されているが、" ++ bt pro ++ "は無視されるパッケージ。"
getVirtualConflictsMsg2 Polish     p pro = bt pro ++ " dostarcza " ++ bt p ++ ", ale " ++ bt pro ++ " jest ignorowany."
getVirtualConflictsMsg2 Croatian   p pro = bt pro ++ " pruža  " ++ bt p ++ ", ali " ++ bt pro ++ " je ignoriran paket."
getVirtualConflictsMsg2 Swedish    p pro = bt pro ++ " tillhandahåller " ++ bt p ++ ", men " ++ bt pro ++ " är ett ignorerat paket."
getVirtualConflictsMsg2 German     p pro = bt pro ++ " stellt " ++ bt p ++ " bereit, aber " ++ bt pro ++ " ist ein ignoriertes Paket."
getVirtualConflictsMsg2 Spanish    p pro = bt pro ++ " provee " ++ bt p ++ ", pero " ++ bt pro ++ " es un paquete ignorado."
getVirtualConflictsMsg2 Portuguese p pro = bt pro ++ " provê " ++ bt p ++ ", mas " ++ bt pro ++ "é um pacote ignorado."
getVirtualConflictsMsg2 French     p pro = bt pro ++ " fourni " ++ bt p ++ ", mais " ++ bt pro ++ " est un paquet ignoré."
getVirtualConflictsMsg2 Russian    p pro = bt pro ++ " предоставляет " ++ bt p ++ ", но " ++ bt pro ++ " является игнорируемым пакетом."
getVirtualConflictsMsg2 Italian    p pro = bt pro ++ " distribuisce " ++ bt p ++ " ma " ++ bt pro ++ " è un pacchetto ignorato."
getVirtualConflictsMsg2 Serbian    p pro = bt pro ++ " пружа " ++ bt p ++ ", али је " ++ bt pro ++ " игнорисан пакет."

getVirtualConflictsMsg3 :: Language -> String -> String -> String -> String -> String
getVirtualConflictsMsg3 English    d dv p pv = "The dependency " ++ bt d ++ " demands version " ++ bt dv ++ " but its providing package " ++
                                               bt p ++ " gives version " ++ bt pv
getVirtualConflictsMsg3 Japanese   d dv p pv = "仮のパッケージ" ++ bt d ++ "はバージョン" ++ bt dv ++ "を要するが、" ++ "それを提供する" ++
                                               bt p ++ "はバージョン" ++ bt pv ++ "しか提供しない"
getVirtualConflictsMsg3 Polish     d dv p pv = "Zależność " ++ bt d ++ " powinna być w wersji " ++ bt dv ++ ", ale pakiet dostarczający (" ++
                                               bt p ++ ") jest w wersji " ++ bt pv
getVirtualConflictsMsg3 Croatian   d dv p pv = "Zavisnost " ++ bt d ++ " zahtjeva verziju " ++ bt dv ++ ", ali paket " ++ bt p ++ " pruža verziju " ++ bt pv
getVirtualConflictsMsg3 Swedish    d dv p pv = "Beroendepaket " ++ bt d ++ " kräver version " ++ bt dv ++ " men dens tillhandahållande paket " ++
                                               bt p ++ " ger version " ++ bt pv
getVirtualConflictsMsg3 German     d dv p pv = "Die Abhängigkeit " ++ bt d ++ " verlangt Version " ++ bt dv ++ " aber dessen bereitstellendes Paket " ++
                                               bt p ++ " gibt Version " ++ bt pv
getVirtualConflictsMsg3 Spanish    d dv p pv = "La dependencia " ++ bt d ++ " requiere la versión " ++ bt dv ++ " pero el paquete " ++
                                               bt p ++ ", que la provee, da la versión " ++ bt pv
getVirtualConflictsMsg3 Portuguese d dv p pv = "A dependência " ++ bt d ++ " requer a versão " ++ bt dv ++ " entretanto, o pacote " ++
                                               bt p ++ ", que o provê, possui a versão " ++ bt pv
getVirtualConflictsMsg3 French     d dv p pv = "La dépendance " ++ bt d ++ " nécessite la version " ++ bt dv ++ ", mais le paquet qui la fournie (" ++
                                               bt p ++ ") ne le fait qu'en version " ++ bt pv ++ "."
getVirtualConflictsMsg3 Russian    d dv p pv = "Зависимость " ++ bt d ++ " должна быть версии " ++ bt dv ++ ", но предоставленный для неё пакет " ++
                                               bt p ++ " имеет версию " ++ bt pv
getVirtualConflictsMsg3 Italian    d dv p pv = "La dipendenza " ++ bt d ++ " richiede la versione " ++ bt dv ++ " ma il pacchetto " ++
                                               bt p ++ " distribuisce la versione " ++ bt pv
getVirtualConflictsMsg3 Serbian    d dv p pv = "Зависност " ++ bt d ++ " захтева верзију " ++ bt dv ++ ", али пакет " ++ bt p ++
                                               " пружа верзију " ++ bt pv

-----------------
-- aura functions
-----------------
executeOptsMsg1 :: Language -> String
executeOptsMsg1 English    = "Conflicting flags given!"
executeOptsMsg1 Japanese   = "矛盾しているオプションあり。"
executeOptsMsg1 Polish     = "Niektóre flagi są w konflikcie ze sobą!"
executeOptsMsg1 Croatian   = "Predane zastavice su konfliktne!"
executeOptsMsg1 Swedish    = "Givna flaggor är i konflikt!"
executeOptsMsg1 German     = "Gegebene Kommandozeilenflags sind widersprüchlich!"
executeOptsMsg1 Spanish    = "¡Flags contradictorios!"
executeOptsMsg1 Portuguese = "Flags conflitantes!"
executeOptsMsg1 French     = "Arguments contradictoires !"
executeOptsMsg1 Russian    = "Даны конфликтующие флаги!"
executeOptsMsg1 Italian    = "Argomenti in conflitto!"
executeOptsMsg1 Serbian    = "Захтеване опције су контрадикторне!"

-- Packages should not be built if the user is logged in as root!
trueRootMsg1 :: Language -> String
trueRootMsg1 English    = "You should never build packages as the true root. Are you okay with this?"
trueRootMsg1 Japanese   = "本当のrootユーザーとしてパッケージを作成するのが危険。続行？"
trueRootMsg1 Polish     = "Nigdy nie powinieneś budować pakietów jako root. Na pewno kontynuować?"
trueRootMsg1 Croatian   = "Pakete ne bi trebalo graditi s pravim root ovlastima. Nastavi?"
trueRootMsg1 Swedish    = "Det är starkt rekommenderat att INTE vara inloggad som root när man bygger paket. Vill du fortsätta ändå?"
trueRootMsg1 German     = "Sie sollten niemals Pakete als der echte root Nutzer bauen. Sind sie sicher, dass Sie dies tun wollen?"
trueRootMsg1 Spanish    = "Nunca deberías construir paquetes como root real. ¿Estás de acuerdo con esto?"
trueRootMsg1 Portuguese = "Não deveria compilar pacotes como o root de fato. Ainda assim, deseja prosseguir?"
trueRootMsg1 French     = "Il n'est pas sage de construire des paquets avec le compte root. Voulez-vous continuer ?"
trueRootMsg1 Russian    = "Вам никогда не следует собирать пакеты под настоящим рутом. Договорились?"
trueRootMsg1 Italian    = "Non si dovrebbero compilare pacchetti come root. Volete Continuare?"
trueRootMsg1 Serbian    = "Не би требало градити пакете са правим root овлашћењима. Желите ли наставити?"

-- This is for when the user decides to refrain from building afterall.
trueRootMsg2 :: Language -> String
trueRootMsg2 English    = "You’ve done the right thing."
trueRootMsg2 Japanese   = "よしよし。"
trueRootMsg2 Polish     = "Postąpiłeś słusznie."
trueRootMsg2 Croatian   = "Učinili ste Ispravnu Stvar."
trueRootMsg2 Swedish    = "Phew."
trueRootMsg2 German     = "Eine weise Entscheidung."
trueRootMsg2 Spanish    = "Has tomado la decision correcta."
trueRootMsg2 Portuguese = "Ainda bem que tem juízo!"
trueRootMsg2 French     = "C'est la bonne décision."
trueRootMsg2 Russian    = "Вы выбрали православный путь."
trueRootMsg2 Italian    = "Hai fatto la cosa giusta."
trueRootMsg2 Serbian    = "Исправно сте поступили."

installPackagesMsg1 :: Language -> String
installPackagesMsg1 English    = "Dependency checking failed for these reasons:"
installPackagesMsg1 Japanese   = "従属パッケージの確認は以下の理由で失敗した："
installPackagesMsg1 Polish     = "Sprawdzanie zależności nie powiodło się z następujących powodów:"
installPackagesMsg1 Croatian   = "Provjera zavisnosti nije uspjela iz sljedećih razloga:"
installPackagesMsg1 Swedish    = "Beroende-kollen misslyckades pga följande skäl:"
installPackagesMsg1 German     = "Abhängigkeitsüberprüfung schlug Fehl aus folgenden Gründen:"
installPackagesMsg1 Spanish    = "La comprobación de dependencias falló por los siguientes motivos:"
installPackagesMsg1 Portuguese = "Não foi possível checar as dependências pelas seguintes razões:"
installPackagesMsg1 French     = "La vérification des dépendances a faillie pour les raisons suivantes :"
installPackagesMsg1 Russian    = "Проверка зависимостей не удалась из-за:"
installPackagesMsg1 Italian    = "Il controllo delle dipendenze è fallito per i seguenti motivi:"
installPackagesMsg1 Serbian    = "Провера зависности није успела из следећих разлога:"

installPackagesMsg2 :: Language -> String
installPackagesMsg2 English    = "No valid packages specified."
installPackagesMsg2 Japanese   = "適当なパッケージを入力してください。"
installPackagesMsg2 Polish     = "Nie podano prawidłowych pakietów."
installPackagesMsg2 Croatian   = "Nije specificiran nijedan ispravan paket."
installPackagesMsg2 Swedish    = "Inga giltiga paket valda."
installPackagesMsg2 German     = "Keine gültigen Pakete angegeben."
installPackagesMsg2 Spanish    = "No se ha especificado ningún paquete válido."
installPackagesMsg2 Portuguese = "Nenhum pacote válido foi especificado."
installPackagesMsg2 French     = "Aucun paquet valide spécifié."
installPackagesMsg2 Russian    = "Валидные пакеты не указаны."
installPackagesMsg2 Italian    = "Nessun pacchetto valido specificato."
installPackagesMsg2 Serbian    = "Ниједан исправан пакет није специфициран."

installPackagesMsg3 :: Language -> String
installPackagesMsg3 English    = "Continue?"
installPackagesMsg3 Japanese   = "続行？"
installPackagesMsg3 Polish     = "Kontynuować?"
installPackagesMsg3 Croatian   = "Nastavi?"
installPackagesMsg3 Swedish    = "Fortsätta?"
installPackagesMsg3 German     = "Fortsetzen?"
installPackagesMsg3 Spanish    = "¿Continuar?"
installPackagesMsg3 Portuguese = "Continuar?"
installPackagesMsg3 French     = "Continuer ?"
installPackagesMsg3 Russian    = "Продолжить?"
installPackagesMsg3 Italian    = "Continuare?"
installPackagesMsg3 Serbian    = "Наставити?"

installPackagesMsg4 :: Language -> String
installPackagesMsg4 English    = "Installation manually aborted."
installPackagesMsg4 Japanese   = "続行は意図的に阻止された。"
installPackagesMsg4 Polish     = "Instalacja została przerwana przez użytkownika."
installPackagesMsg4 Croatian   = "Instalacija prekinuta od strane korisnika."
installPackagesMsg4 Swedish    = "Installationen avbröts manuellt."
installPackagesMsg4 German     = "Installation durch Benutzer abgebrochen."
installPackagesMsg4 Spanish    = "Instalación abortada manualmente."
installPackagesMsg4 Portuguese = "Instalação manual abortada."
installPackagesMsg4 French     = "Installation manuelle annulée."
installPackagesMsg4 Russian    = "Пользователь прервал установку."
installPackagesMsg4 Italian    = "Installazione manuale interrotta."
installPackagesMsg4 Serbian    = "Инсталација је ручно прекинута."

installPackagesMsg5 :: Language -> String
installPackagesMsg5 English    = "Determining dependencies..."
installPackagesMsg5 Japanese   = "従属パッケージを確認中・・・"
installPackagesMsg5 Polish     = "Ustalanie zależności..."
installPackagesMsg5 Croatian   = "Određivanje zavisnosti..."
installPackagesMsg5 Swedish    = "Avgör beroenden..."
installPackagesMsg5 German     = "Bestimme Abhängigkeiten..."
installPackagesMsg5 Spanish    = "Determinando dependencias..."
installPackagesMsg5 Portuguese = "Determinando as dependências..."
installPackagesMsg5 French     = "Détermination des dépendances en cours…"
installPackagesMsg5 Russian    = "Определение зависимостей..."
installPackagesMsg5 Italian    = "Determinazione dipendenze..."
installPackagesMsg5 Serbian    = "Утврђивање зависности..."

knownBadPkgCheckMsg1 :: Language -> String -> String
knownBadPkgCheckMsg1 Japanese p = bt p ++ "の作成は失敗すると知られている。理由："
knownBadPkgCheckMsg1 Serbian  p = "Познато је да се " ++ bt p ++ " неуспешно гради. Разлог:"
knownBadPkgCheckMsg1 _        p = bt p ++ " is known to fail at building. Reason:"

knownBadPkgCheckMsg2 :: Language -> String
knownBadPkgCheckMsg2 Japanese = "それでもやってみる？"
knownBadPkgCheckMsg2 Serbian  = "Желите ли ипак да пробате?"
knownBadPkgCheckMsg2 _        = "Will you try anyway?"

reportNonPackagesMsg1 :: Language -> String
reportNonPackagesMsg1 English    = "The following are not packages:"
reportNonPackagesMsg1 Japanese   = "下記はパッケージではない："
reportNonPackagesMsg1 Polish     = "To nie są pakiety:"
reportNonPackagesMsg1 Croatian   = "Ovo nisu paketi:"
reportNonPackagesMsg1 Swedish    = "Följande är inte paket:"
reportNonPackagesMsg1 German     = "Folgende sind keine Pakete:"
reportNonPackagesMsg1 Spanish    = "Los siguientes no son paquetes:"
reportNonPackagesMsg1 Portuguese = "Os seguintes não são pacotes:"
reportNonPackagesMsg1 French     = "Les éléments suivants ne sont pas des paquets:"
reportNonPackagesMsg1 Russian    = "Ниже указано то, что не является пакетами:"
reportNonPackagesMsg1 Italian    = "I seguenti non sono pacchetti:"
reportNonPackagesMsg1 Serbian    = "Ово нису пакети:"

reportIgnoredPackagesMsg1 :: Language -> String
reportIgnoredPackagesMsg1 English    = "The following packages will be ignored:"
reportIgnoredPackagesMsg1 Japanese   = "下記のパッケージは無視される："
reportIgnoredPackagesMsg1 Polish     = "Poniższe pakiety zostaną zignorowane:"
reportIgnoredPackagesMsg1 Croatian   = "Sljedeći paketi će biti ignorirani:"
reportIgnoredPackagesMsg1 Swedish    = "Följande paket kommer att ignoreras: "
reportIgnoredPackagesMsg1 German     = "Die folgenden Pakete werden ignoriert:"
reportIgnoredPackagesMsg1 Spanish    = "Los siguientes paquetes serán ignorados:"
reportIgnoredPackagesMsg1 Portuguese = "Os seguintes pacotes serão ignorados:"
reportIgnoredPackagesMsg1 French     = "Les paquets suivants seront ignorés :"
reportIgnoredPackagesMsg1 Russian    = "Следующие пакеты будут проигнорированы:"
reportIgnoredPackagesMsg1 Italian    = "I seguenti pacchetti verranno ignorati:"
reportIgnoredPackagesMsg1 Serbian    = "Следећи пакети ће бити игнорисани:"

reportPkgsToInstallMsg1 :: Language -> String
reportPkgsToInstallMsg1 English    = "Repository dependencies:"
reportPkgsToInstallMsg1 Japanese   = "Pacmanの従属パッケージ："
reportPkgsToInstallMsg1 Polish     = "Zależności z repozytoriów:"
reportPkgsToInstallMsg1 Croatian   = "Zavisnosti iz repozitorija:"
reportPkgsToInstallMsg1 Swedish    = "Beroenden ifrån lager:"
reportPkgsToInstallMsg1 German     = "Abhängigkeiten in den Paketquellen:"
reportPkgsToInstallMsg1 Spanish    = "Dependencias en el repositorio:"
reportPkgsToInstallMsg1 Portuguese = "Dependências no repositório:"
reportPkgsToInstallMsg1 French     = "Dépendances du dépôt:"
reportPkgsToInstallMsg1 Russian    = "Зависимости из репозитория:"
reportPkgsToInstallMsg1 Italian    = "Dipendenze nei repository:"
reportPkgsToInstallMsg1 Serbian    = "Зависности из ризница:"

reportPkgsToInstallMsg2 :: Language -> String
reportPkgsToInstallMsg2 English    = "AUR dependencies:"
reportPkgsToInstallMsg2 Japanese   = "AURの従属パッケージ："
reportPkgsToInstallMsg2 Polish     = "Zależności z AUR:"
reportPkgsToInstallMsg2 Croatian   = "Zavisnosti iz AUR-a:"
reportPkgsToInstallMsg2 Swedish    = "Beroenden ifrån AUR:"
reportPkgsToInstallMsg2 German     = "Abhängigkeiten im AUR:"
reportPkgsToInstallMsg2 Spanish    = "Dependencias en AUR:"
reportPkgsToInstallMsg2 Portuguese = "Dependências no AUR:"
reportPkgsToInstallMsg2 French     = "Dépendances AUR :"
reportPkgsToInstallMsg2 Russian    = "Зависимости из AUR:"
reportPkgsToInstallMsg2 Italian    = "Dipendenze in AUR:"
reportPkgsToInstallMsg2 Serbian    = "Зависности из AUR-а:"

reportPkgsToInstallMsg3 :: Language -> String
reportPkgsToInstallMsg3 English    = "Main AUR packages:"
reportPkgsToInstallMsg3 Japanese   = "主なAURパッケージ："
reportPkgsToInstallMsg3 Polish     = "Główne pakiety z AUR:"
reportPkgsToInstallMsg3 Croatian   = "Glavni AUR paketi:"
reportPkgsToInstallMsg3 Swedish    = "Huvudpaket ifrån AUR:"
reportPkgsToInstallMsg3 German     = "Hauptpaket aus dem AUR:"
reportPkgsToInstallMsg3 Spanish    = "Paquetes principales de AUR:"
reportPkgsToInstallMsg3 Portuguese = "Pacotes principais do AUR:"
reportPkgsToInstallMsg3 French     = "Principaux paquets AUR :"
reportPkgsToInstallMsg3 Russian    = "Главные пакеты из AUR:"
reportPkgsToInstallMsg3 Italian    = "Pacchetto principale di AUR"
reportPkgsToInstallMsg3 Serbian    = "Главни пакети из AUR-а:"

-- Needs translations.
reportPkgbuildDiffsMsg1 :: Language -> String -> String
reportPkgbuildDiffsMsg1 English  p = bt p ++ " has no stored PKGBUILD yet."
reportPkgbuildDiffsMsg1 Japanese p = bt p ++ "のPKGBUILDはまだ保存されていない。"
reportPkgbuildDiffsMsg1 Polish   p = bt p ++ " nie ma jeszcze przechowywanego pliku PKGBUILD."
reportPkgbuildDiffsMsg1 Croatian p = bt p ++ " još nema pohranjen PKGBUILD."
reportPkgbuildDiffsMsg1 Spanish  p = bt p ++ " no tiene PKGBUILD todavía."
reportPkgbuildDiffsMsg1 French   p = bt p ++ " n'a pas encore de PKGBUILD enrigistré."
reportPkgbuildDiffsMsg1 Russian  p = "У " ++ bt p ++ " ещё нет сохраненного PKGBUILD."
reportPkgbuildDiffsMsg1 Italian  p = bt p ++ " non ci sono PKGBUILD salvati"
reportPkgbuildDiffsMsg1 Serbian  p = bt p ++ " још нема похрањен PKGBUILD."
reportPkgbuildDiffsMsg1 _        p = bt p ++ " has no stored PKGBUILD yet."

reportPkgbuildDiffsMsg2 :: Language -> String -> String
reportPkgbuildDiffsMsg2 English  p = bt p ++ "'s PKGBUILD is up to date."
reportPkgbuildDiffsMsg2 Japanese p = bt p ++ "のPKGBUILDは最新。"
reportPkgbuildDiffsMsg2 Polish   p = "PKGBUILD pakietu " ++ bt p ++ " jest aktualny."
reportPkgbuildDiffsMsg2 Croatian p = "PKGBUILD paketa " ++ bt p ++ " je na najnovijoj verziji."
reportPkgbuildDiffsMsg2 Spanish  p = "El PKGBUILD de " ++ bt p ++ " está actualizado."
reportPkgbuildDiffsMsg2 Russian  p = "PKGBUILD " ++ bt p ++ " является новейшим."
reportPkgbuildDiffsMsg2 French   p = "Le PKGBUILD de " ++ bt p ++ " est à jour."
reportPkgbuildDiffsMsg2 Italian  p = "Il PKGBUILD di " ++ bt p ++ " è aggiornato."
reportPkgbuildDiffsMsg2 Serbian  p = "PKGBUILD пакета " ++ bt p ++ " је ажуран."
reportPkgbuildDiffsMsg2 _        p = bt p ++ " PKGBUILD is up to date."

reportPkgbuildDiffsMsg3 :: Language -> String -> String
reportPkgbuildDiffsMsg3 English  p = bt p ++ " PKGBUILD changes:"
reportPkgbuildDiffsMsg3 Japanese p = bt p ++ "のPKGBUILD変更報告："
reportPkgbuildDiffsMsg3 Polish   p = "Zmiany w PKGBUILD dla " ++ bt p ++ ":"
reportPkgbuildDiffsMsg3 Croatian p = "Promjene u PKGBUILD-u za " ++ bt p ++ ":"
reportPkgbuildDiffsMsg3 Spanish  p = "Cambios en el PKGBUILD de " ++ bt p ++ ":"
reportPkgbuildDiffsMsg3 Russian  p = "Изменения, вносимые " ++ bt p ++ " PKGBUILD:"
reportPkgbuildDiffsMsg3 French   p = "Changements du PKGBUILD de " ++ bt p
reportPkgbuildDiffsMsg3 Italian  p = "Cambiamenti nel PKGBUILD di " ++ bt p ++":"
reportPkgbuildDiffsMsg3 Serbian  p = "Промене PKGBUILD-a за " ++ bt p ++ ":"
reportPkgbuildDiffsMsg3 _        p = bt p ++ " PKGBUILD changes:"

reportPkgsToUpgradeMsg1 :: Language -> String
reportPkgsToUpgradeMsg1 English    = "AUR Packages to upgrade:"
reportPkgsToUpgradeMsg1 Japanese   = "アップグレードするAURパッケージ："
reportPkgsToUpgradeMsg1 Polish     = "Pakiety z AUR do zaktualizowania:"
reportPkgsToUpgradeMsg1 Croatian   = "AUR paketi za nadograditi:"
reportPkgsToUpgradeMsg1 Swedish    = "AUR-paket att uppgradera:"
reportPkgsToUpgradeMsg1 German     = "Zu aktualisierendes AUR Paket:"
reportPkgsToUpgradeMsg1 Spanish    = "Paquetes de AUR a actualizar:"
reportPkgsToUpgradeMsg1 Portuguese = "Pacotes do AUR para atualizar:"
reportPkgsToUpgradeMsg1 French     = "Paquets AUR à mettre à jour :"
reportPkgsToUpgradeMsg1 Russian    = "Пакеты AUR, готовые для обновления:"
reportPkgsToUpgradeMsg1 Italian    = "Pacchetti in AUR da aggiornare:"
reportPkgsToUpgradeMsg1 Serbian    = "Пакети из AUR-а за надоградњу:"

reportBadDowngradePkgsMsg1 :: Language -> String
reportBadDowngradePkgsMsg1 English    = "The following aren’t installed, and thus can’t be downgraded:"
reportBadDowngradePkgsMsg1 Japanese   = "このパッケージは最初からインストールしていないので、格下げはできない。"
reportBadDowngradePkgsMsg1 Polish     = "Poniższe pakeity nie są zainstalowane, i nie mogą być zainstalowane w starszej wersji:"
reportBadDowngradePkgsMsg1 Croatian   = "Sljedeći paketi nisu instalirani te se stoga ne mogu vratiti na stare verzije:"
reportBadDowngradePkgsMsg1 Swedish    = "Följande paket är inte installerade, och kan därför inte bli nergraderade:"
reportBadDowngradePkgsMsg1 German     = "Folgende Pakete sind nicht installiert und können daher nicht downgraded werden:"
reportBadDowngradePkgsMsg1 Spanish    = "Los siguientes paquetes no están instalados, por lo que no se pueden retornar a versiones antiguas:"
reportBadDowngradePkgsMsg1 Portuguese = "Os seguintes pacotes não estão instalados, logo não podem retornar a uma versão anterior:"
reportBadDowngradePkgsMsg1 French     = "Les paquets suivants ne sont pas installés ; ils ne peuvent être rétrogradés :"
reportBadDowngradePkgsMsg1 Russian    = "Следующие пакеты не установлены, а следовательно, не могут быть откачены к старой версии:"
reportBadDowngradePkgsMsg1 Italian    = "I seguenti pacchetti non sono stati installati e non posso essere retrocessi:"
reportBadDowngradePkgsMsg1 Serbian    = "Следећи пакети нису ни инсталирани, те се не могу вратити на старију верзију:"

upgradeAURPkgsMsg1 :: Language -> String
upgradeAURPkgsMsg1 English    = "Fetching package information..."
upgradeAURPkgsMsg1 Japanese   = "パッケージ情報をダウンロード中・・・"
upgradeAURPkgsMsg1 Polish     = "Pobieranie informacji o pakietach..."
upgradeAURPkgsMsg1 Croatian   = "Preuzimanje podataka o paketima..."
upgradeAURPkgsMsg1 Swedish    = "Hämtar paketinformation..."
upgradeAURPkgsMsg1 German     = "Rufe Paketinformationen ab..."
upgradeAURPkgsMsg1 Spanish    = "Obteniendo información de paquetes..."
upgradeAURPkgsMsg1 Portuguese = "Obtendo informação dos pacotes..."
upgradeAURPkgsMsg1 French     = "Obtention des informations des paquets en cours…"
upgradeAURPkgsMsg1 Russian    = "Сборка информации о пакетах..."
upgradeAURPkgsMsg1 Italian    = "Ottengo le informazioni del pacchetto..."
upgradeAURPkgsMsg1 Serbian    = "Преузимање информација о пакетима..."

upgradeAURPkgsMsg2 :: Language -> String
upgradeAURPkgsMsg2 English    = "Comparing package versions..."
upgradeAURPkgsMsg2 Japanese   = "バージョンを比較中・・・"
upgradeAURPkgsMsg2 Polish     = "Porównywanie wersji pakietów..."
upgradeAURPkgsMsg2 Croatian   = "Uspoređivanje verzija paketa..."
upgradeAURPkgsMsg2 Swedish    = "Jämför paket-versioner..."
upgradeAURPkgsMsg2 German     = "Vergleiche Paketversionen..."
upgradeAURPkgsMsg2 Spanish    = "Comparando versiones de paquetes..."
upgradeAURPkgsMsg2 Portuguese = "Comparando versões dos pacotes..."
upgradeAURPkgsMsg2 French     = "Comparaison des versions des paquets en cours…"
upgradeAURPkgsMsg2 Russian    = "Сравнение версий пакетов..."
upgradeAURPkgsMsg2 Italian    = "Confronto le ersioni del pacchetto..."
upgradeAURPkgsMsg2 Serbian    = "Упоређивање верзија пакета..."

upgradeAURPkgsMsg3 :: Language -> String
upgradeAURPkgsMsg3 English    = "No AUR package upgrades necessary."
upgradeAURPkgsMsg3 Japanese   = "アップグレードは必要ない。"
upgradeAURPkgsMsg3 Polish     = "Nie jest wymagana aktualizacja pakietów z AUR."
upgradeAURPkgsMsg3 Croatian   = "Svi AUR paketi su ažurirani."
upgradeAURPkgsMsg3 Swedish    = "Inga AUR-paketsuppgraderingar behövs."
upgradeAURPkgsMsg3 German     = "Keine AUR Paketaktualisierungen notwendig."
upgradeAURPkgsMsg3 Spanish    = "No ha sido necesario actualizar paquetes de AUR."
upgradeAURPkgsMsg3 Portuguese = "Nenhum pacote do AUR precisa de atualização."
upgradeAURPkgsMsg3 French     = "Aucune mise à jour de paquets AUR n'est nécessaire."
upgradeAURPkgsMsg3 Russian    = "Обновление пакетов из AUR не требуется."
upgradeAURPkgsMsg3 Italian    = "Non è necessario aggiornare pacchetti di AUR."
upgradeAURPkgsMsg3 Serbian    = "Ажурирање пакета из AUR-а није потребно."

downloadTarballsMsg1 :: Language -> String -> String
downloadTarballsMsg1 English    p = "Downloading " ++ bt p ++ " source tarball..."
downloadTarballsMsg1 Japanese   p = bt p ++ "のソースコードのターボールをダウンロード中・・・"
downloadTarballsMsg1 Polish     p = "Pobieranie paczki źródłowej " ++ bt p ++ "..."
downloadTarballsMsg1 Croatian   p = "Preuzimanje izvornog paketa (tarball) " ++ bt p ++ "..."
downloadTarballsMsg1 Swedish    p = "Laddar ner " ++ bt p ++ " källkodspaket (tarball)..."
downloadTarballsMsg1 German     p = "Lade Quelltext von " ++ bt p ++ " (tarball)..."
downloadTarballsMsg1 Spanish    p = "Descargando los fuentes comprimidos (tarball) de " ++ bt p ++ " ..."
downloadTarballsMsg1 Portuguese p = "Baixando os fontes (tarball) de " ++ bt p ++ " ..."
downloadTarballsMsg1 French     p = "Téléchargement de l'archive de " ++ bt p ++ " en cours…"
downloadTarballsMsg1 Russian    p = "Загрузка исходного архива " ++ bt p ++ "..."
downloadTarballsMsg1 Italian    p = "Downlaod del tarball di " ++ bt p ++ " in corso..."
downloadTarballsMsg1 Serbian    p = "Преузимање архиве изворног кода за " ++ bt p ++ "..."

displayPkgbuildMsg1 :: Language -> String -> String
displayPkgbuildMsg1 English    pkg = bt pkg ++ " does not exist."
displayPkgbuildMsg1 Japanese   pkg = bt pkg ++ "は存在しない。"
displayPkgbuildMsg1 Polish     pkg = bt pkg ++ " nie istnieje."
displayPkgbuildMsg1 Croatian   pkg = bt pkg ++ " ne postoji."
displayPkgbuildMsg1 Swedish    pkg = bt pkg ++ " finns inte."
displayPkgbuildMsg1 German     pkg = bt pkg ++ " existiert nicht."
displayPkgbuildMsg1 Spanish    pkg = bt pkg ++ " no existe."
displayPkgbuildMsg1 Portuguese pkg = bt pkg ++ " não existe."
displayPkgbuildMsg1 French     pkg = bt pkg ++ "n'existe pas."
displayPkgbuildMsg1 Russian    pkg = bt pkg ++ " не существует."
displayPkgbuildMsg1 Italian    pkg = bt pkg ++ " inesistente."
displayPkgbuildMsg1 Serbian    pkg = bt pkg ++ " не постоји."

removeMakeDepsAfterMsg1 :: Language -> String
removeMakeDepsAfterMsg1 English    = "Removing unneeded make dependencies..."
removeMakeDepsAfterMsg1 Japanese   = "あと片付け。必要ないパッケージを削除："
removeMakeDepsAfterMsg1 Polish     = "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."
removeMakeDepsAfterMsg1 Croatian   = "Uklanjanje nepotrebnih zavisnosti vezanih uz izgradnju..."
removeMakeDepsAfterMsg1 Swedish    = "Tar bort obehövda beroenden för `make`..."
removeMakeDepsAfterMsg1 German     = "Entferne nicht mehr benötigte make Abhängigkeiten..."
removeMakeDepsAfterMsg1 Spanish    = "Removiendo dependencias make innecesarias..."
removeMakeDepsAfterMsg1 Portuguese = "Removendo dependências `make` desnecessárias..."
removeMakeDepsAfterMsg1 French     = "Suppression des dépendances inutiles…"
removeMakeDepsAfterMsg1 Russian    = "Удаление ненужных зависимостей make..."
removeMakeDepsAfterMsg1 Italian    = "Rimuovo le dipendenze di compilazione..."
removeMakeDepsAfterMsg1 Serbian    = "Уклањање непотребних зависности за изградњу..."

getDowngradeChoiceMsg1 :: Language -> String -> String
getDowngradeChoiceMsg1 English    p = "What version of " ++ bt p ++ " do you want?"
getDowngradeChoiceMsg1 Japanese   p = bt p ++ "はどのバージョンにする？"
getDowngradeChoiceMsg1 Polish     p = "Którą wersję pakietu " ++ bt p ++ " zainstalować?"
getDowngradeChoiceMsg1 Croatian   p = "Koju verziju paketa " ++ bt p ++ " želite?"
getDowngradeChoiceMsg1 Swedish    p = "Vilken version av " ++ bt p ++ " vill du ha?"
getDowngradeChoiceMsg1 German     p = "Welche Version von " ++ bt p ++ " möchten Sie haben?"
getDowngradeChoiceMsg1 Spanish    p = "¿Qué versión de " ++ bt p ++ " quieres?"
getDowngradeChoiceMsg1 Portuguese p = "Qual versão de " ++ bt p ++ " deseja?"
getDowngradeChoiceMsg1 French     p = "Quelle version de " ++ bt p ++ " voulez-vous ?"
getDowngradeChoiceMsg1 Russian    p = "Какую версию " ++ bt p ++ " вы хотите?"
getDowngradeChoiceMsg1 Italian    p = "Quale versione di " ++ bt p ++ " preferisci?"
getDowngradeChoiceMsg1 Serbian    p = "Коју верзију " ++ bt p ++ "-а желите?"

backupCacheMsg1 :: Language -> String
backupCacheMsg1 English    = "No backup location given."
backupCacheMsg1 Japanese   = "バックアップ先を入力してください。"
backupCacheMsg1 Polish     = "Nie podano lokalizacji kopii zapasowych."
backupCacheMsg1 Croatian   = "Lokacija sigurnosne kopije nije specifirana."
backupCacheMsg1 Swedish    = "Ingen backup-plats specifierad."
backupCacheMsg1 German     = "Kein Sicherungsort angegeben."
backupCacheMsg1 Spanish    = "No se ha especificado localización para la copia de seguridad."
backupCacheMsg1 Portuguese = "Ainda não disse onde quer guardar o backup..."
backupCacheMsg1 French     = "Aucun lieu pour les copies de sauvegardes n'est spécifié."
backupCacheMsg1 Russian    = "Не указан путь к бэкапу."
backupCacheMsg1 Italian    = "Path per il salvataggio non specificato."
backupCacheMsg1 Serbian    = "Није дата путања ка бекапу."

backupCacheMsg2 :: Language -> String
backupCacheMsg2 English    = "You must be root to backup the cache."
backupCacheMsg2 Japanese   = "rootじゃないとバックアップはできない。"
backupCacheMsg2 Polish     = "Musisz być rootem, by zrobić kopię zapasową pamięci podręcznej."
backupCacheMsg2 Croatian   = "Za stvaranje sigurnosne kopije skladišta potrebne su root ovlasti."
backupCacheMsg2 Swedish    = "Du måste vara root för att ta backup på cache-filer."
backupCacheMsg2 German     = "Sie müssen root sein um den Cache zu sichern."
backupCacheMsg2 Spanish    = "Debes ser root para hacer una copia de seguridad de la caché."
backupCacheMsg2 Portuguese = "Precisa ser root para fazer um backup do cache."
backupCacheMsg2 French     = "Vous devez être `root` pour faire une copie de sauvegarde du cache."
backupCacheMsg2 Russian    = "Чтобы создать бэкап кэша, вы должны быть рутом"
backupCacheMsg2 Italian    = "Devi essere root per salvare la cache."
backupCacheMsg2 Serbian    = "Морате бити root да бисте бекаповали кеш."

backupCacheMsg3 :: Language -> String
backupCacheMsg3 English    = "The backup location does not exist."
backupCacheMsg3 Japanese   = "バックアップ先は存在しない。"
backupCacheMsg3 Polish     = "Lokalizacja kopii zapasowych nie istnieje."
backupCacheMsg3 Croatian   = "Lokacija sigurnosne kopije ne postoji."
backupCacheMsg3 Swedish    = "Specifierad backup-plats finns inte."
backupCacheMsg3 German     = "Der Sicherungsort existiert nicht."
backupCacheMsg3 Spanish    = "La localización para copia de seguridad no existe."
backupCacheMsg3 Portuguese = "O caminho indicado para o backup não existe."
backupCacheMsg3 French     = "Le lieu des copies de sauvegarde spécifié n'existe pas."
backupCacheMsg3 Russian    = "Путь к бэкапу не существует."
backupCacheMsg3 Italian    = "L'indirizzo del salvataggio non esiste."
backupCacheMsg3 Serbian    = "Путања ка бекапу не постоји."

backupCacheMsg4 :: Language -> String -> String
backupCacheMsg4 English    dir = "Backing up cache to " ++ bt dir
backupCacheMsg4 Japanese   dir = "キャッシュのバックアップ先：" ++ bt dir
backupCacheMsg4 Polish     dir = "Tworzenie kopii zapasowej pamięci podręcznej w " ++ bt dir
backupCacheMsg4 Croatian   dir = "Stvaram sigurnosnu kopiju u " ++ bt dir
backupCacheMsg4 Swedish    dir = "Tar backup på cache-filer till " ++ bt dir
backupCacheMsg4 German     dir = "Sichere Cache in " ++ bt dir
backupCacheMsg4 Spanish    dir = "Haciendo una copia de seguridad de la caché en " ++ bt dir
backupCacheMsg4 Portuguese dir = "Backup do cache sendo feito em " ++ bt dir
backupCacheMsg4 French     dir = "Copie de sauvegarde dans " ++ bt dir ++ "."
backupCacheMsg4 Russian    dir = "Бэкап создается в директории " ++ bt dir
backupCacheMsg4 Italian    dir = "Salvataggio della chace in " ++ bt dir
backupCacheMsg4 Serbian    dir = "Бекапујем кеш у " ++ bt dir

backupCacheMsg5 :: Language -> Int -> String
backupCacheMsg5 English    n = "Package files to backup: " ++ bt (show n)
backupCacheMsg5 Japanese   n = "パッケージのファイル数：" ++ bt (show n)
backupCacheMsg5 Polish     n = "Pliki będące częścią kopii zapasowej: " ++ bt (show n)
backupCacheMsg5 Croatian   n = "Datoteke koje su dio sigurnosne kopije: " ++ bt (show n)
backupCacheMsg5 Swedish    n = "Paket-filer att ta backup på: " ++ bt (show n)
backupCacheMsg5 German     n = "Zu sichernde Paketdateien: " ++ bt (show n)
backupCacheMsg5 Spanish    n = "Ficheros de paquetes de los que se hará copia de seguridad: " ++ bt (show n)
backupCacheMsg5 Portuguese n = "Arquivos de pacotes para backup: " ++ bt (show n)
backupCacheMsg5 French     n = "Copie de sauvegarde des fichiers de paquets suivants : " ++ bt (show n)
backupCacheMsg5 Russian    n = "Упакуйте файлы для бэкапа: " ++ bt (show n)
backupCacheMsg5 Italian    n = "File del pacchetto da salvare: " ++ bt (show n)
backupCacheMsg5 Serbian    n = "Датотеке за бекап: " ++ bt (show n)

backupCacheMsg6 :: Language -> String
backupCacheMsg6 English    = "Proceed with backup?"
backupCacheMsg6 Japanese   = "バックアップを実行する？"
backupCacheMsg6 Polish     = "Kontynuować tworzenie kopii zapasowej?"
backupCacheMsg6 Croatian   = "Nastavi sa stvaranjem sigurnosne kopije?"
backupCacheMsg6 Swedish    = "Fortsätt med backup?"
backupCacheMsg6 German     = "Sicherung fortsetzen?"
backupCacheMsg6 Spanish    = "¿Proceder con la copia de seguridad?"
backupCacheMsg6 Portuguese = "Proceder com o backup?"
backupCacheMsg6 French     = "Procéder à la copie de sauvegarde ?"
backupCacheMsg6 Russian    = "Продолжить создание бэкапа?"
backupCacheMsg6 Italian    = "Procedere con il salvataggio?"
backupCacheMsg6 Serbian    = "Наставити бекаповање?"

backupCacheMsg7 :: Language -> String
backupCacheMsg7 English    = "Backup manually aborted."
backupCacheMsg7 Japanese   = "バックアップは意図的に阻止された。"
backupCacheMsg7 Polish     = "Tworzenie kopii zapasowej zostało przerwane przez użytkownika."
backupCacheMsg7 Croatian   = "Stvaranje sigurnosne kopije prekinuto od strane korisnika."
backupCacheMsg7 Swedish    = "Backup avbröts manuellt."
backupCacheMsg7 German     = "Backup durch Benutzer abgebrochen."
backupCacheMsg7 Spanish    = "Copia de seguridad abortada manualmente."
backupCacheMsg7 Portuguese = "Backup manualmente abortado."
backupCacheMsg7 French     = "Copie de sauvegarde manuelle annulée."
backupCacheMsg7 Russian    = "Создание бэкапа прервано пользователем."
backupCacheMsg7 Italian    = "Salvataggio manuale interrotto."
backupCacheMsg7 Serbian    = "Бекаповање је ручно прекинуто."

backupCacheMsg8 :: Language -> String
backupCacheMsg8 English    = "Backing up. This may take a few minutes..."
backupCacheMsg8 Japanese   = "バックアップ中。数分かかるかもしれない。"
backupCacheMsg8 Polish     = "Tworzenie kopii zapasowej. To może potrwać kilka minut..."
backupCacheMsg8 Croatian   = "Stvaranje sigurnosne kopije. Ovo može potrajati nekoliko minuta..."
backupCacheMsg8 Swedish    = "Tar backup. Det här kan ta ett tag..."
backupCacheMsg8 German     = "Sichere. Dies kann ein paar Minuten dauern..."
backupCacheMsg8 Spanish    = "Haciendo copia de seguridad. Esto puede tardar unos minutos..."
backupCacheMsg8 Portuguese = "Efetuando backup. Isso pode levar alguns minutos..."
backupCacheMsg8 French     = "Copie de sauvegarde en cours. Ceci peut prendre quelques minutes…"
backupCacheMsg8 Russian    = "Создается бэкап. Это может занять пару минут..."
backupCacheMsg8 Italian    = "Salvataggio. Questo potrebbe richiedere qualche minuto..."
backupCacheMsg8 Serbian    = "Бекапујем. Ово може да потраје пар минута..."

copyAndNotifyMsg1 :: Language -> Int -> String
copyAndNotifyMsg1 English    n = "Copying #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Japanese   n = "#[" ++ cyan (show n) ++"]をコピー中・・・"
copyAndNotifyMsg1 Polish     n = "Kopiowanie #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Croatian   n = "Kopiranje #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Swedish    n = "Kopierar #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 German     n = "Kopiere #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Spanish    n = "Copiando #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Portuguese n = "Copiando #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 French     n = "Copie de #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Russian    n = "Копируется #[" ++ cyan (show n) ++ "]"
copyAndNotifyMsg1 Italian    n = "Copiando #[" ++cyan (show n) ++ "]"
copyAndNotifyMsg1 Serbian    n = "Копирам #[" ++ cyan (show n) ++ "]"

preCleanCacheMsg1 :: Language -> String -> String
preCleanCacheMsg1 English    n = bt n ++ " is not a number."
preCleanCacheMsg1 Japanese   n = bt n ++ "は数字はない。"
preCleanCacheMsg1 Polish     n = bt n ++ " nie jest liczbą."
preCleanCacheMsg1 Croatian   n = bt n ++ " nije broj. "
preCleanCacheMsg1 Swedish    n = bt n ++ " är inte ett nummer."
preCleanCacheMsg1 German     n = bt n ++ " ist keine Nummer."
preCleanCacheMsg1 Spanish    n = bt n ++ " no es un número."
preCleanCacheMsg1 Portuguese n = bt n ++ " não é um número."
preCleanCacheMsg1 French     n = bt n ++ " n'est pas un nombre."
preCleanCacheMsg1 Russian    n = bt n ++ " не является числом."
preCleanCacheMsg1 Italian    n = bt n ++ " non è un numero."
preCleanCacheMsg1 Serbian    n = bt n ++ " није број."

cleanCacheMsg1 :: Language -> String
cleanCacheMsg1 English    = "Invalid number given."
cleanCacheMsg1 Japanese   = "入力の数字は適切ではない。"
cleanCacheMsg1 Polish     = "Nieprawidłowa liczba."
cleanCacheMsg1 Croatian   = "Pogrešan broj."
cleanCacheMsg1 Swedish    = "Ogiltigt nummer specifierat."
cleanCacheMsg1 German     = "Ungültige Nummer gegeben."
cleanCacheMsg1 Spanish    = "Número inválido."
cleanCacheMsg1 Portuguese = "Número inválido."
cleanCacheMsg1 French     = "Nombre invalide."
cleanCacheMsg1 Russian    = "Дано невалидное число."
cleanCacheMsg1 Italian    = "Numero non valido."
cleanCacheMsg1 Serbian    = "Број није валидан."

cleanCacheMsg2 :: Language -> String
cleanCacheMsg2 English    = "This will delete the ENTIRE package cache."
cleanCacheMsg2 Japanese   = "パッケージ・キャッシュは完全に削除される。"
cleanCacheMsg2 Polish     = "To usunie WSZYSTKIE pakiety z pamięci podręcznej."
cleanCacheMsg2 Croatian   = "Ovo će izbrisati CIJELO skladište paketa."
cleanCacheMsg2 Swedish    = "Detta kommer ta bort HELA paket-cachen."
cleanCacheMsg2 German     = "Das wird den GESAMTEN Paketcache leeren."
cleanCacheMsg2 Spanish    = "Esto eliminará POR COMPLETO la caché de paquetes."
cleanCacheMsg2 Portuguese = "Isso eliminara TODOS OS PACOTES do cache."
cleanCacheMsg2 French     = "Ceci va COMPLÉTEMENT supprimer le cache des paquets."
cleanCacheMsg2 Russian    = "Это действие ВСЕЦЕЛО уничтожит кэш пакетов."
cleanCacheMsg2 Italian    = "Questo cancellera l'INTERA cache dei pacchetti."
cleanCacheMsg2 Serbian    = "Ово ће избрисати ЦЕО кеш пакета."

cleanCacheMsg3 :: Language -> Int -> String
cleanCacheMsg3 English    n = bt (show n) ++ " of each package file will be kept."
cleanCacheMsg3 Japanese   n = "パッケージ・ファイルは" ++ bt (show n) ++ "個保存される。"
cleanCacheMsg3 Polish     n = bt (show n) ++ " wersji każdego pakietu zostanie zachowane."
cleanCacheMsg3 Croatian   n = bt (show n) ++ " zadnjih verzija svakog paketa će biti zadržano."
cleanCacheMsg3 Swedish    n = bt (show n) ++ " av varje paketfil kommer att sparas."
cleanCacheMsg3 German     n = bt (show n) ++ " jeder Paketdatei wird behalten."
cleanCacheMsg3 Spanish    n = bt (show n) ++ " ficheros de cada paquete se mantendrán."
cleanCacheMsg3 Portuguese n = bt (show n) ++ " arquivos de cada pacote serão mantidos."
cleanCacheMsg3 French     n = bt (show n) ++ " fichiers de chaque paquet sera conservé."
cleanCacheMsg3 Russian    n = bt (show n) ++ " версии каждого пакета будут нетронуты."
cleanCacheMsg3 Italian    n = bt (show n) ++ " di ciascun pacchetto sarà mantenuto."
cleanCacheMsg3 Serbian    n = bt (show n) ++ " верзије сваког од пакета ће бити сачуване."

cleanCacheMsg4 :: Language -> String
cleanCacheMsg4 English    = "The rest will be deleted. Okay?"
cleanCacheMsg4 Japanese   = "残りは全部削除される。承知する？"
cleanCacheMsg4 Polish     = "Wszystko inne zostanie usunięte. Na pewno?"
cleanCacheMsg4 Croatian   =  "Ostali paketi će biti izbrisani iz skladišta. Jeste li sigurni?"
cleanCacheMsg4 Swedish    = "Resten kommer att tas bort. Är det OK?"
cleanCacheMsg4 German     = "Der Rest wird gelöscht. Ist das OK?"
cleanCacheMsg4 Spanish    = "El resto se eliminará. ¿OK?"
cleanCacheMsg4 Portuguese = "O resto será deletado. OK?"
cleanCacheMsg4 French     = "Le reste sera supprimé. Êtes-vous d'accord ?"
cleanCacheMsg4 Russian    = "Всё остальное будет удалено. Годится?"
cleanCacheMsg4 Italian    = "Il resto verrà mantenuto. Continuare?"
cleanCacheMsg4 Serbian    = "Остатак ће бити избрисан. Да ли је то у реду?"

cleanCacheMsg5 :: Language -> String
cleanCacheMsg5 English    = "Cache cleaning manually aborted."
cleanCacheMsg5 Japanese   = "削除の続行は意図的に阻止された。"
cleanCacheMsg5 Polish     = "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."
cleanCacheMsg5 Croatian   = "Čišćenje skladišta paketa prekinuto od strane korisnika."
cleanCacheMsg5 Swedish    = "Cache-rensning avbröts manuellt."
cleanCacheMsg5 German     = "Säubern des Caches durch Benutzer abgebrochen."
cleanCacheMsg5 Spanish    = "Limpieza de la caché abortada manualmente."
cleanCacheMsg5 Portuguese = "Limpeza do cache abortada manualmente."
cleanCacheMsg5 French     = "Le nettoyage du cache a été arrêté manuellement."
cleanCacheMsg5 Russian    = "Очистка кэша прервана пользователем."
cleanCacheMsg5 Italian    = "Pulitura manuale della cache interrotta."
cleanCacheMsg5 Serbian    = "Чишћење кеша је ручно прекинуто."

cleanCacheMsg6 :: Language -> String
cleanCacheMsg6 English    = "Cleaning package cache..."
cleanCacheMsg6 Japanese   = "パッケージ・キャッシュを掃除中・・・"
cleanCacheMsg6 Polish     = "Czyszczenie pamięci podręcznej..."
cleanCacheMsg6 Croatian   = "Čišćenje skladišta paketa..."
cleanCacheMsg6 Swedish    = "Rensar paket-cache..."
cleanCacheMsg6 German     = "Säubere Paketcache..."
cleanCacheMsg6 Spanish    = "Limpiando la caché de paquetes..."
cleanCacheMsg6 Portuguese = "Limpando cache de pacotes..."
cleanCacheMsg6 French     = "Nettoyage du cache des paquets…"
cleanCacheMsg6 Russian    = "Очистка кэша пакета..."
cleanCacheMsg6 Italian    = "Ripulisco la cache..."
cleanCacheMsg6 Serbian    = "Чишћење кеша..."

logLookUpFields :: Language -> [String]
logLookUpFields English    = [ "Package","First Install","Upgrades","Recent Actions" ]
logLookUpFields Japanese   = [ "パッケージ","初インストール","アップグレード回数","近況" ]
logLookUpFields Polish     = [ "Pakiet","Pierwsza instalacja","Aktualizacje","Ostatnie akcje" ]
logLookUpFields Croatian   = [ "Paket","Prva instalacija","Nadogradnje","Nedavne radnje" ]
logLookUpFields Swedish    = [ "Paket","Första installation","Uppgraderingar","Nyliga händelser" ]
logLookUpFields German     = [ "Paket","Erste Installation","Aktualisierungen","Letzte Aktionen" ]
logLookUpFields Spanish    = [ "Paquete","Primera instalación","Actualizaciones","Acciones Recientes" ]
logLookUpFields Portuguese = [ "Pacote","Primeira instalação","Atualizações","Ações Recentes" ]
logLookUpFields French     = [ "Paquet","Première installation","Mises à jours ","Actions récentes" ]
logLookUpFields Russian    = [ "Пакет","Первая установка","Обновления","Недавние действия" ]
logLookUpFields Italian    = [ "Package","Prima installazione","Upgrades","Azioni recenti" ]
logLookUpFields Serbian    = [ "Пакет","Прва инсталација","Ажурирања","Недавне радње" ]

reportNotInLogMsg1 :: Language -> String
reportNotInLogMsg1 English    = "These have not appeared in the log file:"
reportNotInLogMsg1 Japanese   = "logファイルには出ていない："
reportNotInLogMsg1 Polish     = "Tych pakietów nie ma w dzienniku:"
reportNotInLogMsg1 Croatian   = "Ovih paketa nema u dnevniku:"
reportNotInLogMsg1 Swedish    = "Dessa har inte framkommit i loggfiler:"
reportNotInLogMsg1 German     = "Diese sind nicht in der Logdatei aufgetaucht:"
reportNotInLogMsg1 Spanish    = "Estos no aparecen en el fichero log:"
reportNotInLogMsg1 Portuguese = "Os seguintes não apareceram no log de arquivo:"
reportNotInLogMsg1 French     = "Ceci n'apparaît pas des les journaux (log) :"
reportNotInLogMsg1 Russian    = "Следующих пакетов нет в лог-файле:"
reportNotInLogMsg1 Italian    = "Questo non apparirà nei file di log;"
reportNotInLogMsg1 Serbian    = "Ови пакети се не спомињу у дневнику:"

manpageMsg :: Language -> String
manpageMsg English    = "See the aura man page for aura option details."
manpageMsg Japanese   = "選択肢の詳しいことは、auraのman pageまで。"
manpageMsg Polish     = "W podręczniku man dla aura znajduje się więcej informacji o opcjach."
manpageMsg Croatian   = "Pogledajte Aura man stranicu za detalje o opcijama."
manpageMsg Swedish    = "Hänvisa till auras `man`-sida för detaljerade alternativ."
manpageMsg German     = "Lesen Sie die aura man-Seite für Details zu aura Optionen."
manpageMsg Spanish    = "Lee la página de manual de aura para detalles sobre las opciones."
manpageMsg Portuguese = "Leia a man page do aura para mais detalhes sobre as opções"
manpageMsg French     = "Voir le manuel d'Aura (`man aura`) pour le détail des options."
manpageMsg Russian    = "Чтобы узнать подробное описание опций aura, см. мануал."
manpageMsg Italian    = "Guardare la man page di Aura per maggiori dettagli sulle opzioni."
manpageMsg Serbian    = "За детаље о опцијама, погледајте man страницу Аура."

displayOutputLanguagesMsg1 :: Language -> String
displayOutputLanguagesMsg1 English    = "The following languages are available:"
displayOutputLanguagesMsg1 Japanese   = "auraは下記の言語に対応している："
displayOutputLanguagesMsg1 Polish     = "Następujące języki są dostępne:"
displayOutputLanguagesMsg1 Croatian   = "Dostupni su sljedeći jezici:"
displayOutputLanguagesMsg1 Swedish    = "Följande språk är tillängliga:"
displayOutputLanguagesMsg1 German     = "Die folgenden Sprachen sind verfügbar:"
displayOutputLanguagesMsg1 Spanish    = "Los siguientes idiomas están disponibles:"
displayOutputLanguagesMsg1 Portuguese = "Os seguintes idiomas estão disponíveis:"
displayOutputLanguagesMsg1 French     = "Les langues suivantes sont disponibles :"
displayOutputLanguagesMsg1 Russian    = "Доступны следующие языки:"
displayOutputLanguagesMsg1 Italian    = "Sono disponibili le seguenti lingue:"
displayOutputLanguagesMsg1 Serbian    = "Доступни су следећи језици:"

----------------------
-- AuraFlags functions
----------------------
inheritedOperTitle :: Language -> String
inheritedOperTitle English     = "Inherited Pacman Operations"
inheritedOperTitle Japanese    = "Pacmanからの引継選択肢"
inheritedOperTitle Polish      = "Operacje z Pacmana"
inheritedOperTitle Croatian    = "Pacman operacije"
inheritedOperTitle Swedish     = "Ärvda pacman-operationer"
inheritedOperTitle German      = "Von Pacman geerbte Operationen"
inheritedOperTitle Spanish     = "Operaciones Heredadas de Pacman"
inheritedOperTitle Portuguese  = "Operações herdadas do Pacman"
inheritedOperTitle French      = "Opérations héritées de Pacman"
inheritedOperTitle Russian     = "Позаимствованные из pacman действия"
inheritedOperTitle Italian     = "Operazioni riguardanti Pacman"
inheritedOperTitle Serbian     = "Наслеђене pacman-ове операције"

auraOperTitle :: Language -> String
auraOperTitle English    = "Aura Only Operations:"
auraOperTitle Japanese   = "Auraだけの選択肢："
auraOperTitle Polish     = "Operacje Aury:"
auraOperTitle Croatian   = "Aura operacije:"
auraOperTitle Swedish    = "Aura-specifika operationer:"
auraOperTitle German     = "Aura-spezifische Operationen:"
auraOperTitle Spanish    = "Operaciones Exclusivas de Aura:"
auraOperTitle Portuguese = "Operações exclusivas do Aura:"
auraOperTitle French     = "Opérations propres à Aura :"
auraOperTitle Russian    = "Специфичные для aura действия:"
auraOperTitle Italian    = "Operazioni esclusive di Aura:"
auraOperTitle Serbian    = "Аура-специфичне операције:"

aurSy :: Language -> String
aurSy English    = green "Perform actions involving the [A]UR.\n" ++ "Default action installs from the AUR."
aurSy Japanese   = green "[A]URに関連する処理\n" ++ "デフォルトでAURからインストール"
aurSy Polish     = green "Wykonuje akcje związane z [A]UR.\n" ++ "Domyślnie instaluje pakiety z AUR."
aurSy Croatian   = green "Izvršava radnje vezane uz [A]UR.\n" ++ "Prešutna (default) radnja je instaliranje paketa iz AUR-a."
aurSy Swedish    = green "Utför åtgärder involverandes [A]UR.\n" ++ "Standard-åtgärd installerar ifrån AUR."
aurSy German     = green "Führe Aktionen aus die das [A]UR betreffen.\n" ++ "Standardaktion installiert aus dem AUR."
aurSy Spanish    = green "Realizar acciones relacionadas con el [A]UR.\n" ++ "La acción por defecto es instalar desde AUR."
aurSy Portuguese = green "Realizar ações envolvendo o [A]UR.\n" ++ "Ação padrão instala do AUR."
aurSy French     = green "Actions impliquant [A]UR.\n" ++ "Par default, installe depuis AUR."
aurSy Russian    = green "Совершить действия с участием [A]UR.\n" ++ "Действие по умолчанию устанавливает из AUR."
aurSy Italian    = green "Azioni riguardanti [A]UR.\n" ++ "Di default installa da AUR."
aurSy Serbian    = green "Извршава радње везане за [A]UR.\n" ++ "Уобичајена радња инсталира из AUR-а."

-- NEEDS TRANSLATIONS
saveS :: Language -> String
saveS Japanese = yellow "パッケージの設置状態に関する処理\n" ++ "デフォルトでインストール状態を保存する。"
saveS Serbian  = yellow "Управља чувањем и враћањем глобалног стања пакета.\n" ++ "Уобичајена радња чува тренутно стање."
saveS _        = yellow "Manage the [S]aving and restoring of the global package state.\n" ++ "Default action saves this state."

-- Any way for the Spanish line to be shorter?
downG :: Language -> String
downG English    = red "Perform actions involving the package [C]ache.\n" ++ "Default action downgrades given packages."
downG Japanese   = red "キャッシュに関連する処理\n" ++ "デフォルトでパッケージをダウングレード"
downG Polish     = red "Wykonuje akcje związane z pamięcią podręczną ([C]ache) pakietów.\n" ++ "Domyślnie instaluje starsze wersje podanych pakietów."
downG Croatian   = red "Izvršava radnje vezane uz skladište ([C]ache) paketa.\n" ++ "Prešutna (default) radnja je vraćanje paketa na prijašnju verziju."
downG Swedish    = red "Utför åtgärder involverandes paket-[C]ache.\n" ++ "Standard-åtgärd nergraderar valda paket."
downG German     = red "Führe Aktionen aus die den Paket[C]ache betreffen.\n" ++ "Standardaktion downgradet gegebene Pakete."
downG Spanish    = red "Realizar acciones relacionadas con la [C]aché.\n" ++ "La acción por defecto es retornar a versiones antiguas de los paquetes especificados."
downG Portuguese = red "Realiza ações relacionadas ao [C]ache.\n" ++ "Ação padrão retorna os pacotes informados às suas versões anteriores."
downG French     = red "Actions impliquant le [C]ache des paquets.\n" ++ "Par default, rétrograde les paquets spécifiés."
downG Russian    = red "Совершить действия с участием кэша пакета ([C]ache).\n" ++ "Действие по умолчанию откатывает данные пакеты к старым версиям."
downG Italian    = red "Azioni riguardanti la [C]ache dei pacchetti.\n" ++ "Di default retrocede il pacchetti."
downG Serbian    = red "Извршава радње везане за кеш пакета.\n" ++ "Уобичајена радња враћа претходну верзију датих пакета."

viewL :: Language -> String
viewL English    = cyan "Perform actions involving the pacman [L]ogfile.\n" ++ "Default action opens the log for read-only viewing."
viewL Japanese   = cyan "[L]ogfileに関連する処理\n" ++ "デフォルトでlogfileを閲覧用に開く"
viewL Polish     = cyan "Wykonuje akcje związane z dziennikiem ([L]ogiem) pacmana.\n" ++ "Domyślnie otwiera log w trybie tylko do odczytu."
viewL Croatian   = cyan "Izvršavanje radnje vezane uz pacman dnevnik ([L]ogfile).\n" ++ "Prešutna (default) radnja je ispis dnevnika."
viewL Swedish    = cyan "Utför åtgärder involverandes pacmans [L]ogfil.\n" ++ "Standard-åtgärd öppnar loggen med read-only-attribut."
viewL German     = cyan "Führe Aktionen aus die die Pacman [L]ogdatei betreffen.\n" ++ "Standardaktion öffnet den Log (nur Lesen)"
viewL Spanish    = cyan "Realizar acciones relacionadas con el fichero [L]og de pacman.\n" ++ "La acción por defecto es abrir el log en modo sólo lectura."
viewL Portuguese = cyan "Realiza ações relacionadas ao [L]ogfile do Pacman.\n" ++ "Ação padrão abre o arquivo de log apenas para leitura."
viewL French     = cyan "Actions impliquant le [L]ogfile (journal) de Pacman.\n" ++ "Par default, ouvre le journal en lecture seule."
viewL Russian    = cyan "Совершить действия с участием [L]og-файлов pacman.\n" ++ "Действие по умолчанию открывает лог для просмотра в режиме для чтения."
viewL Italian    = cyan "Azioni riguardanti i [L]ogfile di pacman.\n" ++ "Di default visualizza il log in sola lettura."
viewL Serbian    = cyan "Извршава радње везане за pacman-ов дневник.\n" ++ "Уобичајена радња даје преглед дневника."

orpha :: Language -> String
orpha English    = blue "Perform actions involving [O]rphan packages.\n" ++ "Default action lists all orphan packages."
orpha Japanese   = blue "必要とされていない従属パッケージに関する処理\n" ++ "デフォルトでその従属パッケージの名前を出力"
orpha Polish     = blue "Wykonuje akcje związane z [O]sieroconymi pakietami.\n" ++ "Domyślnie wyświetla wszystkie osierocone pakiety."
orpha Croatian   = blue "Izvršava radnje vezane uz pakete bez roditelja ([O]rphan).\n" ++ "Prešutna (default) radnja je izlistavanje paketa bez roditelja."
orpha Swedish    = blue "Utför åtgärder involverandes [O]rphan-paket.\n" ++ "Standard-åtgärd listar alla orphan-paket."
orpha German     = blue "Führe Aktionen aus die verwaiste ([O]rphans) Pakete betreffen.\n" ++ "Standardaktion listet alle verwaisten Pakete auf."
orpha Spanish    = blue "Realizar acciones relacionadas con paquetes huérfanos ([O]rphan).\n" ++ "La acción por defecto es listar todos los paquetes huérfanos."
orpha Portuguese = blue "Realiza ações com pacotes [O]rfãos.\n" ++ "Ação padrão lista todos os pactes orfãos."
orpha French     = blue "Actions impliquant les paquets [O]rphelins.\n" ++ "Par default, liste l'ensemble des paquets orphelins."
orpha Russian    = blue "Совершить действия с участием [O]сиротевших пакетов.\n" ++ "Действие по умолчанию берёт в расчёт все осиротевшие пакеты."
orpha Italian    = blue "Azioni riguardanti i pacchetti [O]rfani.\n" ++ "Di default elenca i pacchetti orfani."
orpha Serbian    = blue "Извршава радње везане за пакете без родитеља.\n" ++ "Уобичајена радња листа пакете без родитеља."

--------------------------
-- AurConnection functions
--------------------------
getAURPkgInfoMsg1 :: Language -> String
getAURPkgInfoMsg1 Japanese = "AURのAPIに繋げなかった。ネット接続状態を確認して下さい。"
getAURPkgInfoMsg1 Serbian  = "Приступ AUR-у није успео. Проверите вашу везу."
getAURPkgInfoMsg1 _        = "AUR API lookup failed. Please check your connection."

infoFields :: Language -> [String]
infoFields English    = [ "Repository","Name","Version","AUR Status","Project URL","AUR URL","License", "Votes","Description" ]
infoFields Japanese   = [ "リポジトリ","名前","バージョン","パッケージ状態","プロジェクト","パッケージページ","ライセンス","投票数","概要" ]
infoFields Polish     = [ "Repository","Nazwa","Wersja","Status w AUR","URL Projektu","URL w AUR","Licencja","Głosy","Opis" ]
infoFields Croatian   = [ "Repository","Ime","Verzija","AUR Stanje","URL Projekta","AUR URL","Licenca","Glasovi","Opis" ]
infoFields Swedish    = [ "Repository","Namn","Version","AUR Status","Projekt URL","AUR URL","Licens","Röster","Beskrivning" ]
infoFields German     = [ "Repository","Name","Version","AUR Status","Projekt URL","AUR URL","Lizenz","Stimmen","Beschreibung" ]
infoFields Spanish    = [ "Repository","Nombre","Versión","Estado en AUR","URL del proyecto","URL en AUR","Licencia", "Votos","Descripción" ]
infoFields Portuguese = [ "Repositório","Nome","Versão","Estado no AUR","URL do projeto","URL no AUR","Licença", "Votos","Descrição" ]
infoFields French     = [ "Dépôt","Nom","Version","AUR Statut","URL du projet","URL AUR","License", "Votes","Description" ]
infoFields Russian    = [ "Репозиторий","Название","Версия","Статус в AUR","URL проекта","URL в AUR","Лицензия", "Рейтинг","Описание" ]
infoFields Italian    = [ "Repository","Nome","Versione","Stato in AUR","URL","URL AUR","Licenza","Voti","Descrizione" ]
infoFields Serbian    = [ "Ризница","Име","Верзија","Статус у AUR-у","Страница пројекта","Страница у AUR-у","Лиценца","Гласови","Опис" ]


outOfDateMsg :: Language -> Bool -> String
outOfDateMsg English    True  = red "Out of Date!"
outOfDateMsg English    False = green "Up to Date"
outOfDateMsg Japanese   True  = red "AURで要更新！"
outOfDateMsg Japanese   False = green "最新"
outOfDateMsg Polish     True  = red "Nieaktualny!"
outOfDateMsg Polish     False = green "Aktualny"
outOfDateMsg Croatian   True  = red "Zastarjelo!"
outOfDateMsg Croatian   False = green "Ažurirano"
outOfDateMsg Swedish    True  = red "Utdaterad!"
outOfDateMsg Swedish    False = green "Aktuell"
outOfDateMsg German     True  = red "Veraltet!"
outOfDateMsg German     False = green "Aktuell"
outOfDateMsg Spanish    True  = red "¡Desactualizado!"
outOfDateMsg Spanish    False = green "Actualizado"
outOfDateMsg Portuguese True  = red "Desatualizado!"
outOfDateMsg Portuguese False = green "Atualizado"
outOfDateMsg French     True  = red "Périmé !"
outOfDateMsg French     False = green "À jour"
outOfDateMsg Russian    True  = red "Устарел!"
outOfDateMsg Russian    False = green "Новейший"
outOfDateMsg Italian    True  = red "Out of Date!"
outOfDateMsg Italian    False = green "Aggiornato"
outOfDateMsg Serbian    True  = red "Застарео!"
outOfDateMsg Serbian    False = green "Ажуран"

------------------
-- State functions
------------------
saveStateMsg1 :: Language -> String
saveStateMsg1 Japanese = "現在パッケージ状態保存完了。"
saveStateMsg1 Serbian  = "Сачувано стање пакета."
saveStateMsg1 _        = "Saved package state."

restoreStateMsg1 :: Language -> String
restoreStateMsg1 Japanese = "対象バージョンがないパッケージ："
restoreStateMsg1 Serbian  = "Захтеване старе верзије нису доступне за:"
restoreStateMsg1 _        = "Requested downgrade versions not available for:"

downgradeAndRemoveMsg1 :: Language -> String
downgradeAndRemoveMsg1 Japanese = "パッケージを変更する必要ない。"
downgradeAndRemoveMsg1 Serbian  = "Ниједан пакет не захтева измене."
downgradeAndRemoveMsg1 _        = "No packages need changing."

----------------
-- `B` functions
----------------
cleanStates_1 :: Language -> String
cleanStates_1 Japanese = "入力は数字ではない。"
cleanStates_1 Serbian  = "Улаз није валидан број."
cleanStates_1 _        = "Input isn't a valid number."

cleanStates_2 :: Language -> Int -> String
cleanStates_2 Japanese n = bt (show n) ++ "個のパッケージ状態記録だけが残される。その他削除？"
cleanStates_2 Serbian  n = bt (show n) ++ " стања пакета ће бити сачувано. Уклонити остатак?"
cleanStates_2 _        n = bt (show n) ++ " package states will be kept. Remove the rest?"

cleanStates_3 :: Language -> String
cleanStates_3 Japanese = "何も削除しないで終了。"
cleanStates_3 Serbian  = "Ниједно стање пакета није уклоњено."
cleanStates_3 _        = "No package states were removed."

------------------------
-- BadPackages functions
------------------------
circDep_1 :: Language -> String -> String
circDep_1 Japanese p = bt p ++ "と互いに従属している。"
circDep_1 Serbian  p = "Има кружну зависност са " ++ bt p ++ "."
circDep_1 _        p = "Has a circular dependency with " ++ bt p ++ "."

bashisms_1 :: Language -> String
bashisms_1 Japanese = "PKGBUILDのBashコードが複雑すぎる。"
bashisms_1 Serbian  = "Превише „bash-изама“ у PKGBUILD-у."
bashisms_1 _        = "Too many bashisms in PKGBUILD."

-------------------
-- Pacman functions
-------------------
pacmanFailureMsg1 :: Language -> String
pacmanFailureMsg1 Japanese = "入力を確認して下さい。"
pacmanFailureMsg1 Serbian  = "Молим Вас, проверите ваш унос."
pacmanFailureMsg1 _        = "Please check your input."
