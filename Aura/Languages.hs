-- Library for Aura output in different languages.
-- All normal restrictions on line length do not apply for this file, and this file only.

{- AURA TRANSLATORS - Thank you all
Chris "Kwpolska" Warrick | Polish
Denis Kasak              | Croatian
"stranac"                | Croatian
Fredrik Haikarainen      | Swedish
Lukas Niederbremer       | German
Alejandro Gómez          | Spanish
Henry "Ingvij" Kupty     | Portuguese
Ma Jiehong               | French
Fabien Dubosson          | French
Kyrylo Silin             | Russian
Bob Valantin             | Italian
Filip Brcic              | Serbian
"chinatsun"              | Norwegian
-}

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

import Aura.Colour.Text (cyan, green, red, blue, yellow, magenta, bForeground)

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
              | Norwegian
                deriving (Eq,Enum,Read,Show)

translators :: [String]
translators = [ " Chris \"Kwpolska\" Warrick"
              , " Denis Kasak / \"stranac\""
              , " Fredrik Haikarainen"
              , " Lukas Niederbremer"
              , " Alejandro Gómez"
              , " Henry \"Ingvij\" Kupty"
              , " Ma Jiehong / Fabien Dubosson"
              , " Kyrylo Silin"
              , " Bob Valantin"
              , " Filip Brcic"
              , " \"chinatsun\"" ]

-- These need updating! Or removing...
languageNames :: Language -> [String]
languageNames English    = [ "Polish","Croatian","Swedish","German","Spanish","Portuguese","French","Russian", "Italian", "Serbian", "Norwegian" ]
languageNames Japanese   = [ "ポーランド語","クロアチア語","スウェーデン語","ドイツ語","スペイン語","ポルトガル語","フランス語","ロシア語", "", "", "" ]
languageNames Polish     = [ "polski","chorwacki","szwedzki","niemiecki","hiszpański","portugalski","francuski","rosyjski", "", "", "" ]
languageNames Croatian   = [ "poljski","hrvatski","švedski","njemački","španjolski","portugalski","francuski","ruski", "talijanski", "srpski", "norveški" ]
languageNames Swedish    = [ "polska","kroatiska","svenska","tyska","spanska","portugisiska", "", "", "" ]
languageNames German     = [ "Polnisch","Kroatisch","Schwedisch","Deutsch","Spanisch","Portugiesisch", "", "", "" ]
languageNames Spanish    = [ "Polaco","Croata","Sueco","Alemán","Español","Portugués", "", "", "" ]
languageNames Portuguese = [ "Polonês","Croata","Sueco","Alemão","Espanhol","Português", "", "", "" ]
languageNames French     = [ "Polonais","Croate","Suédois","Allemand","Espagnol","Portugais", "Français", "Russe", "Italien", "Serbe", "Norvégien" ]
languageNames Russian    = [ "Польский","Хорватский","Шведский","Немецкий","Испанский","Португальский", "Русский", "", "", "" ]
languageNames Italian    = [ "Polacco", "Croato", "Svedese", "Tedesco", "Spagnolo", "Portoghese", "Francese", "Russo", "Italiano", "", "" ]
languageNames Serbian    = [ "Пољски","Хрватски","Шведски","Немачки","Шпански","Португалски","Француски","Руски","Италијански","Српски", "" ]
languageNames Norwegian  = [ "Polsk","Kroatisk","Svensk","Tysk","Spansk","Portugisisk","Fransk","Russisk","Italiensk","Serbisk","Norsk" ]

translatorMsgTitle :: Language -> String
translatorMsgTitle English    = "Aura Translators:"
translatorMsgTitle Japanese   = "Auraの翻訳者："
translatorMsgTitle Polish     = "Tłumacze Aury:"
translatorMsgTitle Croatian   = "Aura Prevoditelji:"
translatorMsgTitle Swedish    = "Aura Översättare:"
translatorMsgTitle German     = "Aura Übersetzer:"
translatorMsgTitle Spanish    = "Traductores de Aura:"
translatorMsgTitle Portuguese = "Tradutores de Aura:"
translatorMsgTitle French     = "Traducteurs d'Aura:"
translatorMsgTitle Russian    = "Переводчики Aura:"
translatorMsgTitle Italian    = "Traduttori di Aura:"
translatorMsgTitle Serbian    = "Преводиоци Аура:"
translatorMsgTitle Norwegian  = "Aura Oversettere:"

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
langFromEnv ('n':'b':_) = Norwegian
langFromEnv _           = English

----------------------
-- Aura/Core functions
----------------------
-- NEEDS TRANSLATION
checkDBLock_1 :: Language -> String
checkDBLock_1 Japanese   = "パッケージデータベースが今閉鎖状態。開放したらキーを押して続行をどうぞ。"
checkDBLock_1 Croatian   = "Baza paketa je zaključana. Kad se otključa, pritisnite enter da biste nastavili."
checkDBLock_1 German     = "The Paketdatenbank ist gesperrt. Drücken Sie Enter wenn sie entsperrt ist um fortzufahren."
checkDBLock_1 Norwegian  = "Pakkedatabasen er låst. Trykk enter når den er åpnet for å fortsette."
checkDBLock_1 French     = "La base de données des paquets est bloquée. Appuyez sur enter pour continuer."
checkDBLock_1 Portuguese = "Banco de dados de pacote travado. Aperte 'enter' quando estivier detravado para poder continuar."
checkDBLock_1 _          = "The package database is locked. Press enter when it's unlocked to continue."

-- Packages should not be built if the user is logged in as root!
trueRoot_1 :: Language -> String
trueRoot_1 English    = "You should never build packages as the true root. Are you okay with this?"
trueRoot_1 Japanese   = "本当のrootユーザーとしてパッケージを作成するのが危険。続行？"
trueRoot_1 Polish     = "Nigdy nie powinieneś budować pakietów jako root. Na pewno kontynuować?"
trueRoot_1 Croatian   = "Pakete ne bi trebalo graditi sa root korisničkim računom. Nastaviti?"
trueRoot_1 Swedish    = "Det är starkt rekommenderat att INTE vara inloggad som root när man bygger paket. Vill du fortsätta ändå?"
trueRoot_1 German     = "Sie sollten niemals Pakete als der echte root Nutzer bauen. Sind sie sicher, dass Sie dies tun wollen?"
trueRoot_1 Spanish    = "Nunca deberías construir paquetes como root real. ¿Estás de acuerdo con esto?"
trueRoot_1 Portuguese = "Não deveria compilar pacotes como o root de fato. Ainda assim, deseja prosseguir?"
trueRoot_1 French     = "Il n'est pas recommandé de construire des paquets avec le compte root. Voulez-vous continuer?"
trueRoot_1 Russian    = "Вам никогда не следует собирать пакеты под настоящим рутом. Договорились?"
trueRoot_1 Italian    = "Non si dovrebbero compilare pacchetti come root. Volete Continuare?"
trueRoot_1 Serbian    = "Не би требало градити пакете са правим root овлашћењима. Желите ли наставити?"
trueRoot_1 Norwegian  = "Du bør aldri bygge pakker som root. Er du helt sikker på at du vil gjøre dette?"

-- This is for when the user decides to refrain from building afterall.
trueRoot_2 :: Language -> String
trueRoot_2 English    = "You’ve done the right thing."
trueRoot_2 Japanese   = "よしよし。"
trueRoot_2 Polish     = "Postąpiłeś słusznie."
trueRoot_2 Croatian   = "Ispravno ste postupili."
trueRoot_2 Swedish    = "Phew."
trueRoot_2 German     = "Eine weise Entscheidung."
trueRoot_2 Spanish    = "Has tomado la decision correcta."
trueRoot_2 Portuguese = "Ainda bem que tem juízo!"
trueRoot_2 French     = "C'est la bonne décision."
trueRoot_2 Russian    = "Вы выбрали православный путь."
trueRoot_2 Italian    = "Hai fatto la cosa giusta."
trueRoot_2 Serbian    = "Исправно сте поступили."
trueRoot_2 Norwegian  = "Du gjør det rette."

mustBeRoot_1 :: Language -> String
mustBeRoot_1 English    = "You have to use " ++ bt "sudo" ++ " for that."
mustBeRoot_1 Japanese   = bt "sudo" ++ "を使わないとそれができない！"
mustBeRoot_1 Polish     = "Musisz użyć " ++ bt "sudo" ++ ", żeby to zrobić."
mustBeRoot_1 Croatian   = "Morate koristiti" ++ bt "sudo" ++ "za ovu radnju."
mustBeRoot_1 Swedish    = "Du måste använda " ++ bt "sudo" ++ " för det."
mustBeRoot_1 German     = "Sie müssen dafür " ++ bt "sudo" ++ " benutzen."
mustBeRoot_1 Spanish    = "Tienes que utilizar " ++ bt "sudo" ++ " para eso."
mustBeRoot_1 Portuguese = "Utilize " ++ bt "sudo" ++ "para isso."
mustBeRoot_1 French     = "Vous devez utiliser " ++ bt "sudo" ++ " pour ça."
mustBeRoot_1 Russian    = "Необходимо использовать " ++ bt "sudo" ++ " для этого."
mustBeRoot_1 Italian    = "È necessario utilizzare " ++ bt "sudo" ++ " per questo."
mustBeRoot_1 Serbian    = "Морате да користите " ++ bt "sudo" ++ " за ову радњу."
mustBeRoot_1 Norwegian  = "Du må bruke " ++ bt "sudo" ++ " for det."

-----------------------
-- Aura/Build functions
-----------------------
buildPackages_1 :: String -> Language -> String
buildPackages_1 p English    = "Building " ++ bt p ++ "..."
buildPackages_1 p Japanese   = bt p ++ "を作成中・・・"
buildPackages_1 p Polish     = "Budowanie " ++ bt p ++ "..."
buildPackages_1 p Croatian   = "Gradim " ++ bt p ++ "..."
buildPackages_1 p Swedish    = "Bygger paket " ++ bt p ++ "..."
buildPackages_1 p German     = "Baue Paket " ++ bt p ++ "..."
buildPackages_1 p Spanish    = "Construyendo " ++ bt p ++ "..."
buildPackages_1 p Portuguese = "Compilando " ++ bt p ++ "..."
buildPackages_1 p French     = "Compilation de " ++ bt p ++ "..."
buildPackages_1 p Russian    = "Сборка " ++ bt p ++ "..."
buildPackages_1 p Italian    = "Compilazione di " ++ bt p ++ "..."
buildPackages_1 p Serbian    = "Градим " ++ bt p ++ "..."
buildPackages_1 p Norwegian  = "Bygger " ++ bt p ++ "..."

buildFail_1 :: String -> Language -> String
buildFail_1 p English    = "Well, building " ++ bt p ++ " failed."
buildFail_1 p Japanese   = bt p ++ "の作成は失敗したようだ。"
buildFail_1 p Polish     = "Budowanie " ++ bt p ++ " zakończyło się niepowodzeniem."
buildFail_1 p Croatian   = "Izgradnja " ++ bt p ++ " nije uspjela."
buildFail_1 p Swedish    = "Det gick inte att bygga paketet " ++ bt p ++ "."
buildFail_1 p German     = "Bauen von " ++ bt p ++ " ist fehlgeschlagen."
buildFail_1 p Spanish    = "La construcción de " ++ bt p ++ " ha fallado."
buildFail_1 p Portuguese = "Falha na compilação do pacote " ++ bt p ++ "."
buildFail_1 p French     = "Bon, la compilation de " ++ bt p ++ " a échoué."
buildFail_1 p Russian    = "Что ж, сборка " ++ bt p ++ " не удалась."
buildFail_1 p Italian    = "La compilazione di " ++ bt p ++ "è fallita."
buildFail_1 p Serbian    = "Изградња пакета " ++ bt p ++ " није успела."
buildFail_1 p Norwegian  = "Bygging av " ++ bt p ++ " feilet."

buildFail_2 :: Language -> String
buildFail_2 English    = "Also, the following weren’t built:"
buildFail_2 Japanese   = "ちなみに下記のパッケージも作成されなかった："
buildFail_2 Polish     = "Dodatkowo, następujące pakiety nie zostały zbudowane:"
buildFail_2 Croatian   = "Osim toga, ni sljedeće nije izgrađeno:"
buildFail_2 Swedish    = "Det gick heller inte att bygga följande paket:"
buildFail_2 German     = "Die folgenden Pakete wurden zusätzlich nicht gebaut:"
buildFail_2 Spanish    = "Los siguientes paquetes no se han construido:"
buildFail_2 Portuguese = "Os pacotes a seguir não foram compilados:"
buildFail_2 French     = "En outre, les paquets suivants n'ont pu être compilés:"
buildFail_2 Russian    = "К тому же следующие пакеты не были собраны:"
buildFail_2 Italian    = "Inoltre non è stato possibile cotruire i seguenti pacchetti:"
buildFail_2 Serbian    = "Такође, ни следећи пакети нису изграђени::"
buildFail_2 Norwegian  = "Det gikk heller ikke an å bygge følgende:"

buildFail_3 :: Language -> String
buildFail_3 English    = "However, these packages were successfully built:"
buildFail_3 Japanese   = "しかし、以下のパッケージファイルは無事作成された："
buildFail_3 Polish     = "Następujące pakiety zostały zbudowane pomyślnie:"
buildFail_3 Croatian   = "Neki paketi su ipak uspješno izgrađeni:"
buildFail_3 Swedish    = "Vissa paket kanske har byggts ordentligt (Osäker)."
buildFail_3 German     = "Diese Pakete wurden wiederrum erfolgreich gebaut:"
buildFail_3 Spanish    = "Sin embargo, los siguientes paquetes se han construido:"
buildFail_3 Portuguese = "Entretanto, os seguintes pacotes compilaram com sucesso:"
buildFail_3 French     = "Cependant, les paquets suivants ont été compilés avec succès:"
buildFail_3 Russian    = "Однако эти пакеты были успешно собраны:"
buildFail_3 Italian    = "Comunque questi pacchetti sono stato compilati con successo:"
buildFail_3 Serbian    = "Међутим, ови пакети су успешно изграђени:"
buildFail_3 Norwegian  = "Heldigvis ble de følgende pakkene bygd:"

buildFail_4 :: Language -> String
buildFail_4 English    = "Would you like to install them?"
buildFail_4 Japanese   = "できたやつのインストールを続行する？"
buildFail_4 Polish     = "Czy chcesz je zainstalować?"
buildFail_4 Croatian   = "Želite li ih instalirati?"
buildFail_4 Swedish    = "Vill du installera dem?"
buildFail_4 German     = "Möchten sie diese installieren?"
buildFail_4 Spanish    = "¿Te gustaría instalarlos?"
buildFail_4 Portuguese = "Gostaria de instalá-los?"
buildFail_4 French     = "Voulez-vous les installer?"
buildFail_4 Russian    = "Желаете ли вы их установить?"
buildFail_4 Italian    = "Volete installarli?"
buildFail_4 Serbian    = "Желите ли их инсталирати?"
buildFail_4 Norwegian  = "Vil du installere dem?"

buildFail_5 :: Language -> String
buildFail_5 English    = "Building failed."
buildFail_5 Japanese   = "パッケージ作成は失敗した。"
buildFail_5 Polish     = "Budowanie nie powiodło się."
buildFail_5 Croatian   = "Izgradnja nije uspjela."
buildFail_5 Swedish    = "Gick inte att bygga paket."
buildFail_5 German     = "Bauen fehlgeschlagen."
buildFail_5 Spanish    = "La construcción falló."
buildFail_5 Portuguese = "Falha na compilação."
buildFail_5 French     = "Compilation échouée."
buildFail_5 Russian    = "Сборка не удалась."
buildFail_5 Italian    = "Compilazione fallita."
buildFail_5 Serbian    = "Изградња пакета није успела."
buildFail_5 Norwegian  = "Bygging feilet."

-- NEEDS TRANSLATION
buildFail_6 :: Language -> String
buildFail_6 Japanese  = "それでも続行？"
buildFail_6 Croatian  = "Želite li svejedno nastaviti?"
buildFail_6 Norwegian = "Vil du fortsette likevel?"
buildFail_6 Italian   = "Vuoi continuare comunque?"
buildFail_6 French    = "Voulez-vous tout de même continuer?"
buildFail_6 _         = "Would you like to continue anyway?"

displayBuildErrors_1 :: Language -> String
displayBuildErrors_1 English    = "Dumping makepkg output in "
displayBuildErrors_1 Japanese   = "抑えていたmakepkgの出力を受け取る用意・・・"
displayBuildErrors_1 Polish     = "Wyjście makepkg zostanie wypisane za "
displayBuildErrors_1 Croatian   = "Zapisujem makepkg ispis u "
displayBuildErrors_1 Swedish    = "Dumpar makepkgs utskrift i "
displayBuildErrors_1 German     = "Schreibe makepkg Ausgabe in "
displayBuildErrors_1 Spanish    = "Volcando la salida de makepkg en "
displayBuildErrors_1 Portuguese = "Despejando a saída do makepkg em "
displayBuildErrors_1 French     = "Redirection de la sortie de makepkg dans "
displayBuildErrors_1 Russian    = "Вывод makepkg записывается в "
displayBuildErrors_1 Italian    = "Salvataggio dell'output di makepkg in "
displayBuildErrors_1 Serbian    = "Уписујем излаз makepkg-а у "
displayBuildErrors_1 Norwegian  = "Dumper makepkg's utskrift i "

------------------------------
-- Aura/Dependencies functions
------------------------------
-- Is this still used?
getDepsToInstall_1 :: Language -> String
getDepsToInstall_1 English    = "No AUR packages specified for install."
getDepsToInstall_1 Japanese   = "パッケージは一つも指摘されていない。"
getDepsToInstall_1 Polish     = "Nie podano pakietów z AUR do zainstalowania."
getDepsToInstall_1 Croatian   = "Nijedan AUR paket nije specificiran za instalaciju."
getDepsToInstall_1 Swedish    = "Inga AUR-paket är valda för installation."
getDepsToInstall_1 German     = "Keine AUR Pakete zur Installation vermerkt."
getDepsToInstall_1 Spanish    = "No se han especificado paquetes de AUR para instalar."
getDepsToInstall_1 Portuguese = "Nenhum pacote AUR foi especificado para instalação."
getDepsToInstall_1 French     = "Aucun paquet AUR n'a été spécifié pour l'installation."
getDepsToInstall_1 Russian    = "Пакеты AUR для установки не указаны."
getDepsToInstall_1 Italian    = "Nessun pacchetto di AUR specificato per l'installazione."
getDepsToInstall_1 Serbian    = "Ниједан AUR пакет није специфициран за инсталацију."
getDepsToInstall_1 Norwegian  = "Ingen pakker fra AUR er valgt for installasjon."

getRealPkgConflicts_1 :: String -> String -> String -> Language -> String
getRealPkgConflicts_1 p r d English    = "The dependency " ++ bt p ++ " demands version " ++ bt d ++ "but the most recent version is " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Japanese   = "パッケージ" ++ bt p ++ "はバージョン" ++ bt d ++ "を要するが" ++ "一番最新のバージョンは" ++ bt r ++ "。"
getRealPkgConflicts_1 p r d Polish     = "Zależność " ++ bt p ++ " powinna być w wersji " ++ bt d ++ ", ale najnowsza wersja to " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Croatian   = "Zavisnost " ++ bt p ++ " zahtjeva verziju " ++ bt d ++ ", a najnovija dostupna verzija je " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Swedish    = "Beroendepaketet " ++ bt p ++ " kräver version " ++ bt d ++ "men den senaste versionen är " ++ bt r ++ "."
getRealPkgConflicts_1 p r d German     = "Die Abhängigkeit " ++ bt p ++ " verlangt Version " ++ bt d ++ "aber die neuste Version ist " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Spanish    = "La dependencia " ++ bt p ++ " duiere la versión " ++ bt d ++ "pero la versión más reciente es " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Portuguese = "A dependência " ++ bt p ++ " exige a versão " ++ bt d ++ "mas a versão mais recente é " ++ bt r ++ "."
getRealPkgConflicts_1 p r d French     = bt p ++ " est une dépendance nécessitant une version " ++ bt d ++ ", mais la plus récente est la " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Russian    = "Зависимость " ++ bt p ++ " требует версию " ++ bt d ++ ", однако самой последней версией является " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Italian    = "La dipendenza " ++ bt p ++ " richiede la versione " ++ bt d ++ "ma la versione disponibile è " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Serbian    = "Зависност " ++ bt p ++ " захтева верзију " ++ bt d ++ ", али најновија верзија је " ++ bt r ++ "."
getRealPkgConflicts_1 p r d Norwegian  = "Avhengigheten " ++ bt p ++ " krever versjon " ++ bt d ++", men den nyeste versjonen er " ++ bt r ++ "."

getRealPkgConflicts_2 :: String -> Language -> String
getRealPkgConflicts_2 p English    = bt p ++ " is an ignored package! See your `pacman.conf` file."
getRealPkgConflicts_2 p Japanese   = bt p ++ "は無視されるパッケージ！`pacman.conf`を参考に。"
getRealPkgConflicts_2 p Polish     = bt p ++ " jest ignorowany! Sprawdź plik `pacman.conf`."
getRealPkgConflicts_2 p Croatian   = bt p ++ " je ignoriran paket! Provjerite svoj `pacman.conf`."
getRealPkgConflicts_2 p Swedish    = bt p ++ " är ett ignorerat paket! Kolla din `pacman.conf`-fil."
getRealPkgConflicts_2 p German     = bt p ++ " ist ein ignoriertes Paket! Siehe /etc/pacman.conf."
getRealPkgConflicts_2 p Spanish    = "¡" ++ bt p ++ " es un paquete ignorado! Revisa tu fichero `pacman.conf`."
getRealPkgConflicts_2 p Portuguese = bt p ++ " é um pacote ignorado conforme configuração em `pacman.conf`!"
getRealPkgConflicts_2 p French     = "Le paquet " ++ bt p ++ " est ignoré. Vous devriez jeter un œil à votre `pacman.conf`."
getRealPkgConflicts_2 p Russian    = "Пакет " ++ bt p ++ " игнорируется! Проверьте ваш файл `pacman.conf`."
getRealPkgConflicts_2 p Italian    = bt p ++ " è un pacchetto ignorato, controllare `pacman.conf`."
getRealPkgConflicts_2 p Serbian    = "Пакет " ++ bt p ++ " је игнорисан! Видите ваш фајл „pacman.conf“."
getRealPkgConflicts_2 p Norwegian  = bt p ++ " er en ignorert pakke! Sjekk din `pacman.conf`-fil."

-- NEEDS TRANSLATION
missingPkg_1 :: String -> Language -> String
missingPkg_1 p Croatian  = "Zavisnost  " ++ bt p ++ " nije pronađena. Pokušajte pronaći paket koji zadovoljava ovu zavisnost."
missingPkg_1 p Norwegian = "Avhengigheten " ++ bt p ++ " ble ikke funnet. Du kan søke etter en pakke som tilfredsstiller avhengigheten."
missingPkg_1 p Italian   = "La dipendenza " ++ bt p ++ " non è stata trovata. Potrebbe essere necessario cercare un pacchetto che possa soddisfarla?"
missingPkg_1 p French    = "La dépendance " ++ bt p ++ " n'a pas pu être trouvée. Il vous faut trouver un paquet pour la satisfaire."
missingPkg_1 p _         = "The dependency " ++ bt p ++ " could not be found. You may need to search for a package to satisfy it."

-----------------
-- aura functions
-----------------
executeOpts_1 :: Language -> String
executeOpts_1 English    = "Conflicting flags given!"
executeOpts_1 Japanese   = "矛盾しているオプションあり。"
executeOpts_1 Polish     = "Niektóre flagi są w konflikcie ze sobą!"
executeOpts_1 Croatian   = "Neke od danih zastavica nije moguće kombinirati!"
executeOpts_1 Swedish    = "Givna flaggor är i konflikt!"
executeOpts_1 German     = "Gegebene Kommandozeilenflags sind widersprüchlich!"
executeOpts_1 Spanish    = "¡Flags contradictorios!"
executeOpts_1 Portuguese = "Flags conflitantes!"
executeOpts_1 French     = "Arguments contradictoires!"
executeOpts_1 Russian    = "Даны конфликтующие флаги!"
executeOpts_1 Italian    = "Argomenti in conflitto!"
executeOpts_1 Serbian    = "Захтеване опције су контрадикторне!"
executeOpts_1 Norwegian  = "Motstridene flagg er spesifisert!"

manpageMsg :: Language -> String
manpageMsg English    = "See the aura man page for aura option details."
manpageMsg Japanese   = "選択肢の詳しいことは、auraのman pageまで。"
manpageMsg Polish     = "W podręczniku man dla aura znajduje się\xa0więcej informacji o opcjach."
manpageMsg Croatian   = "Za detalje o opcijama, pogledajte Aura man stranicu."
manpageMsg Swedish    = "Hänvisa till auras `man`-sida för detaljerade alternativ."
manpageMsg German     = "Lesen Sie die aura man-Seite für Details zu aura Optionen."
manpageMsg Spanish    = "Lee la página de manual de aura para detalles sobre las opciones."
manpageMsg Portuguese = "Leia a man page do aura para mais detalhes sobre as opções"
manpageMsg French     = "Voir le manuel d'Aura (`man aura`) pour le détail des options."
manpageMsg Russian    = "Чтобы узнать подробное описание опций aura, см. мануал."
manpageMsg Italian    = "Guardare la man page di Aura per maggiori dettagli sulle opzioni."
manpageMsg Serbian    = "За детаље о опцијама, погледајте man страницу Аура."
manpageMsg Norwegian  = "Referer til Aura's `man`-side for instillingsdetaljer."

displayOutputLanguages_1 :: Language -> String
displayOutputLanguages_1 English    = "The following languages are available:"
displayOutputLanguages_1 Japanese   = "auraは下記の言語に対応している："
displayOutputLanguages_1 Polish     = "Następujące języki są dostępne:"
displayOutputLanguages_1 Croatian   = "Dostupni su sljedeći jezici:"
displayOutputLanguages_1 Swedish    = "Följande språk är tillängliga:"
displayOutputLanguages_1 German     = "Die folgenden Sprachen sind verfügbar:"
displayOutputLanguages_1 Spanish    = "Los siguientes idiomas están disponibles:"
displayOutputLanguages_1 Portuguese = "Os seguintes idiomas estão disponíveis:"
displayOutputLanguages_1 French     = "Les langues suivantes sont disponibles:"
displayOutputLanguages_1 Russian    = "Доступны следующие языки:"
displayOutputLanguages_1 Italian    = "Sono disponibili le seguenti lingue:"
displayOutputLanguages_1 Serbian    = "Доступни су следећи језици:"
displayOutputLanguages_1 Norwegian  = "Følgende språk er tilgjengelig:"

----------------------------
-- Aura/Commands/A functions
----------------------------
auraCheck_1 :: Language -> String
auraCheck_1 Japanese  = "Auraアップグレードあり。先にAuraだけを？"
auraCheck_1 Croatian  = "Dostupna je nova verzija Aura. Želite li prvo ažurirati?"
auraCheck_1 German    = "Aura Update verfügbar. Dies zuerst aktualisieren?"
auraCheck_1 Norwegian = "En Aura-oppdatering er tilgjengelig. Oppdater den først?"
auraCheck_1 French    = "Une mise à jour d'Aura est disponible. Voulez-vous la mettre à jour en premier?"
auraCheck_1 _         = "Aura update available. Update it first?"

install_1 :: Language -> String
install_1 English    = "Dependency checking failed for these reasons:"
install_1 Japanese   = "従属パッケージの確認は以下の理由で失敗した："
install_1 Polish     = "Sprawdzanie zależności nie powiodło się z następujących powodów:"
install_1 Croatian   = "Provjera zavisnosti nije uspjela iz sljedećih razloga:"
install_1 Swedish    = "Beroende-kollen misslyckades pga följande skäl:"
install_1 German     = "Abhängigkeitsüberprüfung schlug Fehl aus folgenden Gründen:"
install_1 Spanish    = "La comprobación de dependencias falló por los siguientes motivos:"
install_1 Portuguese = "Não foi possível checar as dependências pelas seguintes razões:"
install_1 French     = "La vérification des dépendances a failli pour les raisons suivantes:"
install_1 Russian    = "Проверка зависимостей не удалась из-за:"
install_1 Italian    = "Il controllo delle dipendenze è fallito per i seguenti motivi:"
install_1 Serbian    = "Провера зависности није успела из следећих разлога:"
install_1 Norwegian  = "Avhengighets-sjekken mislyktes på grunn av følgende:"

install_2 :: Language -> String
install_2 English    = "No valid packages specified."
install_2 Japanese   = "適当なパッケージを入力してください。"
install_2 Polish     = "Nie podano prawidłowych pakietów."
install_2 Croatian   = "Nije specificiran nijedan ispravan paket."
install_2 Swedish    = "Inga giltiga paket valda."
install_2 German     = "Keine gültigen Pakete angegeben."
install_2 Spanish    = "No se ha especificado ningún paquete válido."
install_2 Portuguese = "Nenhum pacote válido foi especificado."
install_2 French     = "Aucun paquet valide n'a été spécifié."
install_2 Russian    = "Валидные пакеты не указаны."
install_2 Italian    = "Nessun pacchetto valido specificato."
install_2 Serbian    = "Ниједан исправан пакет није специфициран."
install_2 Norwegian  = "Ingen gyldige pakker er valgte."

install_3 :: Language -> String
install_3 English    = "Continue?"
install_3 Japanese   = "続行？"
install_3 Polish     = "Kontynuować?"
install_3 Croatian   = "Nastaviti?"
install_3 Swedish    = "Fortsätta?"
install_3 German     = "Fortsetzen?"
install_3 Spanish    = "¿Continuar?"
install_3 Portuguese = "Continuar?"
install_3 French     = "Continuer?"
install_3 Russian    = "Продолжить?"
install_3 Italian    = "Continuare?"
install_3 Serbian    = "Наставити?"
install_3 Norwegian  = "Fortsett?"

install_4 :: Language -> String
install_4 English    = "Installation manually aborted."
install_4 Japanese   = "続行は意図的に阻止された。"
install_4 Polish     = "Instalacja została przerwana przez użytkownika."
install_4 Croatian   = "Instalacija prekinuta od strane korisnika."
install_4 Swedish    = "Installationen avbröts manuellt."
install_4 German     = "Installation durch Benutzer abgebrochen."
install_4 Spanish    = "Instalación abortada manualmente."
install_4 Portuguese = "Instalação manual abortada."
install_4 French     = "Installation manuelle annulée."
install_4 Russian    = "Пользователь прервал установку."
install_4 Italian    = "Installazione manuale interrotta."
install_4 Serbian    = "Инсталација је ручно прекинута."
install_4 Norwegian  = "Installasjonen ble avbrutt manuelt."

install_5 :: Language -> String
install_5 English    = "Determining dependencies..."
install_5 Japanese   = "従属パッケージを確認中・・・"
install_5 Polish     = "Ustalanie zależności..."
install_5 Croatian   = "Određivanje zavisnosti..."
install_5 Swedish    = "Avgör beroenden..."
install_5 German     = "Bestimme Abhängigkeiten..."
install_5 Spanish    = "Determinando dependencias..."
install_5 Portuguese = "Determinando as dependências..."
install_5 French     = "Détermination des dépendances en cours..."
install_5 Russian    = "Определение зависимостей..."
install_5 Italian    = "Determinazione dipendenze..."
install_5 Serbian    = "Утврђивање зависности..."
install_5 Norwegian  = "Bestemmer avhengigheter..."

-- NEEDS UPDATE TO REFLECT CHANGED ENGLISH
reportNonPackages_1 :: Language -> String
reportNonPackages_1 English    = "The following are not AUR packages:"
reportNonPackages_1 Japanese   = "下記はAURパッケージではない："
reportNonPackages_1 Polish     = "To nie są pakiety:"
reportNonPackages_1 Croatian   = "Ovo nisu AUR paketi:"
reportNonPackages_1 Swedish    = "Följande är inte paket:"
reportNonPackages_1 German     = "Folgende sind keine Pakete:"
reportNonPackages_1 Spanish    = "Los siguientes no son paquetes:"
reportNonPackages_1 Portuguese = "Os seguintes não são pacotes:"
reportNonPackages_1 French     = "Les éléments suivants ne sont pas des paquets:"
reportNonPackages_1 Russian    = "Ниже указано то, что не является пакетами:"
reportNonPackages_1 Italian    = "I seguenti pacchetti non sono presenti in AUR:"
reportNonPackages_1 Serbian    = "Ово нису пакети:"
reportNonPackages_1 Norwegian  = "Det følgende er ikke AUR-pakker:"

reportIgnoredPackages_1 :: Language -> String
reportIgnoredPackages_1 English    = "The following packages will be ignored:"
reportIgnoredPackages_1 Japanese   = "下記のパッケージは無視される："
reportIgnoredPackages_1 Polish     = "Poniższe pakiety zostaną zignorowane:"
reportIgnoredPackages_1 Croatian   = "Sljedeći paketi će biti ignorirani:"
reportIgnoredPackages_1 Swedish    = "Följande paket kommer att ignoreras: "
reportIgnoredPackages_1 German     = "Die folgenden Pakete werden ignoriert:"
reportIgnoredPackages_1 Spanish    = "Los siguientes paquetes serán ignorados:"
reportIgnoredPackages_1 Portuguese = "Os seguintes pacotes serão ignorados:"
reportIgnoredPackages_1 French     = "Les paquets suivants seront ignorés:"
reportIgnoredPackages_1 Russian    = "Следующие пакеты будут проигнорированы:"
reportIgnoredPackages_1 Italian    = "I seguenti pacchetti verranno ignorati:"
reportIgnoredPackages_1 Serbian    = "Следећи пакети ће бити игнорисани:"
reportIgnoredPackages_1 Norwegian  = "De følgende pakker vil bli ignorert:"

reportPkgsToInstall_1 :: Language -> String
reportPkgsToInstall_1 English    = "Repository dependencies:"
reportPkgsToInstall_1 Japanese   = "Pacmanの従属パッケージ："
reportPkgsToInstall_1 Polish     = "Zależności z repozytoriów:"
reportPkgsToInstall_1 Croatian   = "Zavisnosti iz repozitorija:"
reportPkgsToInstall_1 Swedish    = "Beroenden ifrån lager:"
reportPkgsToInstall_1 German     = "Abhängigkeiten in den Paketquellen:"
reportPkgsToInstall_1 Spanish    = "Dependencias en el repositorio:"
reportPkgsToInstall_1 Portuguese = "Dependências no repositório:"
reportPkgsToInstall_1 French     = "Dépendances du dépôt:"
reportPkgsToInstall_1 Russian    = "Зависимости из репозитория:"
reportPkgsToInstall_1 Italian    = "Dipendenze nei repository:"
reportPkgsToInstall_1 Serbian    = "Зависности из ризница:"
reportPkgsToInstall_1 Norwegian  = "Avhengigheter fra depotet:"

-- NEEDS AN UPDATE
reportPkgsToInstall_2 :: String -> Language -> String
reportPkgsToInstall_2 l Japanese  = l ++ "のパッケージ:"
reportPkgsToInstall_2 l Croatian  = l ++ " Paketi:"
reportPkgsToInstall_2 l German    = l ++ " Pakete:"
reportPkgsToInstall_2 l Norwegian = l ++ " Pakker:"
reportPkgsToInstall_2 l Italian   = l ++ " Pacchetti:"
reportPkgsToInstall_2 l French    = l ++ " Paquets:"
reportPkgsToInstall_2 l _         = l ++ " Packages:"

{-}
reportPkgsToInstall_2 :: String -> Language -> String
reportPkgsToInstall_2 l English    = l ++ " dependencies:"
reportPkgsToInstall_2 l Japanese   = l ++ "の従属パッケージ："
reportPkgsToInstall_2 l Polish     = "Zależności z " ++ l ++ ":"
reportPkgsToInstall_2 l Croatian   = "Zavisnosti iz " ++ l ++ "-a:"
reportPkgsToInstall_2 l Swedish    = "Beroenden ifrån " ++ l ++ ":"
reportPkgsToInstall_2 l German     = "Abhängigkeiten im " ++ l ++ ":"
reportPkgsToInstall_2 l Spanish    = "Dependencias en " ++ l ++ ":"
reportPkgsToInstall_2 l Portuguese = "Dependências no " ++ l ++ ":"
reportPkgsToInstall_2 l French     = "Dépendances " ++ l ++ "\xa0:"
reportPkgsToInstall_2 l Russian    = "Зависимости из " ++ l ++ ":"
reportPkgsToInstall_2 l Italian    = "Dipendenze in " ++ l ++ ":"
reportPkgsToInstall_2 l Serbian    = "Зависности из " ++ l ++ "-а:"
reportPkgsToInstall_2 l Norwegian  = "Avhengigheter fra " ++ l ++ ":"

reportPkgsToInstall_3 :: String -> Language -> String
reportPkgsToInstall_3 l English    = "Main " ++ l ++ " packages:"
reportPkgsToInstall_3 l Japanese   = "主な" ++ l ++ "パッケージ："
reportPkgsToInstall_3 l Polish     = "Główne pakiety z " ++ l ++ ":"
reportPkgsToInstall_3 l Croatian   = "Glavni " ++ l ++ " paketi:"
reportPkgsToInstall_3 l Swedish    = "Huvudpaket ifrån " ++ l ++ ":"
reportPkgsToInstall_3 l German     = "Hauptpaket aus dem " ++ l ++ ":"
reportPkgsToInstall_3 l Spanish    = "Paquetes principales de " ++ l ++ ":"
reportPkgsToInstall_3 l Portuguese = "Pacotes principais do " ++ l ++ ":"
reportPkgsToInstall_3 l French     = "Principaux paquets " ++ l ++ "\xa0:"
reportPkgsToInstall_3 l Russian    = "Главные пакеты из " ++ l ++ ":"
reportPkgsToInstall_3 l Italian    = "Pacchetto principale di " ++ l ++ ":"
reportPkgsToInstall_3 l Serbian    = "Главни пакети из " ++ l ++ "-а:"
reportPkgsToInstall_3 l Norwegian  = "Hovedpakker fra " ++ l ++ ":"
-}

-- Needs translations.
reportPkgbuildDiffs_1 :: String -> Language -> String
reportPkgbuildDiffs_1 p Japanese  = bt p ++ "のPKGBUILDはまだ保存されていない。"
reportPkgbuildDiffs_1 p Polish    = bt p ++ " nie ma jeszcze przechowywanego pliku PKGBUILD."
reportPkgbuildDiffs_1 p Croatian  = bt p ++ " još nema pohranjen PKGBUILD."
reportPkgbuildDiffs_1 p German    = bt p ++ " hat noch keinen gespeicherten PKGBUILD."
reportPkgbuildDiffs_1 p Spanish   = bt p ++ " no tiene PKGBUILD todavía."
reportPkgbuildDiffs_1 p French    = bt p ++ " n'a pas encore de PKGBUILD enregistré."
reportPkgbuildDiffs_1 p Russian   = "У " ++ bt p ++ " ещё нет сохраненного PKGBUILD."
reportPkgbuildDiffs_1 p Italian   = bt p ++ " non ci sono PKGBUILD salvati"
reportPkgbuildDiffs_1 p Serbian   = bt p ++ " још нема похрањен PKGBUILD."
reportPkgbuildDiffs_1 p Norwegian = bt p ++ " har ingen PKGBUILD ennå."
reportPkgbuildDiffs_1 p _         = bt p ++ " has no stored PKGBUILD yet."

reportPkgbuildDiffs_2 :: String -> Language -> String
reportPkgbuildDiffs_2 p Japanese  = bt p ++ "のPKGBUILDは最新。"
reportPkgbuildDiffs_2 p Polish    = "PKGBUILD pakietu " ++ bt p ++ " jest aktualny."
reportPkgbuildDiffs_2 p Croatian  = "PKGBUILD paketa " ++ bt p ++ " je na najnovijoj verziji."
reportPkgbuildDiffs_2 p German    = "PKGBUILD von " ++ bt p ++ " ist aktuell."
reportPkgbuildDiffs_2 p Spanish   = "El PKGBUILD de " ++ bt p ++ " está actualizado."
reportPkgbuildDiffs_2 p Russian   = "PKGBUILD " ++ bt p ++ " является новейшим."
reportPkgbuildDiffs_2 p French    = "Le PKGBUILD de " ++ bt p ++ " est à jour."
reportPkgbuildDiffs_2 p Italian   = "Il PKGBUILD di " ++ bt p ++ " è aggiornato."
reportPkgbuildDiffs_2 p Serbian   = "PKGBUILD пакета " ++ bt p ++ " је ажуран."
reportPkgbuildDiffs_2 p Norwegian = bt p ++ "'s PKGBUILD er oppdatert."
reportPkgbuildDiffs_2 p _         = bt p ++ " PKGBUILD is up to date."

reportPkgbuildDiffs_3 :: String -> Language -> String
reportPkgbuildDiffs_3 p Japanese  = bt p ++ "のPKGBUILD変更報告："
reportPkgbuildDiffs_3 p Polish    = "Zmiany w PKGBUILD dla " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Croatian  = "Promjene u PKGBUILD-u za " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p German    = "PKGBUILD Änderungen von " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Spanish   = "Cambios en el PKGBUILD de " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Russian   = "Изменения, вносимые " ++ bt p ++ " PKGBUILD:"
reportPkgbuildDiffs_3 p French    = "Changements du PKGBUILD de " ++ bt p
reportPkgbuildDiffs_3 p Italian   = "Cambiamenti nel PKGBUILD di " ++ bt p ++":"
reportPkgbuildDiffs_3 p Serbian   = "Промене PKGBUILD-a за " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Norwegian = bt p ++ "'s endringer i PKGBUILD:"
reportPkgbuildDiffs_3 p _         = bt p ++ " PKGBUILD changes:"

reportPkgsToUpgrade_1 :: Language -> String
reportPkgsToUpgrade_1 English    = "AUR Packages to upgrade:"
reportPkgsToUpgrade_1 Japanese   = "アップグレードするAURパッケージ："
reportPkgsToUpgrade_1 Polish     = "Pakiety z AUR do zaktualizowania:"
reportPkgsToUpgrade_1 Croatian   = "AUR paketi za nadogradnju:"
reportPkgsToUpgrade_1 Swedish    = "AUR-paket att uppgradera:"
reportPkgsToUpgrade_1 German     = "Zu aktualisierendes AUR Paket:"
reportPkgsToUpgrade_1 Spanish    = "Paquetes de AUR a actualizar:"
reportPkgsToUpgrade_1 Portuguese = "Pacotes do AUR para atualizar:"
reportPkgsToUpgrade_1 French     = "Paquets AUR à mettre à jour:"
reportPkgsToUpgrade_1 Russian    = "Пакеты AUR, готовые для обновления:"
reportPkgsToUpgrade_1 Italian    = "Pacchetti in AUR da aggiornare:"
reportPkgsToUpgrade_1 Serbian    = "Пакети из AUR-а за надоградњу:"
reportPkgsToUpgrade_1 Norwegian  = "AUR-pakker å oppgradere:"

-- NEEDS UPDATING
reportBadDowngradePkgs_1 :: Language -> String
reportBadDowngradePkgs_1 English    = "The following have no versions in the cache, and thus can’t be downgraded:"
reportBadDowngradePkgs_1 Japanese   = "このパッケージはキャッシュには入っていないので、格下げはできない。"
reportBadDowngradePkgs_1 Polish     = "Poniższe pakeity nie są zainstalowane, i nie mogą być zainstalowane w starszej wersji:"
reportBadDowngradePkgs_1 Croatian   = "Sljedeći paketi nisu instalirani te se stoga ne mogu vratiti na stare verzije:"
reportBadDowngradePkgs_1 Swedish    = "Följande paket är inte installerade, och kan därför inte bli nergraderade:"
reportBadDowngradePkgs_1 German     = "Folgende Pakete sind nicht installiert und können daher nicht downgraded werden:"
reportBadDowngradePkgs_1 Spanish    = "Los siguientes paquetes no están instalados, por lo que no se pueden retornar a versiones antiguas:"
reportBadDowngradePkgs_1 Portuguese = "Os seguintes pacotes não estão instalados, logo não podem retornar a uma versão anterior:"
reportBadDowngradePkgs_1 French     = "Les paquets suivants ne sont pas installés; ils ne peuvent pas être rétrogradés:"
reportBadDowngradePkgs_1 Russian    = "Следующие пакеты не установлены, а следовательно, не могут быть откачены к старой версии:"
reportBadDowngradePkgs_1 Italian    = "I seguenti pacchetti non hanno versioni in cache e non posso essere retrocessi:"
reportBadDowngradePkgs_1 Serbian    = "Следећи пакети нису ни инсталирани, те се не могу вратити на старију верзију:"
reportBadDowngradePkgs_1 Norwegian  = "Følgende pakker har ingen versjoner i cache, og kan derfor ikke bli nedgradert:"

upgradeAURPkgs_1 :: Language -> String
upgradeAURPkgs_1 English    = "Fetching package information..."
upgradeAURPkgs_1 Japanese   = "パッケージ情報をダウンロード中・・・"
upgradeAURPkgs_1 Polish     = "Pobieranie informacji o pakietach..."
upgradeAURPkgs_1 Croatian   = "Preuzimanje podataka o paketima..."
upgradeAURPkgs_1 Swedish    = "Hämtar paketinformation..."
upgradeAURPkgs_1 German     = "Rufe Paketinformationen ab..."
upgradeAURPkgs_1 Spanish    = "Obteniendo información de paquetes..."
upgradeAURPkgs_1 Portuguese = "Obtendo informação dos pacotes..."
upgradeAURPkgs_1 French     = "Obtention des informations des paquets en cours..."
upgradeAURPkgs_1 Russian    = "Сборка информации о пакетах..."
upgradeAURPkgs_1 Italian    = "Ottengo le informazioni del pacchetto..."
upgradeAURPkgs_1 Serbian    = "Преузимање информација о пакетима..."
upgradeAURPkgs_1 Norwegian  = "Henter pakkeinformasjon..."

upgradeAURPkgs_2 :: Language -> String
upgradeAURPkgs_2 English    = "Comparing package versions..."
upgradeAURPkgs_2 Japanese   = "バージョンを比較中・・・"
upgradeAURPkgs_2 Polish     = "Porównywanie wersji pakietów..."
upgradeAURPkgs_2 Croatian   = "Uspoređivanje verzija paketa..."
upgradeAURPkgs_2 Swedish    = "Jämför paket-versioner..."
upgradeAURPkgs_2 German     = "Vergleiche Paketversionen..."
upgradeAURPkgs_2 Spanish    = "Comparando versiones de paquetes..."
upgradeAURPkgs_2 Portuguese = "Comparando versões dos pacotes..."
upgradeAURPkgs_2 French     = "Comparaison des versions des paquets en cours..."
upgradeAURPkgs_2 Russian    = "Сравнение версий пакетов..."
upgradeAURPkgs_2 Italian    = "Confronto le ersioni del pacchetto..."
upgradeAURPkgs_2 Serbian    = "Упоређивање верзија пакета..."
upgradeAURPkgs_2 Norwegian  = "Sammenligner pakkeversjoner..."

upgradeAURPkgs_3 :: Language -> String
upgradeAURPkgs_3 English    = "No AUR package upgrades necessary."
upgradeAURPkgs_3 Japanese   = "アップグレードは必要ない。"
upgradeAURPkgs_3 Polish     = "Nie jest wymagana aktualizacja pakietów z AUR."
upgradeAURPkgs_3 Croatian   = "Svi AUR paketi su ažurirani."
upgradeAURPkgs_3 Swedish    = "Inga AUR-paketsuppgraderingar behövs."
upgradeAURPkgs_3 German     = "Keine AUR Paketaktualisierungen notwendig."
upgradeAURPkgs_3 Spanish    = "No ha sido necesario actualizar paquetes de AUR."
upgradeAURPkgs_3 Portuguese = "Nenhum pacote do AUR precisa de atualização."
upgradeAURPkgs_3 French     = "Aucune mise à jour de paquet AUR n'est nécessaire."
upgradeAURPkgs_3 Russian    = "Обновление пакетов из AUR не требуется."
upgradeAURPkgs_3 Italian    = "Non è necessario aggiornare pacchetti di AUR."
upgradeAURPkgs_3 Serbian    = "Ажурирање пакета из AUR-а није потребно."
upgradeAURPkgs_3 Norwegian  = "Ingen pakkeoppgradering fra AUR nødvendig."

downloadTarballs_1 :: String -> Language -> String
downloadTarballs_1 p English    = "Downloading " ++ bt p ++ " source tarball..."
downloadTarballs_1 p Japanese   = bt p ++ "のソースコードのターボールをダウンロード中・・・"
downloadTarballs_1 p Polish     = "Pobieranie paczki źródłowej " ++ bt p ++ "..."
downloadTarballs_1 p Croatian   = "Preuzimanje izvornog paketa (tarball) " ++ bt p ++ "..."
downloadTarballs_1 p Swedish    = "Laddar ner " ++ bt p ++ " källkodspaket (tarball)..."
downloadTarballs_1 p German     = "Lade Quelltext von " ++ bt p ++ " (tarball)..."
downloadTarballs_1 p Spanish    = "Descargando los fuentes comprimidos (tarball) de " ++ bt p ++ " ..."
downloadTarballs_1 p Portuguese = "Baixando os fontes (tarball) de " ++ bt p ++ " ..."
downloadTarballs_1 p French     = "Téléchargement de l'archive de " ++ bt p ++ " en cours..."
downloadTarballs_1 p Russian    = "Загрузка исходного архива " ++ bt p ++ "..."
downloadTarballs_1 p Italian    = "Downlaod del tarball di " ++ bt p ++ " in corso..."
downloadTarballs_1 p Serbian    = "Преузимање архиве изворног кода за " ++ bt p ++ "..."
downloadTarballs_1 p Norwegian  = "Laster ned " ++ bt p ++ " kildekodepakken (tarball)..."

displayPkgbuild_1 :: String -> Language -> String
displayPkgbuild_1 p English    = bt p ++ " does not exist."
displayPkgbuild_1 p Japanese   = bt p ++ "は存在しない。"
displayPkgbuild_1 p Polish     = bt p ++ " nie istnieje."
displayPkgbuild_1 p Croatian   = bt p ++ " ne postoji."
displayPkgbuild_1 p Swedish    = bt p ++ " finns inte."
displayPkgbuild_1 p German     = bt p ++ " existiert nicht."
displayPkgbuild_1 p Spanish    = bt p ++ " no existe."
displayPkgbuild_1 p Portuguese = bt p ++ " não existe."
displayPkgbuild_1 p French     = bt p ++ " n'existe pas."
displayPkgbuild_1 p Russian    = bt p ++ " не существует."
displayPkgbuild_1 p Italian    = bt p ++ " inesistente."
displayPkgbuild_1 p Serbian    = bt p ++ " не постоји."
displayPkgbuild_1 p Norwegian  = bt p ++ " finnes ikke."

removeMakeDepsAfter_1 :: Language -> String
removeMakeDepsAfter_1 English    = "Removing unneeded make dependencies..."
removeMakeDepsAfter_1 Japanese   = "あと片付け。必要ないパッケージを削除："
removeMakeDepsAfter_1 Polish     = "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."
removeMakeDepsAfter_1 Croatian   = "Uklanjanje nepotrebnih zavisnosti vezanih uz izgradnju..."
removeMakeDepsAfter_1 Swedish    = "Tar bort obehövda beroenden för `make`..."
removeMakeDepsAfter_1 German     = "Entferne nicht mehr benötigte make Abhängigkeiten..."
removeMakeDepsAfter_1 Spanish    = "Removiendo dependencias make innecesarias..."
removeMakeDepsAfter_1 Portuguese = "Removendo dependências `make` desnecessárias..."
removeMakeDepsAfter_1 French     = "Suppression des dépendances inutiles..."
removeMakeDepsAfter_1 Russian    = "Удаление ненужных зависимостей make..."
removeMakeDepsAfter_1 Italian    = "Rimuovo le dipendenze di compilazione..."
removeMakeDepsAfter_1 Serbian    = "Уклањање непотребних зависности за изградњу..."
removeMakeDepsAfter_1 Norwegian  = "Fjerner unødvendige make-avhengigheter..."

----------------------------
-- Aura/Commands/B functions
----------------------------
-- NEEDS TRANSLATION
cleanStates_1 :: Language -> String
cleanStates_1 Japanese  = "入力は数字ではない。"
cleanStates_1 Croatian  = "Unos ne predstavlja broj."
cleanStates_1 German    = "Eingabe ist keine gültige Zahl."
cleanStates_1 Serbian   = "Улаз није валидан број."
cleanStates_1 Norwegian = "Oppføringen er ikke et gyldig nummer."
cleanStates_1 Italian   = "Non è un numero valido."
cleanStates_1 French    = "La valeur entrée n'est pas un nombre."
cleanStates_1 _         = "Input isn't a valid number."

-- NEEDS TRANSLATION
cleanStates_2 :: Int -> Language -> String
cleanStates_2 n Japanese  = bt (show n) ++ "個のパッケージ状態記録だけが残される。その他削除？"
cleanStates_2 n Croatian  = bt (show n) ++ " stanja paketa će biti zadržano. Ukloniti ostatak?"
cleanStates_2 n German    = bt (show n) ++ " Paketzustände werden behalten. Den Rest entfernen?"
cleanStates_2 n Serbian   = bt (show n) ++ " стања пакета ће бити сачувано. Уклонити остатак?"
cleanStates_2 n Norwegian = bt (show n) ++ " pakketilstander vil bli beholdt. Vil du fjerne resten?"
cleanStates_2 n Italian   = bt (show n) ++ " lo stato dei pacchetti sarà mantenuto. Rimuovere i rimanenti?"
cleanStates_2 n French    = bt (show n) ++ " états des paquets vont être gardés. Supprimer le reste?"
cleanStates_2 n _         = bt (show n) ++ " package states will be kept. Remove the rest?"

-- NEEDS TRANSLATION
cleanStates_3 :: Language -> String
cleanStates_3 Japanese  = "何も削除しないで終了。"
cleanStates_3 Croatian  = "Nijedno stanje paketa nije uklonjeno."
cleanStates_3 German    = "Keine Paketzustände wurden entfernt."
cleanStates_3 Serbian   = "Ниједно стање пакета није уклоњено."
cleanStates_3 Norwegian = "Ingen pakketilstander ble fjernet."
cleanStates_3 Italian   = "Nessuno stato di pacchetto verrà rimosso."
cleanStates_3 French    = "Aucun état des paquets n'a été supprimé."
cleanStates_3 _         = "No package states were removed."

----------------------------
-- Aura/Commands/C functions
----------------------------
getDowngradeChoice_1 :: String -> Language -> String
getDowngradeChoice_1 p English    = "What version of " ++ bt p ++ " do you want?"
getDowngradeChoice_1 p Japanese   = bt p ++ "はどのバージョンにする？"
getDowngradeChoice_1 p Polish     = "Którą wersję pakietu " ++ bt p ++ " zainstalować?"
getDowngradeChoice_1 p Croatian   = "Koju verziju paketa " ++ bt p ++ " želite?"
getDowngradeChoice_1 p Swedish    = "Vilken version av " ++ bt p ++ " vill du ha?"
getDowngradeChoice_1 p German     = "Welche Version von " ++ bt p ++ " möchten Sie haben?"
getDowngradeChoice_1 p Spanish    = "¿Qué versión de " ++ bt p ++ " quieres?"
getDowngradeChoice_1 p Portuguese = "Qual versão de " ++ bt p ++ " deseja?"
getDowngradeChoice_1 p French     = "Quelle version de " ++ bt p ++ " voulez-vous?"
getDowngradeChoice_1 p Russian    = "Какую версию " ++ bt p ++ " вы хотите?"
getDowngradeChoice_1 p Italian    = "Quale versione di " ++ bt p ++ " preferisci?"
getDowngradeChoice_1 p Serbian    = "Коју верзију " ++ bt p ++ "-а желите?"
getDowngradeChoice_1 p Norwegian  = "Hvilken versjon av " ++ bt p ++ " vil du ha?"

backupCache_1 :: Language -> String
backupCache_1 English    = "No backup location given."
backupCache_1 Japanese   = "バックアップ先を入力してください。"
backupCache_1 Polish     = "Nie podano lokalizacji kopii zapasowych."
backupCache_1 Croatian   = "Lokacija sigurnosne kopije nije specifirana."
backupCache_1 Swedish    = "Ingen backup-plats specifierad."
backupCache_1 German     = "Kein Sicherungsort angegeben."
backupCache_1 Spanish    = "No se ha especificado localización para la copia de seguridad."
backupCache_1 Portuguese = "Ainda não disse onde quer guardar o backup..."
backupCache_1 French     = "Aucun lieu pour les copies de sauvegarde n'est spécifié."
backupCache_1 Russian    = "Не указан путь к бэкапу."
backupCache_1 Italian    = "Path per il salvataggio non specificato."
backupCache_1 Serbian    = "Није дата путања ка бекапу."
backupCache_1 Norwegian  = "Ingen backup-plass spesifisert."

backupCache_2 :: Language -> String
backupCache_2 English    = "You must be root to backup the cache."
backupCache_2 Japanese   = "rootじゃないとバックアップはできない。"
backupCache_2 Polish     = "Musisz być rootem, by zrobić kopię\xa0zapasową pamięci podręcznej."
backupCache_2 Croatian   = "Za stvaranje sigurnosne kopije cache-a potrebne su root ovlasti."
backupCache_2 Swedish    = "Du måste vara root för att ta backup på cache-filer."
backupCache_2 German     = "Sie müssen root sein um den Cache zu sichern."
backupCache_2 Spanish    = "Debes ser root para hacer una copia de seguridad de la caché."
backupCache_2 Portuguese = "Precisa ser root para fazer um backup do cache."
backupCache_2 French     = "Vous devez être `root` pour faire une copie de sauvegarde du cache."
backupCache_2 Russian    = "Чтобы создать бэкап кэша, вы должны быть рутом"
backupCache_2 Italian    = "Devi essere root per salvare la cache."
backupCache_2 Serbian    = "Морате бити root да бисте бекаповали кеш."
backupCache_2 Norwegian  = "Du må være root for å ta backup på cache."

backupCache_3 :: Language -> String
backupCache_3 English    = "The backup location does not exist."
backupCache_3 Japanese   = "バックアップ先は存在しない。"
backupCache_3 Polish     = "Lokalizacja kopii zapasowych nie istnieje."
backupCache_3 Croatian   = "Lokacija sigurnosne kopije ne postoji."
backupCache_3 Swedish    = "Specifierad backup-plats finns inte."
backupCache_3 German     = "Der Sicherungsort existiert nicht."
backupCache_3 Spanish    = "La localización para copia de seguridad no existe."
backupCache_3 Portuguese = "O caminho indicado para o backup não existe."
backupCache_3 French     = "Le lieu des copies de sauvegarde spécifié n'existe pas."
backupCache_3 Russian    = "Путь к бэкапу не существует."
backupCache_3 Italian    = "L'indirizzo del salvataggio non esiste."
backupCache_3 Serbian    = "Путања ка бекапу не постоји."
backupCache_3 Norwegian  = "Spesifisert backup-plass finnes ikke."

backupCache_4 :: FilePath -> Language -> String
backupCache_4 dir English    = "Backing up cache to " ++ bt dir
backupCache_4 dir Japanese   = "キャッシュのバックアップ先：" ++ bt dir
backupCache_4 dir Polish     = "Tworzenie kopii zapasowej pamięci podręcznej w " ++ bt dir
backupCache_4 dir Croatian   = "Stvaram sigurnosnu kopiju u " ++ bt dir
backupCache_4 dir Swedish    = "Tar backup på cache-filer till " ++ bt dir
backupCache_4 dir German     = "Sichere Cache in " ++ bt dir
backupCache_4 dir Spanish    = "Haciendo una copia de seguridad de la caché en " ++ bt dir
backupCache_4 dir Portuguese = "Backup do cache sendo feito em " ++ bt dir
backupCache_4 dir French     = "Copie de sauvegarde dans " ++ bt dir ++ "."
backupCache_4 dir Russian    = "Бэкап создается в директории " ++ bt dir
backupCache_4 dir Italian    = "Salvataggio della chace in " ++ bt dir
backupCache_4 dir Serbian    = "Бекапујем кеш у " ++ bt dir
backupCache_4 dir Norwegian  = "Tar backup på cache til " ++ bt dir

backupCache_5 :: Int -> Language -> String
backupCache_5 n English    = "Package files to backup: " ++ bt (show n)
backupCache_5 n Japanese   = "パッケージのファイル数：" ++ bt (show n)
backupCache_5 n Polish     = "Pliki będące częścią\xa0kopii zapasowej: " ++ bt (show n)
backupCache_5 n Croatian   = "Datoteke koje su dio sigurnosne kopije: " ++ bt (show n)
backupCache_5 n Swedish    = "Paket-filer att ta backup på: " ++ bt (show n)
backupCache_5 n German     = "Zu sichernde Paketdateien: " ++ bt (show n)
backupCache_5 n Spanish    = "Ficheros de paquetes de los que se hará copia de seguridad: " ++ bt (show n)
backupCache_5 n Portuguese = "Arquivos de pacotes para backup: " ++ bt (show n)
backupCache_5 n French     = "Copie de sauvegarde des fichiers de paquets suivants: " ++ bt (show n)
backupCache_5 n Russian    = "Упакуйте файлы для бэкапа: " ++ bt (show n)
backupCache_5 n Italian    = "File del pacchetto da salvare: " ++ bt (show n)
backupCache_5 n Serbian    = "Датотеке за бекап: " ++ bt (show n)
backupCache_5 n Norwegian  = "Pakker som blir tatt backup på: " ++ bt (show n)

backupCache_6 :: Language -> String
backupCache_6 English    = "Proceed with backup?"
backupCache_6 Japanese   = "バックアップを実行する？"
backupCache_6 Polish     = "Kontynuować tworzenie kopii zapasowej?"
backupCache_6 Croatian   = "Nastavi sa stvaranjem sigurnosne kopije?"
backupCache_6 Swedish    = "Fortsätt med backup?"
backupCache_6 German     = "Sicherung fortsetzen?"
backupCache_6 Spanish    = "¿Proceder con la copia de seguridad?"
backupCache_6 Portuguese = "Proceder com o backup?"
backupCache_6 French     = "Procéder à la copie de sauvegarde?"
backupCache_6 Russian    = "Продолжить создание бэкапа?"
backupCache_6 Italian    = "Procedere con il salvataggio?"
backupCache_6 Serbian    = "Наставити бекаповање?"
backupCache_6 Norwegian  = "Fortsett med backup?"

backupCache_7 :: Language -> String
backupCache_7 English    = "Backup manually aborted."
backupCache_7 Japanese   = "バックアップは意図的に阻止された。"
backupCache_7 Polish     = "Tworzenie kopii zapasowej zostało przerwane przez użytkownika."
backupCache_7 Croatian   = "Stvaranje sigurnosne kopije prekinuto od strane korisnika."
backupCache_7 Swedish    = "Backup avbröts manuellt."
backupCache_7 German     = "Backup durch Benutzer abgebrochen."
backupCache_7 Spanish    = "Copia de seguridad abortada manualmente."
backupCache_7 Portuguese = "Backup manualmente abortado."
backupCache_7 French     = "Copie de sauvegarde manuelle annulée."
backupCache_7 Russian    = "Создание бэкапа прервано пользователем."
backupCache_7 Italian    = "Salvataggio manuale interrotto."
backupCache_7 Serbian    = "Бекаповање је ручно прекинуто."
backupCache_7 Norwegian  = "Backup ble avbrutt manuelt."

backupCache_8 :: Language -> String
backupCache_8 English    = "Backing up. This may take a few minutes..."
backupCache_8 Japanese   = "バックアップ中。数分かかるかもしれない。"
backupCache_8 Polish     = "Tworzenie kopii zapasowej. To może potrwać kilka minut..."
backupCache_8 Croatian   = "Stvaranje sigurnosne kopije. Ovo može potrajati nekoliko minuta..."
backupCache_8 Swedish    = "Tar backup. Det här kan ta ett tag..."
backupCache_8 German     = "Sichere. Dies kann ein paar Minuten dauern..."
backupCache_8 Spanish    = "Haciendo copia de seguridad. Esto puede tardar unos minutos..."
backupCache_8 Portuguese = "Efetuando backup. Isso pode levar alguns minutos..."
backupCache_8 French     = "Copie de sauvegarde en cours. Ceci peut prendre quelques minutes..."
backupCache_8 Russian    = "Создается бэкап. Это может занять пару минут..."
backupCache_8 Italian    = "Salvataggio. Questo potrebbe richiedere qualche minuto..."
backupCache_8 Serbian    = "Бекапујем. Ово може да потраје пар минута..."
backupCache_8 Norwegian  = "Tar backup. Dette kan ta en stund..."

copyAndNotify_1 :: Int -> Language -> String
copyAndNotify_1 n English    = "Copying #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Japanese   = "#[" ++ cyan (show n) ++"]をコピー中・・・"
copyAndNotify_1 n Polish     = "Kopiowanie #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Croatian   = "Kopiranje #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Swedish    = "Kopierar #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n German     = "Kopiere #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Spanish    = "Copiando #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Portuguese = "Copiando #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n French     = "Copie de #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Russian    = "Копируется #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Italian    = "Copiando #[" ++cyan (show n) ++ "]"
copyAndNotify_1 n Serbian    = "Копирам #[" ++ cyan (show n) ++ "]"
copyAndNotify_1 n Norwegian  = "Kopierer #[" ++ cyan (show n) ++ "]"

preCleanCache_1 :: String -> Language -> String
preCleanCache_1 n English    = bt n ++ " is not a number."
preCleanCache_1 n Japanese   = bt n ++ "は数字はない。"
preCleanCache_1 n Polish     = bt n ++ " nie jest liczbą."
preCleanCache_1 n Croatian   = bt n ++ " nije broj. "
preCleanCache_1 n Swedish    = bt n ++ " är inte ett nummer."
preCleanCache_1 n German     = bt n ++ " ist keine Nummer."
preCleanCache_1 n Spanish    = bt n ++ " no es un número."
preCleanCache_1 n Portuguese = bt n ++ " não é um número."
preCleanCache_1 n French     = bt n ++ " n'est pas un nombre."
preCleanCache_1 n Russian    = bt n ++ " не является числом."
preCleanCache_1 n Italian    = bt n ++ " non è un numero."
preCleanCache_1 n Serbian    = bt n ++ " није број."
preCleanCache_1 n Norwegian  = bt n ++ " er ikke et nummer."

cleanCache_1 :: Language -> String
cleanCache_1 English    = "Invalid number given."
cleanCache_1 Japanese   = "入力の数字は適切ではない。"
cleanCache_1 Polish     = "Nieprawidłowa liczba."
cleanCache_1 Croatian   = "Broj nije ispravan."
cleanCache_1 Swedish    = "Ogiltigt nummer specifierat."
cleanCache_1 German     = "Ungültige Nummer gegeben."
cleanCache_1 Spanish    = "Número inválido."
cleanCache_1 Portuguese = "Número inválido."
cleanCache_1 French     = "Nombre invalide."
cleanCache_1 Russian    = "Дано невалидное число."
cleanCache_1 Italian    = "Numero non valido."
cleanCache_1 Serbian    = "Број није валидан."
cleanCache_1 Norwegian  = "Ugyldig number spesifisert."

cleanCache_2 :: Language -> String
cleanCache_2 English    = "This will delete the ENTIRE package cache."
cleanCache_2 Japanese   = "パッケージ・キャッシュは完全に削除される。"
cleanCache_2 Polish     = "To usunie WSZYSTKIE pakiety z pamięci podręcznej."
cleanCache_2 Croatian   = "Ovo će izbrisati CIJELI cache paketa."
cleanCache_2 Swedish    = "Detta kommer ta bort HELA paket-cachen."
cleanCache_2 German     = "Dies wird den GESAMTEN Paketcache leeren."
cleanCache_2 Spanish    = "Esto eliminará POR COMPLETO la caché de paquetes."
cleanCache_2 Portuguese = "Isso eliminara TODOS OS PACOTES do cache."
cleanCache_2 French     = "Ceci va COMPLÈTEMENT supprimer le cache des paquets."
cleanCache_2 Russian    = "Это действие ВСЕЦЕЛО уничтожит кэш пакетов."
cleanCache_2 Italian    = "Questo cancellera l'INTERA cache dei pacchetti."
cleanCache_2 Serbian    = "Ово ће избрисати ЦЕО кеш пакета."
cleanCache_2 Norwegian  = "Dette vil slette HELE pakke-cachen."

cleanCache_3 :: Int -> Language -> String
cleanCache_3 n English    = bt (show n) ++ " of each package file will be kept."
cleanCache_3 n Japanese   = "パッケージ・ファイルは" ++ bt (show n) ++ "個保存される。"
cleanCache_3 n Polish     = bt (show n) ++ " wersji każdego pakietu zostanie zachowane."
cleanCache_3 n Croatian   = bt (show n) ++ " zadnjih verzija svakog paketa će biti zadržano."
cleanCache_3 n Swedish    = bt (show n) ++ " av varje paketfil kommer att sparas."
cleanCache_3 n German     = bt (show n) ++ " jeder Paketdatei wird behalten."
cleanCache_3 n Spanish    = bt (show n) ++ " ficheros de cada paquete se mantendrán."
cleanCache_3 n Portuguese = bt (show n) ++ " arquivos de cada pacote serão mantidos."
cleanCache_3 n French     = bt (show n) ++ " fichiers de chaque paquet sera conservé."
cleanCache_3 n Russian    = bt (show n) ++ " версии каждого пакета будут нетронуты."
cleanCache_3 n Italian    = bt (show n) ++ " di ciascun pacchetto sarà mantenuto."
cleanCache_3 n Serbian    = bt (show n) ++ " верзије сваког од пакета ће бити сачуване."
cleanCache_3 n Norwegian  = bt (show n) ++ " av hver pakkefil blir beholdt."

cleanCache_4 :: Language -> String
cleanCache_4 English    = "The rest will be deleted. Okay?"
cleanCache_4 Japanese   = "残りは全部削除される。承知する？"
cleanCache_4 Polish     = "Wszystko inne zostanie usunięte. Na pewno?"
cleanCache_4 Croatian   = "Ostali paketi će biti izbrisani. Jeste li sigurni?"
cleanCache_4 Swedish    = "Resten kommer att tas bort. Är det OK?"
cleanCache_4 German     = "Der Rest wird gelöscht. Ist das OK?"
cleanCache_4 Spanish    = "El resto se eliminará. ¿OK?"
cleanCache_4 Portuguese = "O resto será deletado. OK?"
cleanCache_4 French     = "Le reste sera supprimé. Êtes-vous d'accord?"
cleanCache_4 Russian    = "Всё остальное будет удалено. Годится?"
cleanCache_4 Italian    = "Il resto verrà mantenuto. Continuare?"
cleanCache_4 Serbian    = "Остатак ће бити избрисан. Да ли је то у реду?"
cleanCache_4 Norwegian  = "Resten vil bli slettet. Er det OK?"

cleanCache_5 :: Language -> String
cleanCache_5 English    = "Cache cleaning manually aborted."
cleanCache_5 Japanese   = "削除の続行は意図的に阻止された。"
cleanCache_5 Polish     = "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."
cleanCache_5 Croatian   = "Čišćenje cache-a paketa prekinuto od strane korisnika."
cleanCache_5 Swedish    = "Cache-rensning avbröts manuellt."
cleanCache_5 German     = "Säubern des Caches durch Benutzer abgebrochen."
cleanCache_5 Spanish    = "Limpieza de la caché abortada manualmente."
cleanCache_5 Portuguese = "Limpeza do cache abortada manualmente."
cleanCache_5 French     = "Le nettoyage du cache a été arrêté manuellement."
cleanCache_5 Russian    = "Очистка кэша прервана пользователем."
cleanCache_5 Italian    = "Pulitura manuale della cache interrotta."
cleanCache_5 Serbian    = "Чишћење кеша је ручно прекинуто."
cleanCache_5 Norwegian  = "Cache-rensing ble avbrutt manuelt."

cleanCache_6 :: Language -> String
cleanCache_6 English    = "Cleaning package cache..."
cleanCache_6 Japanese   = "パッケージ・キャッシュを掃除中・・・"
cleanCache_6 Polish     = "Czyszczenie pamięci podręcznej..."
cleanCache_6 Croatian   = "Čišćenje cache-a paketa..."
cleanCache_6 Swedish    = "Rensar paket-cache..."
cleanCache_6 German     = "Säubere Paketcache..."
cleanCache_6 Spanish    = "Limpiando la caché de paquetes..."
cleanCache_6 Portuguese = "Limpando cache de pacotes..."
cleanCache_6 French     = "Nettoyage du cache des paquets..."
cleanCache_6 Russian    = "Очистка кэша пакета..."
cleanCache_6 Italian    = "Ripulisco la cache..."
cleanCache_6 Serbian    = "Чишћење кеша..."
cleanCache_6 Norwegian  = "Renser pakke-cache..."

-- NEEDS TRANSLATION
cleanNotSaved_1 :: Language -> String
cleanNotSaved_1 Japanese  = "不要パッケージファイルを確認・・・"
cleanNotSaved_1 Croatian  = "Pronalazim nepotrebne datoteke paketa..."
cleanNotSaved_1 German    = "Bestimme nicht benötigte Paketdateien..."
cleanNotSaved_1 Norwegian = "Finner unødige pakkefiler..."
cleanNotSaved_1 Italian   = "Determino i pacchetti non più necessari..."
cleanNotSaved_1 French    = "Détermination des fichiers de paquet non-nécessaires..."
cleanNotSaved_1 _         = "Determining unneeded package files..."

cleanNotSaved_2 :: Int -> Language -> String
cleanNotSaved_2 s Japanese  = "「" ++ cyan (show s) ++ "」の不要パッケージファイルあり。削除？"
cleanNotSaved_2 s Croatian  = cyan (show s) ++ " nepotrebnih datoteka pronađeno. Obrisati?"
cleanNotSaved_2 s German    = cyan (show s) ++ " nicht benötigte Paketdateien gefunden. Löschen?"
cleanNotSaved_2 s Norwegian = cyan (show s) ++ " unødige pakkefiler funnet. Vil du slette?"
cleanNotSaved_2 s Italian   = cyan (show s) ++ " pacchetti non necessari trovati. Cancellarli?"
cleanNotSaved_2 s French    = cyan (show s) ++ " paquets non-nécessaires trouvés? Supprimer?"
cleanNotSaved_2 s _         = cyan (show s) ++ " unneeded package files found. Delete?"

----------------------------
-- Aura/Commands/L functions
----------------------------
logLookUpFields :: Language -> [String]
logLookUpFields English    = [ "Package","First Install","Upgrades","Recent Actions" ]
logLookUpFields Japanese   = [ "パッケージ","初インストール","アップグレード回数","近況" ]
logLookUpFields Polish     = [ "Pakiet","Pierwsza instalacja","Aktualizacje","Ostatnie akcje" ]
logLookUpFields Croatian   = [ "Paket","Prva instalacija","Nadogradnje","Nedavne radnje" ]
logLookUpFields Swedish    = [ "Paket","Första installation","Uppgraderingar","Nyliga händelser" ]
logLookUpFields German     = [ "Paket","Erste Installation","Aktualisierungen","Letzte Aktionen" ]
logLookUpFields Spanish    = [ "Paquete","Primera instalación","Actualizaciones","Acciones Recientes" ]
logLookUpFields Portuguese = [ "Pacote","Primeira instalação","Atualizações","Ações Recentes" ]
logLookUpFields French     = [ "Paquet","Première installation","Mises à jours","Actions récentes" ]
logLookUpFields Russian    = [ "Пакет","Первая установка","Обновления","Недавние действия" ]
logLookUpFields Italian    = [ "Package","Prima installazione","Upgrades","Azioni recenti" ]
logLookUpFields Serbian    = [ "Пакет","Прва инсталација","Ажурирања","Недавне радње" ]
logLookUpFields Norwegian  = [ "Pakke","Første installasjon","Oppgraderinger","Nylige hendelser" ]

reportNotInLog_1 :: Language -> String
reportNotInLog_1 English    = "These have not appeared in the log file:"
reportNotInLog_1 Japanese   = "logファイルには出ていない："
reportNotInLog_1 Polish     = "Tych pakietów nie ma w dzienniku:"
reportNotInLog_1 Croatian   = "Ovih paketa nema u dnevniku:"
reportNotInLog_1 Swedish    = "Dessa har inte framkommit i loggfiler:"
reportNotInLog_1 German     = "Diese sind nicht in der Logdatei aufgetaucht:"
reportNotInLog_1 Spanish    = "Estos no aparecen en el fichero log:"
reportNotInLog_1 Portuguese = "Os seguintes não apareceram no log de arquivo:"
reportNotInLog_1 French     = "Ceci n'apparaît pas des les journaux (log):"
reportNotInLog_1 Russian    = "Следующих пакетов нет в лог-файле:"
reportNotInLog_1 Italian    = "Questo non apparirà nei file di log;"
reportNotInLog_1 Serbian    = "Ови пакети се не спомињу у дневнику:"
reportNotInLog_1 Norwegian  = "Følgende har ikke vist seg i loggen:"

----------------------------
-- Aura/Commands/M functions
----------------------------
-- NEEDS TRANSLATION
cleanABSTree_1 :: Language -> String
cleanABSTree_1 Japanese  = "ABS Treeの中身を削除？"
cleanABSTree_1 Croatian  = "Obrisati cijelo ABS stablo?"
cleanABSTree_1 German    = "Den gesamten ABS Baum löschen?"
cleanABSTree_1 Norwegian = "Slett hele ABS-treet?"
cleanABSTree_1 Italian   = "Cancellare l'intero albero ABS?"
cleanABSTree_1 French    = "Supprimer l'entier de l'arbre ABS?"
cleanABSTree_1 _         = "Delete the entire ABS Tree?"

cleanABSTree_2 :: Language -> String
cleanABSTree_2 Japanese  = "ABS Treeの中身を削除中・・・"
cleanABSTree_2 Croatian  = "Brišem ABS stablo..."
cleanABSTree_2 German    = "Lösche ABS Baum..."
cleanABSTree_2 Norwegian = "Renser ABS-treet..."
cleanABSTree_2 Italian   = "Ripulisco l'abero ABS..."
cleanABSTree_2 French    = "Suppression de l'arbre ABS..."
cleanABSTree_2 _         = "Clearing out ABS Tree..."

----------------------
-- Aura/Flags functions
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
inheritedOperTitle Norwegian   = "Arvede `pacman`-operasjoner"

auraOperTitle :: Language -> String
auraOperTitle English    = "Aura Only Operations:"
auraOperTitle Japanese   = "Auraだけの選択肢："
auraOperTitle Polish     = "Operacje Aury:"
auraOperTitle Croatian   = "Aura operacije:"
auraOperTitle Swedish    = "Aura-specifika operationer:"
auraOperTitle German     = "Aura-spezifische Operationen:"
auraOperTitle Spanish    = "Operaciones Exclusivas de Aura:"
auraOperTitle Portuguese = "Operações exclusivas do Aura:"
auraOperTitle French     = "Opérations propres à Aura:"
auraOperTitle Russian    = "Специфичные для aura действия:"
auraOperTitle Italian    = "Operazioni esclusive di Aura:"
auraOperTitle Serbian    = "Аура-специфичне операције:"
auraOperTitle Norwegian  = "Aura-spesifikke operasjoner:"

aurSy :: Language -> String
aurSy English    = green "Perform actions involving the [A]UR.\n" ++ "Default action installs from the AUR."
aurSy Japanese   = green "[A]URに関連する処理\n" ++ "デフォルトでAURからインストール"
aurSy Polish     = green "Wykonuje akcje związane z [A]UR.\n" ++ "Domyślnie instaluje pakiety z AUR."
aurSy Croatian   = green "Izvršava radnje s [A]UR-om.\n" ++ "Uobičajena (default) radnja je instaliranje paketa iz AUR-a."
aurSy Swedish    = green "Utför åtgärder involverandes [A]UR.\n" ++ "Standard-åtgärd installerar ifrån AUR."
aurSy German     = green "Führe Aktionen aus die das [A]UR betreffen.\n" ++ "Standardaktion installiert aus dem AUR."
aurSy Spanish    = green "Realizar acciones relacionadas con el [A]UR.\n" ++ "La acción por defecto es instalar desde AUR."
aurSy Portuguese = green "Realizar ações envolvendo o [A]UR.\n" ++ "Ação padrão instala do AUR."
aurSy French     = green "Actions impliquant [A]UR.\n" ++ "Par défaut, installe depuis AUR."
aurSy Russian    = green "Совершить действия с участием [A]UR.\n" ++ "Действие по умолчанию устанавливает из AUR."
aurSy Italian    = green "Azioni riguardanti [A]UR.\n" ++ "Di default installa da AUR."
aurSy Serbian    = green "Извршава радње везане за [A]UR.\n" ++ "Уобичајена радња инсталира из AUR-а."
aurSy Norwegian  = green "Utfør handlinger som innebærer [A]UR.\n" ++ "Standard-handling installerer fra AUR."

absSy :: Language -> String
absSy Croatian  = magenta "Izvršava operacije sa ABS stablom.\n" ++ "Uobičajena (default) radnja je ručna izgradnja iz ABS stabla ([M]anual)."
absSy German    = magenta "Führe Aktionen aus die den ABS Baum betreffen.\n" ++ "Standardaktion baut [M]anuell aus ABS."
absSy Norwegian = magenta "Utfør handlinger som involverer ABS-treet.\n" ++ "Standard-handling bygger [M]anuelt fra ABS."
absSy French    = magenta "Effectue une action impliquant l'arbre ABS.\n" ++ "Par défaut, installe [M]anuellement depuis ABS."
absSy _         = magenta "Perform actions involving the ABS tree.\n" ++ "Default action [M]anually builds from ABS."

-- NEEDS TRANSLATION
saveS :: Language -> String
saveS Japanese  = yellow "パッケージの設置状態に関する処理\n" ++ "デフォルトでインストール状態を保存する。"
saveS Croatian  = yellow "Upravlja spremanjem i vraćanjem globalnog stanja paketa.\n" ++ "Uobičajena (default) radnja je spremanje trenutnog stanja paketa."
saveS German    = yellow "Verwalte das [S]peichern und Wiederherstellen des globalen Paketzustände.\n" ++ "Standardaktion sichert die Zustände."
saveS Serbian   = yellow "Управља чувањем и враћањем глобалног стања пакета.\n" ++ "Уобичајена радња чува тренутно стање."
saveS Norwegian = yellow "Administer lagring og gjenoppretting av den globale pakketilstanden.\n" ++ "Standard-handling lagrer denne tilstanden."
saveS Italian   = yellow "Gestisco il [S]alvataggio e ripristino dello stato globale dei pacchetti.\n" ++ "Salva lo stato in maniera predefinita."
saveS French    = yellow "Gestion de la [S]auvegarde et restauration de l'état global des paquets.\n" ++ "Par défaut, sauvegarde l'état actuel."
saveS _         = yellow "Manage the [S]aving and restoring of the global package state.\n" ++ "Default action saves this state."

downG :: Language -> String
downG English    = red "Perform actions involving the package [C]ache.\n" ++ "Default action downgrades given packages."
downG Japanese   = red "キャッシュに関連する処理\n" ++ "デフォルトでパッケージをダウングレード"
downG Polish     = red "Wykonuje akcje związane z pamięcią podręczną ([C]ache) pakietów.\n" ++ "Domyślnie instaluje starsze wersje podanych pakietów."
downG Croatian   = red "Izvršava radnje sa [C]ache-om paketa.\n" ++ "Uobičajena (default) radnja je vraćanje paketa na prijašnju verziju."
downG Swedish    = red "Utför åtgärder involverandes paket-[C]ache.\n" ++ "Standard-åtgärd nergraderar valda paket."
downG German     = red "Führe Aktionen aus die den Paket[C]ache betreffen.\n" ++ "Standardaktion downgradet gegebene Pakete."
downG Spanish    = red "Realizar acciones relacionadas con la [C]aché.\n" ++ "La acción por defecto es retornar a versiones antiguas de los paquetes especificados."
downG Portuguese = red "Realiza ações relacionadas ao [C]ache.\n" ++ "Ação padrão retorna os pacotes informados às suas versões anteriores."
downG French     = red "Actions impliquant le [C]ache des paquets.\n" ++ "Par défaut, rétrograde les paquets spécifiés."
downG Russian    = red "Совершить действия с участием кэша пакета ([C]ache).\n" ++ "Действие по умолчанию откатывает данные пакеты к старым версиям."
downG Italian    = red "Azioni riguardanti la [C]ache dei pacchetti.\n" ++ "Di default retrocede il pacchetti."
downG Serbian    = red "Извршава радње везане за кеш пакета.\n" ++ "Уобичајена радња враћа претходну верзију датих пакета."
downG Norwegian  = red "Utfør handlinger som involverer pakke-[C]achen.\n" ++ "Standard-handling nedgraderer den valgte pakken."

viewL :: Language -> String
viewL English    = cyan "Perform actions involving the pacman [L]ogfile.\n" ++ "Default action opens the log for read-only viewing."
viewL Japanese   = cyan "[L]ogfileに関連する処理\n" ++ "デフォルトでlogfileを閲覧用に開く"
viewL Polish     = cyan "Wykonuje akcje związane z dziennikiem ([L]ogiem) pacmana.\n" ++ "Domyślnie otwiera log w trybie tylko do odczytu."
viewL Croatian   = cyan "Izvršavanje radnje sa pacman dnevnikom ([L]ogfile).\n" ++ "Uobičajena (default) radnja je ispis dnevnika."
viewL Swedish    = cyan "Utför åtgärder involverandes pacmans [L]ogfil.\n" ++ "Standard-åtgärd öppnar loggen med read-only-attribut."
viewL German     = cyan "Führe Aktionen aus die die Pacman [L]ogdatei betreffen.\n" ++ "Standardaktion öffnet den Log (nur Lesen)"
viewL Spanish    = cyan "Realizar acciones relacionadas con el fichero [L]og de pacman.\n" ++ "La acción por defecto es abrir el log en modo sólo lectura."
viewL Portuguese = cyan "Realiza ações relacionadas ao [L]ogfile do Pacman.\n" ++ "Ação padrão abre o arquivo de log apenas para leitura."
viewL French     = cyan "Actions impliquant le [L]ogfile (journal) de Pacman.\n" ++ "Par défaut, ouvre le journal en lecture seule."
viewL Russian    = cyan "Совершить действия с участием [L]og-файлов pacman.\n" ++ "Действие по умолчанию открывает лог для просмотра в режиме для чтения."
viewL Italian    = cyan "Azioni riguardanti i [L]ogfile di pacman.\n" ++ "Di default visualizza il log in sola lettura."
viewL Serbian    = cyan "Извршава радње везане за pacman-ов дневник.\n" ++ "Уобичајена радња даје преглед дневника."
viewL Norwegian  = cyan "Utfør handlinger som involverer `pacman`'s [L]oggfil.\n" ++ "Standard-handling åpner loggen for skrivebeskyttet lesing."

orpha :: Language -> String
orpha English    = blue "Perform actions involving [O]rphan packages.\n" ++ "Default action lists all orphan packages."
orpha Japanese   = blue "必要とされていない従属パッケージに関する処理\n" ++ "デフォルトでその従属パッケージの名前を出力"
orpha Polish     = blue "Wykonuje akcje związane z [O]sieroconymi pakietami.\n" ++ "Domyślnie wyświetla wszystkie osierocone pakiety."
orpha Croatian   = blue "Izvršava radnje s paketima bez roditelja ([O]rphan).\n" ++ "Uobičajena (default) radnja je izlistavanje paketa bez roditelja."
orpha Swedish    = blue "Utför åtgärder involverandes [O]rphan-paket.\n" ++ "Standard-åtgärd listar alla orphan-paket."
orpha German     = blue "Führe Aktionen aus die verwaiste ([O]rphans) Pakete betreffen.\n" ++ "Standardaktion listet alle verwaisten Pakete auf."
orpha Spanish    = blue "Realizar acciones relacionadas con paquetes huérfanos ([O]rphan).\n" ++ "La acción por defecto es listar todos los paquetes huérfanos."
orpha Portuguese = blue "Realiza ações com pacotes [O]rfãos.\n" ++ "Ação padrão lista todos os pactes orfãos."
orpha French     = blue "Actions impliquant les paquets [O]rphelins.\n" ++ "Par défaut, liste l'ensemble des paquets orphelins."
orpha Russian    = blue "Совершить действия с участием [O]сиротевших пакетов.\n" ++ "Действие по умолчанию берёт в расчёт все осиротевшие пакеты."
orpha Italian    = blue "Azioni riguardanti i pacchetti [O]rfani.\n" ++ "Di default elenca i pacchetti orfani."
orpha Serbian    = blue "Извршава радње везане за пакете без родитеља.\n" ++ "Уобичајена радња листа пакете без родитеља."
orpha Norwegian  = blue "Utfør handlinger som involverer foreldreløse pakker ([O]rphans).\n" ++ "Standard-handling åpner alle foreldreløse pakker."

-------------------------------
-- Aura/AUR functions
-------------------------------
-- NEEDS TRANSLATION
getAURPkgInfo_1 :: Language -> String
getAURPkgInfo_1 Japanese  = "AURのAPIに繋げなかった。ネット接続状態を確認して下さい。"
getAURPkgInfo_1 Croatian  = "Pristup AUR-u nije uspio. Provjerite svoju vezu."
getAURPkgInfo_1 German    = "AUR API Suche fehlgeschlagen. Bitte überprüfen Sie Ihre Verbindung."
getAURPkgInfo_1 Serbian   = "Приступ AUR-у није успео. Проверите вашу везу."
getAURPkgInfo_1 Norwegian = "AUR API-oppslag feilet. Vennligst sjekk tilkoblingen din."
getAURPkgInfo_1 Italian   = "connessione ad AUR API fallita. Controllare la propria connessione."
getAURPkgInfo_1 French    = "La recherche dans l'API AUR a échoué. Vérifiez votre connexion."
getAURPkgInfo_1 _         = "AUR API lookup failed. Please check your connection."

-- `Maintainer` value NEEDS UPDATING!
infoFields :: Language -> [String]
infoFields English    = [ "Repository","Name","Version","AUR Status","Maintainer","Project URL","AUR URL","License","Depends On","Build Deps","Votes","Description" ]
infoFields Japanese   = [ "リポジトリ","名前","バージョン","パッケージ状態","管理者","プロジェクト","パッケージページ","ライセンス","従属パッケージ","作成時従属パ","投票数","概要" ]
infoFields Polish     = [ "Repository","Nazwa","Wersja","Status w AUR","Maintainer","URL Projektu","URL w AUR","Licencja","Depends On","Build Deps","Głosy","Opis" ]
infoFields Croatian   = [ "Repozitorij","Ime","Verzija","AUR Stanje","Maintainer","URL Projekta","AUR URL","Licenca","Depends On","Build Deps","Glasovi","Opis" ]
infoFields Swedish    = [ "Repository","Namn","Version","AUR Status","Maintainer","Projekt URL","AUR URL","Licens","Depends On","Build Deps","Röster","Beskrivning" ]
infoFields German     = [ "Repository","Name","Version","AUR Status","Maintainer","Projekt URL","AUR URL","Lizenz","Depends On","Build Deps","Stimmen","Beschreibung" ]
infoFields Spanish    = [ "Repository","Nombre","Versión","Estado en AUR","Maintainer","URL del proyecto","URL en AUR","Licencia","Depends On","Build Deps","Votos","Descripción" ]
infoFields Portuguese = [ "Repositório","Nome","Versão","Estado no AUR","Maintainer","URL do projeto","URL no AUR","Licença","Depends On","Build Deps","Votos","Descrição" ]
infoFields French     = [ "Dépôt","Nom","Version","Statut de AUR","Mainteneur","URL du projet","URL AUR","Licence","Depends On","Build Deps","Votes","Description" ]
infoFields Russian    = [ "Репозиторий","Название","Версия","Статус в AUR","Maintainer","URL проекта","URL в AUR","Лицензия","Depends On","Build Deps","Рейтинг","Описание" ]
infoFields Italian    = [ "Repository","Nome","Versione","Stato in AUR","Maintainer","URL del progetto","URL AUR","Licenza","Depends On","Build Deps","Voti","Descrizione" ]
infoFields Serbian    = [ "Ризница","Име","Верзија","Статус у AUR-у","Maintainer","Страница пројекта","Страница у AUR-у","Лиценца","Depends On","Build Deps","Гласови","Опис" ]
infoFields Norwegian  = [ "Depot","Navn","Versjon","AUR Status","Vedlikeholder","Prosjekt-URL","AUR URL","Lisens","Depends On","Build Deps","Stemmer","Beskrivelse" ]

outOfDateMsg :: Bool -> Language -> String
outOfDateMsg True  English    = red "Out of Date!"
outOfDateMsg False English    = green "Up to Date"
outOfDateMsg True  Japanese   = red "AURで要更新！"
outOfDateMsg False Japanese   = green "最新"
outOfDateMsg True  Polish     = red "Nieaktualny!"
outOfDateMsg False Polish     = green "Aktualny"
outOfDateMsg True  Croatian   = red "Zastarjelo!"
outOfDateMsg False Croatian   = green "Ažurirano"
outOfDateMsg True  Swedish    = red "Utdaterad!"
outOfDateMsg False Swedish    = green "Aktuell"
outOfDateMsg True  German     = red "Veraltet!"
outOfDateMsg False German     = green "Aktuell"
outOfDateMsg True  Spanish    = red "¡Desactualizado!"
outOfDateMsg False Spanish    = green "Actualizado"
outOfDateMsg True  Portuguese = red "Desatualizado!"
outOfDateMsg False Portuguese = green "Atualizado"
outOfDateMsg True  French     = red "Périmé!"
outOfDateMsg False French     = green "À jour"
outOfDateMsg True  Russian    = red "Устарел!"
outOfDateMsg False Russian    = green "Новейший"
outOfDateMsg True  Italian    = red "Out of Date!"
outOfDateMsg False Italian    = green "Aggiornato"
outOfDateMsg True  Serbian    = red "Застарео!"
outOfDateMsg False Serbian    = green "Ажуран"
outOfDateMsg True  Norwegian  = red "Utdatert!"
outOfDateMsg False Norwegian  = green "Oppdatert"

orphanedMsg :: Maybe String -> Language -> String
orphanedMsg (Just m) _        = bForeground m
orphanedMsg Nothing Japanese  = red "いない"
orphanedMsg Nothing Croatian  = red "Nema roditelja!"
orphanedMsg Nothing German    = red "Verwaist!"
orphanedMsg Nothing Norwegian = red "Foreldreløs!"
orphanedMsg Nothing French    = red "Orphelin!"
orphanedMsg Nothing _         = red "Orphaned!"

-----------------------
-- Aura/ABS functions
-----------------------
-- NEEDS TRANSLATION
absSync_1 :: Language -> String
absSync_1 Japanese  = "ローカルABS Treeを同期？"
absSync_1 Croatian  = "Sinkronizirati lokalno ABS stablo?"
absSync_1 German    = "Lokalen ABS Baum synchronisieren?"
absSync_1 Norwegian = "Synkroniser det lokale ABS-treet?"
absSync_1 Italian   = "Sincronizzare l'albero ABS locale?"
absSync_1 French    = "Synchroniser l'arbre ABS local?"
absSync_1 _         = "Sync the local ABS Tree?"

absSync_2 :: Language -> String
absSync_2 Japanese  = "ローカルABS Treeを同期中・・・"
absSync_2 Croatian  = "Sinkroniziram lokalno ABS stablo..."
absSync_2 German    = "Synchronisiere lokalen ABS Baum..."
absSync_2 Norwegian = "Synkroniserer det lokale ABS-treet..."
absSync_2 Italian   = "Sincronizzo l'albero ABS locale..."
absSync_2 French    = "Synchronisation de l'arbre ABS local..."
absSync_2 _         = "Syncing local ABS Tree..."

singleSync_1 :: String -> Language -> String
singleSync_1 p Japanese  = bt p ++ "をABS Treeに同期・・・"
singleSync_1 p Croatian  = "Sinkroniziram " ++ bt p ++ " u lokalnom stablu..."
singleSync_1 p German    = "Synchronisiere " ++ bt p ++ " in den lokalen ABS Baum..."
singleSync_1 p Norwegian = "Synkroniserer " ++ bt p ++ " til det lokale ABS-treet..."
singleSync_1 p Italian   = "Sincronizzo " ++ bt p ++ " nell'albero ABS locale..."
singleSync_1 p French    = "Synchronisation de " ++ bt p ++ " dans l'arbre ABS local..."
singleSync_1 p _         = "Syncing " ++ bt p ++ " to the local ABS Tree..."

absInfoFields :: Language -> [String]
absInfoFields Croatian  = [ "Repozitorij","Ime","Verzija","Zavisnosti","Make Zavisnosti","Opis" ]
absInfoFields German    = [ "Quelle","Name","Version","Hängt ab von","Make Abh.","Beschreibung"]
absInfoFields Norwegian = [ "Depot","Navn","Versjon","Er avhengig av","Make Deps","Beskrivelse"]
absInfoFields Italian   = [ "Repository","Nome","Versione","Dipende da","Make Deps","Descrizione" ]
absInfoFields French    = [ "Dépôt","Nom","Version","Dépendances","Dépendances de compilation","Description" ]
absInfoFields _         = [ "Repository","Name","Version","Depends On","Make Deps","Description" ]

repository_1 :: String -> Language -> String
repository_1 p Japanese  = p ++ "はどのリポジトリにもない。"
repository_1 p Croatian  = p ++ "nije paket u repozitoriju."
repository_1 p German    = p ++ " ist kein Paket in irgendeiner Quelle."
repository_1 p Norwegian = p ++ " er ikke en pakke i noe depot."
repository_1 p Italian   = p ++ " non è un pacchetto di nessun repository."
repository_1 p French    = p ++ " n'est un paquet dans aucun des dépôts."
repository_1 p _         = p ++ " is not a package in any repository."

pkgBuildKeyMissing :: Language -> String -> String
pkgBuildKeyMissing Croatian  key = "Nemoguće izvući vrijednost za " ++ key ++ " iz PKGBUILD-a."
pkgBuildKeyMissing German    key = "Kann Schlüssel " ++ key ++ " nicht aus PKGBUILD parsen."
pkgBuildKeyMissing Norwegian key = "Forstår ikke " ++ key ++ " fra PKGBUILD."
pkgBuildKeyMissing Italian   key = "Inpossibile elaborare la chiave " ++ key ++ " dal PKGBUILD."
pkgBuildKeyMissing French    key = "Impossible de parser la clef " ++ key ++ " depuis le PKGBUILD."
pkgBuildKeyMissing _         key = "Unable to parse key " ++ key ++ " from PKGBUILD."

missingDescription :: Language -> String
missingDescription Croatian  = "Nema opisa."
missingDescription German    = "Keine Beschreibung."
missingDescription Norwegian = "Ingen beskrivelse."
missingDescription Italian   = "Nessuna Descrizione."
missingDescription French    = "Aucune description."
missingDescription _         = "No description."

-----------------------
-- Aura/State functions
-----------------------
-- NEEDS TRANSLATION
saveState_1 :: Language -> String
saveState_1 Japanese  = "現在パッケージ状態保存完了。"
saveState_1 Croatian  = "Stanje paketa spremljeno."
saveState_1 German    = "Paketzustände gesichert."
saveState_1 Serbian   = "Сачувано стање пакета."
saveState_1 Norwegian = "Lagret pakketilstand."
saveState_1 Italian   = "Stato del pacchetto salvato."
saveState_1 French    = "État des paquets sauvegardé."
saveState_1 _         = "Saved package state."

-- NEEDS TRANSLATION
restoreState_1 :: Language -> String
restoreState_1 Japanese  = "対象バージョンがないパッケージ："
restoreState_1 Croatian  = "Tražene stare verzije nisu dostupne za:"
restoreState_1 German    = "Gewünschte Downgradeversionen nicht Verfügbar für:"
restoreState_1 Serbian   = "Захтеване старе верзије нису доступне за:"
restoreState_1 Norwegian = "De spesifiserte nedgraderingsversjonene er ikke tilgjengelig for:"
restoreState_1 Italian   = "Richiesta di retrocessione di versione non disponibile per:"
restoreState_1 French    = "Version de rétrogradation requise non disponible pour:"
restoreState_1 _         = "Requested downgrade versions not available for:"

-- NEEDS TRANSLATION
reinstallAndRemove_1 :: Language -> String
reinstallAndRemove_1 Japanese  = "パッケージを変更する必要ない。"
reinstallAndRemove_1 Croatian  = "Nema paketa kojima su potrebne izmjene."
reinstallAndRemove_1 German    = "Keine Pakete brauchen Änderungen."
reinstallAndRemove_1 Serbian   = "Ниједан пакет не захтева измене."
reinstallAndRemove_1 Norwegian = "Ingen pakker trenger forandring."
reinstallAndRemove_1 Italian   = "Nessun pacchetto necessita cambiamenti."
reinstallAndRemove_1 French    = "Aucun paquet n'a besoin de changement."
reinstallAndRemove_1 _         = "No packages need changing."

--------------------------------------
-- Aura/Settings/BadPackages functions
--------------------------------------
-- NEEDS TRANSLATION
circDep_1 :: String -> Language -> String
circDep_1 p Japanese  = bt p ++ "と互いに従属している。"
circDep_1 p Croatian  = "Ima kružnu zavisnost sa " ++ bt p ++ "."
circDep_1 p German    = "Hat eine zirkuläre Abhängigkeit mit " ++ bt p ++ "."
circDep_1 p Serbian   = "Има кружну зависност са " ++ bt p ++ "."
circDep_1 p Norwegian = "Har en sirkulær avhengighet med " ++ bt p ++ "."
circDep_1 p Italian   = "E' una dipendenza circolare di " ++ bt p ++ "."
circDep_1 p French    = "A une dépendance circulaire avec " ++ bt p ++ "."
circDep_1 p _         = "Has a circular dependency with " ++ bt p ++ "."

-- NEEDS TRANSLATION
bashisms_1 :: Language -> String
bashisms_1 Japanese  = "PKGBUILDのBashコードが複雑すぎる。"
bashisms_1 Croatian  = "Previše „bash-izama“ u PKGBUILD-u."
bashisms_1 German    = "Zu viele „bashismen“ im PKGBUILD."
bashisms_1 Serbian   = "Превише „bash-изама“ у PKGBUILD-у."
bashisms_1 Norwegian = "For mange „bashismer“ i PKGBUILD."
bashisms_1 Italian   = "Troppo 'bashisms' nel PKGBUILD."
bashisms_1 French    = "Trop de \"bashisms\" dans le PKGBUILD."
bashisms_1 _         = "Too many bashisms in PKGBUILD."

------------------------
-- Aura/Pacman functions
------------------------
-- NEEDS TRANSLATION
pacmanFailure_1 :: Language -> String
pacmanFailure_1 Japanese  = "入力を確認して下さい。"
pacmanFailure_1 Croatian  = "Molim vas, provjerite svoj unos."
pacmanFailure_1 German    = "Bitte überprüfen Sie Ihre Eingabe."
pacmanFailure_1 Serbian   = "Молим Вас, проверите ваш унос."
pacmanFailure_1 Norwegian = "Vennligst sjekk din oppføring."
pacmanFailure_1 Italian   = "Controllare il proprio input."
pacmanFailure_1 French    = "Merci de vérifier les donnés entrées."
pacmanFailure_1 _         = "Please check your input."

----------------------------------
-- Aura/Pkgbuild/Editing functions
----------------------------------
hotEdit_1 :: String -> Language -> String
hotEdit_1 p English    = "Would you like to edit the PKGBUILD of " ++ bt p ++ "?"
hotEdit_1 p Japanese   = bt p ++ "のPKGBUILDを編成？"
hotEdit_1 p Polish     = "Czy chcesz edytować PKGBUILD " ++ bt p ++ "?"
hotEdit_1 p Croatian   = "Želite li izmjeniti PKGBUILD " ++ bt p ++ "?"
hotEdit_1 p Swedish    = "Vill du ändra PKGBUILD-filen ifrån " ++ bt p ++ "?"
hotEdit_1 p German     = "Möchten Sie die PKGBUILD-Datei für " ++ bt p ++ " bearbeiten?"
hotEdit_1 p Spanish    = "¿Te gustaría editar el PKGBUILD de " ++ bt p ++ "?"
hotEdit_1 p Portuguese = "Desejaria editar o PKGBUILD de " ++ bt p ++ "?"
hotEdit_1 p French     = "Voulez-vous éditer le PKGBUILD de " ++ bt p ++ "?"
hotEdit_1 p Russian    = "Отредактировать PKGBUILD пакета " ++ bt p ++ "?"
hotEdit_1 p Italian    = "Volete modificare il PKGBUILD di " ++ bt p ++ "?"
hotEdit_1 p Serbian    = "Желите ли да измените PKGBUILD за " ++ bt p ++ "?"
hotEdit_1 p Norwegian  = "Vil du endre PKGBUILD for " ++ bt p ++ "?"

customizepkg_1 :: Language -> String
customizepkg_1 Japanese  = bt "customizepkg" ++ "はインストールされていない。"
customizepkg_1 Croatian  = bt "customizepkg" ++ "nije instaliran."
customizepkg_1 German    = bt "customizepkg" ++ "ist nicht installiert."
customizepkg_1 Norwegian = bt "customizepkg" ++ "er ikke installert."
customizepkg_1 Italian   = bt "customizepkg" ++ "non è installato."
customizepkg_1 French    = bt "customizepkg" ++ "n'est pas installé."
customizepkg_1 _         = bt "customizepkg" ++ "isn't installed."
