-- Library for Aura output in different languages.
-- All normal restrictions on line length do not apply for this file, and this file only.

{- AURA TRANSLATORS - Thank you all
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

-- These need updating! Or removing...
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

----------------------
-- Aura/Core functions
----------------------
-- NEEDS TRANSLATION
checkDBLock_1 :: Language -> String
checkDBLock_1 Japanese = "パッケージデータベースが今閉鎖状態。開放したらキーを押して続行をどうぞ。"
checkDBLock_1 Croatian = "Baza paketa je zaključana. Kad se otključa, pritisnite enter da biste nastavili."
checkDBLock_1 _        = "The package database is locked. Press enter when it's unlocked to continue."

-- Packages should not be built if the user is logged in as root!
trueRoot_1 :: Language -> String
trueRoot_1 English    = "You should never build packages as the true root. Are you okay with this?"
trueRoot_1 Japanese   = "本当のrootユーザーとしてパッケージを作成するのが危険。続行？"
trueRoot_1 Polish     = "Nigdy nie powinieneś budować pakietów jako root. Na pewno kontynuować?"
trueRoot_1 Croatian   = "Pakete ne bi trebalo graditi s pravim root ovlastima. Nastavi?"
trueRoot_1 Swedish    = "Det är starkt rekommenderat att INTE vara inloggad som root när man bygger paket. Vill du fortsätta ändå?"
trueRoot_1 German     = "Sie sollten niemals Pakete als der echte root Nutzer bauen. Sind sie sicher, dass Sie dies tun wollen?"
trueRoot_1 Spanish    = "Nunca deberías construir paquetes como root real. ¿Estás de acuerdo con esto?"
trueRoot_1 Portuguese = "Não deveria compilar pacotes como o root de fato. Ainda assim, deseja prosseguir?"
trueRoot_1 French     = "Il n'est pas sage de construire des paquets avec le compte root. Voulez-vous continuer ?"
trueRoot_1 Russian    = "Вам никогда не следует собирать пакеты под настоящим рутом. Договорились?"
trueRoot_1 Italian    = "Non si dovrebbero compilare pacchetti come root. Volete Continuare?"
trueRoot_1 Serbian    = "Не би требало градити пакете са правим root овлашћењима. Желите ли наставити?"

-- This is for when the user decides to refrain from building afterall.
trueRoot_2 :: Language -> String
trueRoot_2 English    = "You’ve done the right thing."
trueRoot_2 Japanese   = "よしよし。"
trueRoot_2 Polish     = "Postąpiłeś słusznie."
trueRoot_2 Croatian   = "Učinili ste Ispravnu Stvar."
trueRoot_2 Swedish    = "Phew."
trueRoot_2 German     = "Eine weise Entscheidung."
trueRoot_2 Spanish    = "Has tomado la decision correcta."
trueRoot_2 Portuguese = "Ainda bem que tem juízo!"
trueRoot_2 French     = "C'est la bonne décision."
trueRoot_2 Russian    = "Вы выбрали православный путь."
trueRoot_2 Italian    = "Hai fatto la cosa giusta."
trueRoot_2 Serbian    = "Исправно сте поступили."

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

-----------------------
-- Aura/Build functions
-----------------------
buildPackages_1 :: Language -> String -> String
buildPackages_1 English    p = "Building " ++ bt p ++ "..."
buildPackages_1 Japanese   p = bt p ++ "を作成中・・・"
buildPackages_1 Polish     p = "Budowanie " ++ bt p ++ "..."
buildPackages_1 Croatian   p = "Gradim " ++ bt p ++ "..."
buildPackages_1 Swedish    p = "Bygger paket " ++ bt p ++ "..."
buildPackages_1 German     p = "Baue Paket " ++ bt p ++ "..."
buildPackages_1 Spanish    p = "Construyendo " ++ bt p ++ "..."
buildPackages_1 Portuguese p = "Compilando " ++ bt p ++ "..."
buildPackages_1 French     p = "Construction de " ++ bt p ++ "…"
buildPackages_1 Russian    p = "Сборка " ++ bt p ++ "..."
buildPackages_1 Italian    p = "Compilazione di " ++ bt p ++ "..."
buildPackages_1 Serbian    p = "Градим " ++ bt p ++ "..."

buildFail_1 :: Language -> String -> String
buildFail_1 English    p = "Well, building " ++ bt p ++ " failed."
buildFail_1 Japanese   p = bt p ++ "の作成は失敗したようだ。"
buildFail_1 Polish     p = "Budowanie " ++ bt p ++ " zakończyło się niepowodzeniem."
buildFail_1 Croatian   p = "Izgradnja " ++ bt p ++ " nije uspjela."
buildFail_1 Swedish    p = "Det gick inte att bygga paketet " ++ bt p ++ "."
buildFail_1 German     p = "Bauen von " ++ bt p ++ " ist fehlgeschlagen."
buildFail_1 Spanish    p = "La construcción de " ++ bt p ++ " ha fallado."
buildFail_1 Portuguese p = "Falha na compilação do pacote " ++ bt p ++ "."
buildFail_1 French     p = "Bon, la construction de " ++ bt p ++ " a échouée."
buildFail_1 Russian    p = "Что ж, сборка " ++ bt p ++ " не удалась."
buildFail_1 Italian    p = "La compilazione di " ++ bt p ++ "è fallita."
buildFail_1 Serbian    p = "Изградња пакета " ++ bt p ++ " није успела."

buildFail_2 :: Language -> String
buildFail_2 English    = "Also, the following weren’t built:"
buildFail_2 Japanese   = "ちなみに下記のパッケージも作成されなかった："
buildFail_2 Polish     = "Dodatkowo, następujące pakiety nie zostały zbudowane:"
buildFail_2 Croatian   = "Dodatno, ni sljedeće nije izgrađeno:"
buildFail_2 Swedish    = "Det gick heller inte att bygga följande paket:"
buildFail_2 German     = "Die folgenden Pakete wurden zusätzlich nicht gebaut:"
buildFail_2 Spanish    = "Los siguientes paquetes no se han construido:"
buildFail_2 Portuguese = "Os pacotes a seguir não foram compilados:"
buildFail_2 French     = "En outre, les paquets suivants n'ont pu être construits :"
buildFail_2 Russian    = "К тому же следующие пакеты не были собраны:"
buildFail_2 Italian    = "Inoltre non è stato possibile cotruire i seguenti pacchetti:"
buildFail_2 Serbian    = "Такође, ни следећи пакети нису изграђени::"

buildFail_3 :: Language -> String
buildFail_3 English    = "However, these packages were successfully built:"
buildFail_3 Japanese   = "しかし、以下のパッケージファイルは無事作成された："
buildFail_3 Polish     = "Następujące pakiety zostały zbudowane pomyślnie:"
buildFail_3 Croatian   = "Neki paketi su možda izgrađeni uspješno."
buildFail_3 Swedish    = "Vissa paket kanske har byggts ordentligt (Osäker)."
buildFail_3 German     = "Diese Pakete wurden wiederrum erfolgreich gebaut:"
buildFail_3 Spanish    = "Sin embargo, los siguientes paquetes se han construido:"
buildFail_3 Portuguese = "Entretanto, os seguintes pacotes compilaram com sucesso:"
buildFail_3 French     = "Cependant, les paquets suivants ont été construits avec succès :"
buildFail_3 Russian    = "Однако эти пакеты были успешно собраны:"
buildFail_3 Italian    = "Comunque questi pacchetti sono stato compilati con successo:"
buildFail_3 Serbian    = "Међутим, ови пакети су успешно изграђени:"

buildFail_4 :: Language -> String
buildFail_4 English    = "Would you like to install them?"
buildFail_4 Japanese   = "できたやつのインストールを続行する？"
buildFail_4 Polish     = "Czy chcesz je zainstalować?"
buildFail_4 Croatian   = "Želite li ih instalirati?"
buildFail_4 Swedish    = "Vill du installera dem?"
buildFail_4 German     = "Möchten sie diese installieren?"
buildFail_4 Spanish    = "¿Te gustaría instalarlos?"
buildFail_4 Portuguese = "Gostaria de instalá-los?"
buildFail_4 French     = "Voulez-vous les installer ?"
buildFail_4 Russian    = "Желаете ли вы их установить?"
buildFail_4 Italian    = "Volete installarli?"
buildFail_4 Serbian    = "Желите ли их инсталирати?"

buildFail_5 :: Language -> String
buildFail_5 English    = "Building failed."
buildFail_5 Japanese   = "パッケージ作成は失敗した。"
buildFail_5 Polish     = "Budowanie nie powiodło się."
buildFail_5 Croatian   = "Izgradnja nije uspjela."
buildFail_5 Swedish    = "Gick inte att bygga paket."
buildFail_5 German     = "Bauen fehlgeschlagen."
buildFail_5 Spanish    = "La construcción falló."
buildFail_5 Portuguese = "Falha na compilação."
buildFail_5 French     = "Construction ratée."
buildFail_5 Russian    = "Сборка не удалась."
buildFail_5 Italian    = "Compilazione fallita."
buildFail_5 Serbian    = "Изградња пакета није успела."

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

getRealPkgConflicts_2 :: String -> Language -> String
getRealPkgConflicts_2 p English    = bt p ++ " is an ignored package! See your `pacman.conf` file."    
getRealPkgConflicts_2 p Japanese   = bt p ++ "は無視されるパッケージ！`pacman.conf`を参考に。"
getRealPkgConflicts_2 p Polish     = bt p ++ " jest ignorowany! Sprawdź plik `pacman.conf`."
getRealPkgConflicts_2 p Croatian   = bt p ++ " je ignoriran paket! Pogledajte svoj `pacman.conf`."
getRealPkgConflicts_2 p Swedish    = bt p ++ " är ett ignorerat paket! Kolla din `pacman.conf`-fil."
getRealPkgConflicts_2 p German     = bt p ++ " ist ein ignoriertes Paket! Siehe /etc/pacman.conf."
getRealPkgConflicts_2 p Spanish    = "¡" ++ bt p ++ " es un paquete ignorado! Revisa tu fichero `pacman.conf`."
getRealPkgConflicts_2 p Portuguese = bt p ++ " é um pacote ignorado conforme configuração em `pacman.conf`!"
getRealPkgConflicts_2 p French     = "Le paquet " ++ bt p ++ " est ignoré. Vous devriez jeter un œil à votre `pacman.conf`."
getRealPkgConflicts_2 p Russian    = "Пакет " ++ bt p ++ " игнорируется! Проверьте ваш файл `pacman.conf`."
getRealPkgConflicts_2 p Italian    = bt p ++ " è un pacchetto ignorato, controllare `pacman.conf`."
getRealPkgConflicts_2 p Serbian    = "Пакет " ++ bt p ++ " је игнорисан! Видите ваш фајл „pacman.conf“."

getVirtualConflicts_1 :: String -> Language -> String
getVirtualConflicts_1 p English    = bt p ++ " exists in NO WAY as a package or as one provided by another!"
getVirtualConflicts_1 p Japanese   = bt p ++ "はパッケージでもないし、他のパッケージにも提供されていない！"
getVirtualConflicts_1 p Polish     = bt p ++ " nie istnieje jako pakiet lub jako pakiet dostarczany przez inny!"
getVirtualConflicts_1 p Croatian   = bt p ++ " ne postoji kao paket niti ga bilo koji paket pruža!"
getVirtualConflicts_1 p Swedish    = bt p ++ " existerar varken som ett paket eller som ett tillhandahållet av ett annat!"
getVirtualConflicts_1 p German     = bt p ++ " existiert nicht als Paket oder als Bereitstellung eines anderen!"
getVirtualConflicts_1 p Spanish    = "¡" ++ bt p ++ " no existe como paquete ni es provisto por ninguno!"
getVirtualConflicts_1 p Portuguese = bt p ++ " não existe como um pacote e não é provido por nenhum!"
getVirtualConflicts_1 p French     = bt p ++ " n'est ni un paquet existant, ni un paquet fourni par un autre !"
getVirtualConflicts_1 p Russian    = bt p ++ " никоим образом не существует в виде пакета или пакета, " ++ " предоставленного другим пакетом!"
getVirtualConflicts_1 p Italian    = bt p ++ " non esiste e non è distribuito da nessun'altro."
getVirtualConflicts_1 p Serbian    = bt p ++ " не постоји као пакет нити га други пакет пружа!"

getVirtualConflicts_2 :: String -> String -> Language -> String
getVirtualConflicts_2 p pro English    = bt pro ++ " provides " ++ bt p ++ ", but " ++ bt pro ++ " is an ignored package."
getVirtualConflicts_2 p pro Japanese   = bt p ++ "は" ++ bt pro ++ "に提供されているが、" ++ bt pro ++ "は無視されるパッケージ。"
getVirtualConflicts_2 p pro Polish     = bt pro ++ " dostarcza " ++ bt p ++ ", ale " ++ bt pro ++ " jest ignorowany."
getVirtualConflicts_2 p pro Croatian   = bt pro ++ " pruža  " ++ bt p ++ ", ali " ++ bt pro ++ " je ignoriran paket."
getVirtualConflicts_2 p pro Swedish    = bt pro ++ " tillhandahåller " ++ bt p ++ ", men " ++ bt pro ++ " är ett ignorerat paket."
getVirtualConflicts_2 p pro German     = bt pro ++ " stellt " ++ bt p ++ " bereit, aber " ++ bt pro ++ " ist ein ignoriertes Paket."
getVirtualConflicts_2 p pro Spanish    = bt pro ++ " provee " ++ bt p ++ ", pero " ++ bt pro ++ " es un paquete ignorado."
getVirtualConflicts_2 p pro Portuguese = bt pro ++ " provê " ++ bt p ++ ", mas " ++ bt pro ++ "é um pacote ignorado."
getVirtualConflicts_2 p pro French     = bt pro ++ " fourni " ++ bt p ++ ", mais " ++ bt pro ++ " est un paquet ignoré."
getVirtualConflicts_2 p pro Russian    = bt pro ++ " предоставляет " ++ bt p ++ ", но " ++ bt pro ++ " является игнорируемым пакетом."
getVirtualConflicts_2 p pro Italian    = bt pro ++ " distribuisce " ++ bt p ++ " ma " ++ bt pro ++ " è un pacchetto ignorato."
getVirtualConflicts_2 p pro Serbian    = bt pro ++ " пружа " ++ bt p ++ ", али је " ++ bt pro ++ " игнорисан пакет."

getVirtualConflicts_3 :: String -> String -> String -> String -> Language -> String
getVirtualConflicts_3 d dv p pv English    = "The dependency " ++ bt d ++ " demands version " ++ bt dv ++ " but its providing package " ++
                                             bt p ++ " gives version " ++ bt pv
getVirtualConflicts_3 d dv p pv Japanese   = "仮のパッケージ" ++ bt d ++ "はバージョン" ++ bt dv ++ "を要するが、" ++ "それを提供する" ++
                                             bt p ++ "はバージョン" ++ bt pv ++ "しか提供しない"
getVirtualConflicts_3 d dv p pv Polish     = "Zależność " ++ bt d ++ " powinna być w wersji " ++ bt dv ++ ", ale pakiet dostarczający (" ++
                                             bt p ++ ") jest w wersji " ++ bt pv
getVirtualConflicts_3 d dv p pv Croatian   = "Zavisnost " ++ bt d ++ " zahtjeva verziju " ++ bt dv ++ ", ali paket " ++ bt p ++ " pruža verziju " ++ bt pv
getVirtualConflicts_3 d dv p pv Swedish    = "Beroendepaket " ++ bt d ++ " kräver version " ++ bt dv ++ " men dens tillhandahållande paket " ++
                                             bt p ++ " ger version " ++ bt pv
getVirtualConflicts_3 d dv p pv German     = "Die Abhängigkeit " ++ bt d ++ " verlangt Version " ++ bt dv ++ " aber dessen bereitstellendes Paket " ++
                                             bt p ++ " gibt Version " ++ bt pv
getVirtualConflicts_3 d dv p pv Spanish    = "La dependencia " ++ bt d ++ " requiere la versión " ++ bt dv ++ " pero el paquete " ++
                                             bt p ++ ", que la provee, da la versión " ++ bt pv
getVirtualConflicts_3 d dv p pv Portuguese = "A dependência " ++ bt d ++ " requer a versão " ++ bt dv ++ " entretanto, o pacote " ++
                                             bt p ++ ", que o provê, possui a versão " ++ bt pv
getVirtualConflicts_3 d dv p pv French     = "La dépendance " ++ bt d ++ " nécessite la version " ++ bt dv ++ ", mais le paquet qui la fournie (" ++
                                             bt p ++ ") ne le fait qu'en version " ++ bt pv ++ "."
getVirtualConflicts_3 d dv p pv Russian    = "Зависимость " ++ bt d ++ " должна быть версии " ++ bt dv ++ ", но предоставленный для неё пакет " ++
                                             bt p ++ " имеет версию " ++ bt pv
getVirtualConflicts_3 d dv p pv Italian    = "La dipendenza " ++ bt d ++ " richiede la versione " ++ bt dv ++ " ma il pacchetto " ++
                                             bt p ++ " distribuisce la versione " ++ bt pv
getVirtualConflicts_3 d dv p pv Serbian    = "Зависност " ++ bt d ++ " захтева верзију " ++ bt dv ++ ", али пакет " ++ bt p ++
                                             " пружа верзију " ++ bt pv

-----------------
-- aura functions
-----------------
executeOpts_1 :: Language -> String
executeOpts_1 English    = "Conflicting flags given!"
executeOpts_1 Japanese   = "矛盾しているオプションあり。"
executeOpts_1 Polish     = "Niektóre flagi są w konflikcie ze sobą!"
executeOpts_1 Croatian   = "Predane zastavice su konfliktne!"
executeOpts_1 Swedish    = "Givna flaggor är i konflikt!"
executeOpts_1 German     = "Gegebene Kommandozeilenflags sind widersprüchlich!"
executeOpts_1 Spanish    = "¡Flags contradictorios!"
executeOpts_1 Portuguese = "Flags conflitantes!"
executeOpts_1 French     = "Arguments contradictoires !"
executeOpts_1 Russian    = "Даны конфликтующие флаги!"
executeOpts_1 Italian    = "Argomenti in conflitto!"
executeOpts_1 Serbian    = "Захтеване опције су контрадикторне!"

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

displayOutputLanguages_1 :: Language -> String
displayOutputLanguages_1 English    = "The following languages are available:"
displayOutputLanguages_1 Japanese   = "auraは下記の言語に対応している："
displayOutputLanguages_1 Polish     = "Następujące języki są dostępne:"
displayOutputLanguages_1 Croatian   = "Dostupni su sljedeći jezici:"
displayOutputLanguages_1 Swedish    = "Följande språk är tillängliga:"
displayOutputLanguages_1 German     = "Die folgenden Sprachen sind verfügbar:"
displayOutputLanguages_1 Spanish    = "Los siguientes idiomas están disponibles:"
displayOutputLanguages_1 Portuguese = "Os seguintes idiomas estão disponíveis:"
displayOutputLanguages_1 French     = "Les langues suivantes sont disponibles :"
displayOutputLanguages_1 Russian    = "Доступны следующие языки:"
displayOutputLanguages_1 Italian    = "Sono disponibili le seguenti lingue:"
displayOutputLanguages_1 Serbian    = "Доступни су следећи језици:"

----------------------------
-- Aura/Commands/A functions
----------------------------
installPackages_1 :: Language -> String
installPackages_1 English    = "Dependency checking failed for these reasons:"
installPackages_1 Japanese   = "従属パッケージの確認は以下の理由で失敗した："
installPackages_1 Polish     = "Sprawdzanie zależności nie powiodło się z następujących powodów:"
installPackages_1 Croatian   = "Provjera zavisnosti nije uspjela iz sljedećih razloga:"
installPackages_1 Swedish    = "Beroende-kollen misslyckades pga följande skäl:"
installPackages_1 German     = "Abhängigkeitsüberprüfung schlug Fehl aus folgenden Gründen:"
installPackages_1 Spanish    = "La comprobación de dependencias falló por los siguientes motivos:"
installPackages_1 Portuguese = "Não foi possível checar as dependências pelas seguintes razões:"
installPackages_1 French     = "La vérification des dépendances a faillie pour les raisons suivantes :"
installPackages_1 Russian    = "Проверка зависимостей не удалась из-за:"
installPackages_1 Italian    = "Il controllo delle dipendenze è fallito per i seguenti motivi:"
installPackages_1 Serbian    = "Провера зависности није успела из следећих разлога:"

installPackages_2 :: Language -> String
installPackages_2 English    = "No valid packages specified."
installPackages_2 Japanese   = "適当なパッケージを入力してください。"
installPackages_2 Polish     = "Nie podano prawidłowych pakietów."
installPackages_2 Croatian   = "Nije specificiran nijedan ispravan paket."
installPackages_2 Swedish    = "Inga giltiga paket valda."
installPackages_2 German     = "Keine gültigen Pakete angegeben."
installPackages_2 Spanish    = "No se ha especificado ningún paquete válido."
installPackages_2 Portuguese = "Nenhum pacote válido foi especificado."
installPackages_2 French     = "Aucun paquet valide spécifié."
installPackages_2 Russian    = "Валидные пакеты не указаны."
installPackages_2 Italian    = "Nessun pacchetto valido specificato."
installPackages_2 Serbian    = "Ниједан исправан пакет није специфициран."

installPackages_3 :: Language -> String
installPackages_3 English    = "Continue?"
installPackages_3 Japanese   = "続行？"
installPackages_3 Polish     = "Kontynuować?"
installPackages_3 Croatian   = "Nastavi?"
installPackages_3 Swedish    = "Fortsätta?"
installPackages_3 German     = "Fortsetzen?"
installPackages_3 Spanish    = "¿Continuar?"
installPackages_3 Portuguese = "Continuar?"
installPackages_3 French     = "Continuer ?"
installPackages_3 Russian    = "Продолжить?"
installPackages_3 Italian    = "Continuare?"
installPackages_3 Serbian    = "Наставити?"

installPackages_4 :: Language -> String
installPackages_4 English    = "Installation manually aborted."
installPackages_4 Japanese   = "続行は意図的に阻止された。"
installPackages_4 Polish     = "Instalacja została przerwana przez użytkownika."
installPackages_4 Croatian   = "Instalacija prekinuta od strane korisnika."
installPackages_4 Swedish    = "Installationen avbröts manuellt."
installPackages_4 German     = "Installation durch Benutzer abgebrochen."
installPackages_4 Spanish    = "Instalación abortada manualmente."
installPackages_4 Portuguese = "Instalação manual abortada."
installPackages_4 French     = "Installation manuelle annulée."
installPackages_4 Russian    = "Пользователь прервал установку."
installPackages_4 Italian    = "Installazione manuale interrotta."
installPackages_4 Serbian    = "Инсталација је ручно прекинута."

installPackages_5 :: Language -> String
installPackages_5 English    = "Determining dependencies..."
installPackages_5 Japanese   = "従属パッケージを確認中・・・"
installPackages_5 Polish     = "Ustalanie zależności..."
installPackages_5 Croatian   = "Određivanje zavisnosti..."
installPackages_5 Swedish    = "Avgör beroenden..."
installPackages_5 German     = "Bestimme Abhängigkeiten..."
installPackages_5 Spanish    = "Determinando dependencias..."
installPackages_5 Portuguese = "Determinando as dependências..."
installPackages_5 French     = "Détermination des dépendances en cours…"
installPackages_5 Russian    = "Определение зависимостей..."
installPackages_5 Italian    = "Determinazione dipendenze..."
installPackages_5 Serbian    = "Утврђивање зависности..."

-- NEEDS TRANSLATION
knownBadPkgCheck_1 :: String -> Language -> String
knownBadPkgCheck_1 p Japanese = bt p ++ "の作成は失敗すると知られている。理由："
knownBadPkgCheck_1 p Croatian = "Poznato je da se " ++ bt p ++ " neuspješno gradi. Razlog:"
knownBadPkgCheck_1 p Serbian  = "Познато је да се " ++ bt p ++ " неуспешно гради. Разлог:"
knownBadPkgCheck_1 p _        = bt p ++ " is known to fail at building. Reason:"

-- NEEDS TRANSLATION
knownBadPkgCheck_2 :: Language -> String
knownBadPkgCheck_2 Japanese = "それでもやってみる？"
knownBadPkgCheck_2 Croatian = "Želite li ipak pokušati?"
knownBadPkgCheck_2 Serbian  = "Желите ли ипак да пробате?"
knownBadPkgCheck_2 _        = "Will you try anyway?"

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
reportNonPackages_1 Italian    = "I seguenti non sono pacchetti:"
reportNonPackages_1 Serbian    = "Ово нису пакети:"

reportIgnoredPackages_1 :: Language -> String
reportIgnoredPackages_1 English    = "The following packages will be ignored:"
reportIgnoredPackages_1 Japanese   = "下記のパッケージは無視される："
reportIgnoredPackages_1 Polish     = "Poniższe pakiety zostaną zignorowane:"
reportIgnoredPackages_1 Croatian   = "Sljedeći paketi će biti ignorirani:"
reportIgnoredPackages_1 Swedish    = "Följande paket kommer att ignoreras: "
reportIgnoredPackages_1 German     = "Die folgenden Pakete werden ignoriert:"
reportIgnoredPackages_1 Spanish    = "Los siguientes paquetes serán ignorados:"
reportIgnoredPackages_1 Portuguese = "Os seguintes pacotes serão ignorados:"
reportIgnoredPackages_1 French     = "Les paquets suivants seront ignorés :"
reportIgnoredPackages_1 Russian    = "Следующие пакеты будут проигнорированы:"
reportIgnoredPackages_1 Italian    = "I seguenti pacchetti verranno ignorati:"
reportIgnoredPackages_1 Serbian    = "Следећи пакети ће бити игнорисани:"

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

reportPkgsToInstall_2 :: Language -> String
reportPkgsToInstall_2 English    = "AUR dependencies:"
reportPkgsToInstall_2 Japanese   = "AURの従属パッケージ："
reportPkgsToInstall_2 Polish     = "Zależności z AUR:"
reportPkgsToInstall_2 Croatian   = "Zavisnosti iz AUR-a:"
reportPkgsToInstall_2 Swedish    = "Beroenden ifrån AUR:"
reportPkgsToInstall_2 German     = "Abhängigkeiten im AUR:"
reportPkgsToInstall_2 Spanish    = "Dependencias en AUR:"
reportPkgsToInstall_2 Portuguese = "Dependências no AUR:"
reportPkgsToInstall_2 French     = "Dépendances AUR :"
reportPkgsToInstall_2 Russian    = "Зависимости из AUR:"
reportPkgsToInstall_2 Italian    = "Dipendenze in AUR:"
reportPkgsToInstall_2 Serbian    = "Зависности из AUR-а:"

reportPkgsToInstall_3 :: Language -> String
reportPkgsToInstall_3 English    = "Main AUR packages:"
reportPkgsToInstall_3 Japanese   = "主なAURパッケージ："
reportPkgsToInstall_3 Polish     = "Główne pakiety z AUR:"
reportPkgsToInstall_3 Croatian   = "Glavni AUR paketi:"
reportPkgsToInstall_3 Swedish    = "Huvudpaket ifrån AUR:"
reportPkgsToInstall_3 German     = "Hauptpaket aus dem AUR:"
reportPkgsToInstall_3 Spanish    = "Paquetes principales de AUR:"
reportPkgsToInstall_3 Portuguese = "Pacotes principais do AUR:"
reportPkgsToInstall_3 French     = "Principaux paquets AUR :"
reportPkgsToInstall_3 Russian    = "Главные пакеты из AUR:"
reportPkgsToInstall_3 Italian    = "Pacchetto principale di AUR"
reportPkgsToInstall_3 Serbian    = "Главни пакети из AUR-а:"

-- Needs translations.
reportPkgbuildDiffs_1 :: String -> Language -> String
reportPkgbuildDiffs_1 p Japanese  = bt p ++ "のPKGBUILDはまだ保存されていない。"
reportPkgbuildDiffs_1 p Polish   = bt p ++ " nie ma jeszcze przechowywanego pliku PKGBUILD."
reportPkgbuildDiffs_1 p Croatian = bt p ++ " još nema pohranjen PKGBUILD."
reportPkgbuildDiffs_1 p Spanish  = bt p ++ " no tiene PKGBUILD todavía."
reportPkgbuildDiffs_1 p French   = bt p ++ " n'a pas encore de PKGBUILD enrigistré."
reportPkgbuildDiffs_1 p Russian  = "У " ++ bt p ++ " ещё нет сохраненного PKGBUILD."
reportPkgbuildDiffs_1 p Italian  = bt p ++ " non ci sono PKGBUILD salvati"
reportPkgbuildDiffs_1 p Serbian  = bt p ++ " још нема похрањен PKGBUILD."
reportPkgbuildDiffs_1 p _        = bt p ++ " has no stored PKGBUILD yet."

reportPkgbuildDiffs_2 :: String -> Language -> String
reportPkgbuildDiffs_2 p Japanese  = bt p ++ "のPKGBUILDは最新。"
reportPkgbuildDiffs_2 p Polish   = "PKGBUILD pakietu " ++ bt p ++ " jest aktualny."
reportPkgbuildDiffs_2 p Croatian = "PKGBUILD paketa " ++ bt p ++ " je na najnovijoj verziji."
reportPkgbuildDiffs_2 p Spanish  = "El PKGBUILD de " ++ bt p ++ " está actualizado."
reportPkgbuildDiffs_2 p Russian  = "PKGBUILD " ++ bt p ++ " является новейшим."
reportPkgbuildDiffs_2 p French   = "Le PKGBUILD de " ++ bt p ++ " est à jour."
reportPkgbuildDiffs_2 p Italian  = "Il PKGBUILD di " ++ bt p ++ " è aggiornato."
reportPkgbuildDiffs_2 p Serbian  = "PKGBUILD пакета " ++ bt p ++ " је ажуран."
reportPkgbuildDiffs_2 p _        = bt p ++ " PKGBUILD is up to date."

reportPkgbuildDiffs_3 :: String -> Language -> String
reportPkgbuildDiffs_3 p Japanese = bt p ++ "のPKGBUILD変更報告："
reportPkgbuildDiffs_3 p Polish   = "Zmiany w PKGBUILD dla " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Croatian = "Promjene u PKGBUILD-u za " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Spanish  = "Cambios en el PKGBUILD de " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p Russian  = "Изменения, вносимые " ++ bt p ++ " PKGBUILD:"
reportPkgbuildDiffs_3 p French   = "Changements du PKGBUILD de " ++ bt p
reportPkgbuildDiffs_3 p Italian  = "Cambiamenti nel PKGBUILD di " ++ bt p ++":"
reportPkgbuildDiffs_3 p Serbian  = "Промене PKGBUILD-a за " ++ bt p ++ ":"
reportPkgbuildDiffs_3 p _        = bt p ++ " PKGBUILD changes:"

reportPkgsToUpgrade_1 :: Language -> String
reportPkgsToUpgrade_1 English    = "AUR Packages to upgrade:"
reportPkgsToUpgrade_1 Japanese   = "アップグレードするAURパッケージ："
reportPkgsToUpgrade_1 Polish     = "Pakiety z AUR do zaktualizowania:"
reportPkgsToUpgrade_1 Croatian   = "AUR paketi za nadograditi:"
reportPkgsToUpgrade_1 Swedish    = "AUR-paket att uppgradera:"
reportPkgsToUpgrade_1 German     = "Zu aktualisierendes AUR Paket:"
reportPkgsToUpgrade_1 Spanish    = "Paquetes de AUR a actualizar:"
reportPkgsToUpgrade_1 Portuguese = "Pacotes do AUR para atualizar:"
reportPkgsToUpgrade_1 French     = "Paquets AUR à mettre à jour :"
reportPkgsToUpgrade_1 Russian    = "Пакеты AUR, готовые для обновления:"
reportPkgsToUpgrade_1 Italian    = "Pacchetti in AUR da aggiornare:"
reportPkgsToUpgrade_1 Serbian    = "Пакети из AUR-а за надоградњу:"

reportBadDowngradePkgs_1 :: Language -> String
reportBadDowngradePkgs_1 English    = "The following aren’t installed, and thus can’t be downgraded:"
reportBadDowngradePkgs_1 Japanese   = "このパッケージは最初からインストールしていないので、格下げはできない。"
reportBadDowngradePkgs_1 Polish     = "Poniższe pakeity nie są zainstalowane, i nie mogą być zainstalowane w starszej wersji:"
reportBadDowngradePkgs_1 Croatian   = "Sljedeći paketi nisu instalirani te se stoga ne mogu vratiti na stare verzije:"
reportBadDowngradePkgs_1 Swedish    = "Följande paket är inte installerade, och kan därför inte bli nergraderade:"
reportBadDowngradePkgs_1 German     = "Folgende Pakete sind nicht installiert und können daher nicht downgraded werden:"
reportBadDowngradePkgs_1 Spanish    = "Los siguientes paquetes no están instalados, por lo que no se pueden retornar a versiones antiguas:"
reportBadDowngradePkgs_1 Portuguese = "Os seguintes pacotes não estão instalados, logo não podem retornar a uma versão anterior:"
reportBadDowngradePkgs_1 French     = "Les paquets suivants ne sont pas installés ; ils ne peuvent être rétrogradés :"
reportBadDowngradePkgs_1 Russian    = "Следующие пакеты не установлены, а следовательно, не могут быть откачены к старой версии:"
reportBadDowngradePkgs_1 Italian    = "I seguenti pacchetti non sono stati installati e non posso essere retrocessi:"
reportBadDowngradePkgs_1 Serbian    = "Следећи пакети нису ни инсталирани, те се не могу вратити на старију верзију:"

upgradeAURPkgs_1 :: Language -> String
upgradeAURPkgs_1 English    = "Fetching package information..."
upgradeAURPkgs_1 Japanese   = "パッケージ情報をダウンロード中・・・"
upgradeAURPkgs_1 Polish     = "Pobieranie informacji o pakietach..."
upgradeAURPkgs_1 Croatian   = "Preuzimanje podataka o paketima..."
upgradeAURPkgs_1 Swedish    = "Hämtar paketinformation..."
upgradeAURPkgs_1 German     = "Rufe Paketinformationen ab..."
upgradeAURPkgs_1 Spanish    = "Obteniendo información de paquetes..."
upgradeAURPkgs_1 Portuguese = "Obtendo informação dos pacotes..."
upgradeAURPkgs_1 French     = "Obtention des informations des paquets en cours…"
upgradeAURPkgs_1 Russian    = "Сборка информации о пакетах..."
upgradeAURPkgs_1 Italian    = "Ottengo le informazioni del pacchetto..."
upgradeAURPkgs_1 Serbian    = "Преузимање информација о пакетима..."

upgradeAURPkgs_2 :: Language -> String
upgradeAURPkgs_2 English    = "Comparing package versions..."
upgradeAURPkgs_2 Japanese   = "バージョンを比較中・・・"
upgradeAURPkgs_2 Polish     = "Porównywanie wersji pakietów..."
upgradeAURPkgs_2 Croatian   = "Uspoređivanje verzija paketa..."
upgradeAURPkgs_2 Swedish    = "Jämför paket-versioner..."
upgradeAURPkgs_2 German     = "Vergleiche Paketversionen..."
upgradeAURPkgs_2 Spanish    = "Comparando versiones de paquetes..."
upgradeAURPkgs_2 Portuguese = "Comparando versões dos pacotes..."
upgradeAURPkgs_2 French     = "Comparaison des versions des paquets en cours…"
upgradeAURPkgs_2 Russian    = "Сравнение версий пакетов..."
upgradeAURPkgs_2 Italian    = "Confronto le ersioni del pacchetto..."
upgradeAURPkgs_2 Serbian    = "Упоређивање верзија пакета..."

upgradeAURPkgs_3 :: Language -> String
upgradeAURPkgs_3 English    = "No AUR package upgrades necessary."
upgradeAURPkgs_3 Japanese   = "アップグレードは必要ない。"
upgradeAURPkgs_3 Polish     = "Nie jest wymagana aktualizacja pakietów z AUR."
upgradeAURPkgs_3 Croatian   = "Svi AUR paketi su ažurirani."
upgradeAURPkgs_3 Swedish    = "Inga AUR-paketsuppgraderingar behövs."
upgradeAURPkgs_3 German     = "Keine AUR Paketaktualisierungen notwendig."
upgradeAURPkgs_3 Spanish    = "No ha sido necesario actualizar paquetes de AUR."
upgradeAURPkgs_3 Portuguese = "Nenhum pacote do AUR precisa de atualização."
upgradeAURPkgs_3 French     = "Aucune mise à jour de paquets AUR n'est nécessaire."
upgradeAURPkgs_3 Russian    = "Обновление пакетов из AUR не требуется."
upgradeAURPkgs_3 Italian    = "Non è necessario aggiornare pacchetti di AUR."
upgradeAURPkgs_3 Serbian    = "Ажурирање пакета из AUR-а није потребно."

downloadTarballs_1 :: String -> Language -> String
downloadTarballs_1 p English    = "Downloading " ++ bt p ++ " source tarball..."
downloadTarballs_1 p Japanese   = bt p ++ "のソースコードのターボールをダウンロード中・・・"
downloadTarballs_1 p Polish     = "Pobieranie paczki źródłowej " ++ bt p ++ "..."
downloadTarballs_1 p Croatian   = "Preuzimanje izvornog paketa (tarball) " ++ bt p ++ "..."
downloadTarballs_1 p Swedish    = "Laddar ner " ++ bt p ++ " källkodspaket (tarball)..."
downloadTarballs_1 p German     = "Lade Quelltext von " ++ bt p ++ " (tarball)..."
downloadTarballs_1 p Spanish    = "Descargando los fuentes comprimidos (tarball) de " ++ bt p ++ " ..."
downloadTarballs_1 p Portuguese = "Baixando os fontes (tarball) de " ++ bt p ++ " ..."
downloadTarballs_1 p French     = "Téléchargement de l'archive de " ++ bt p ++ " en cours…"
downloadTarballs_1 p Russian    = "Загрузка исходного архива " ++ bt p ++ "..."
downloadTarballs_1 p Italian    = "Downlaod del tarball di " ++ bt p ++ " in corso..."
downloadTarballs_1 p Serbian    = "Преузимање архиве изворног кода за " ++ bt p ++ "..."

displayPkgbuild_1 :: String -> Language -> String
displayPkgbuild_1 p English    = bt p ++ " does not exist."
displayPkgbuild_1 p Japanese   = bt p ++ "は存在しない。"
displayPkgbuild_1 p Polish     = bt p ++ " nie istnieje."
displayPkgbuild_1 p Croatian   = bt p ++ " ne postoji."
displayPkgbuild_1 p Swedish    = bt p ++ " finns inte."
displayPkgbuild_1 p German     = bt p ++ " existiert nicht."
displayPkgbuild_1 p Spanish    = bt p ++ " no existe."
displayPkgbuild_1 p Portuguese = bt p ++ " não existe."
displayPkgbuild_1 p French     = bt p ++ "n'existe pas."
displayPkgbuild_1 p Russian    = bt p ++ " не существует."
displayPkgbuild_1 p Italian    = bt p ++ " inesistente."
displayPkgbuild_1 p Serbian    = bt p ++ " не постоји."

removeMakeDepsAfter_1 :: Language -> String
removeMakeDepsAfter_1 English    = "Removing unneeded make dependencies..."
removeMakeDepsAfter_1 Japanese   = "あと片付け。必要ないパッケージを削除："
removeMakeDepsAfter_1 Polish     = "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."
removeMakeDepsAfter_1 Croatian   = "Uklanjanje nepotrebnih zavisnosti vezanih uz izgradnju..."
removeMakeDepsAfter_1 Swedish    = "Tar bort obehövda beroenden för `make`..."
removeMakeDepsAfter_1 German     = "Entferne nicht mehr benötigte make Abhängigkeiten..."
removeMakeDepsAfter_1 Spanish    = "Removiendo dependencias make innecesarias..."
removeMakeDepsAfter_1 Portuguese = "Removendo dependências `make` desnecessárias..."
removeMakeDepsAfter_1 French     = "Suppression des dépendances inutiles…"
removeMakeDepsAfter_1 Russian    = "Удаление ненужных зависимостей make..."
removeMakeDepsAfter_1 Italian    = "Rimuovo le dipendenze di compilazione..."
removeMakeDepsAfter_1 Serbian    = "Уклањање непотребних зависности за изградњу..."

----------------------------
-- Aura/Commands/B functions
----------------------------
-- NEEDS TRANSLATION
cleanStates_1 :: Language -> String
cleanStates_1 Japanese = "入力は数字ではない。"
cleanStates_1 Croatian = "Unos ne predstavlja broj."
cleanStates_1 Serbian  = "Улаз није валидан број."
cleanStates_1 _        = "Input isn't a valid number."

-- NEEDS TRANSLATION
cleanStates_2 :: Int -> Language -> String
cleanStates_2 n Japanese = bt (show n) ++ "個のパッケージ状態記録だけが残される。その他削除？"
cleanStates_2 n Croatian = bt (show n) ++ " stanja paketa će biti zadržano. Ukloniti ostatak?"
cleanStates_2 n Serbian  = bt (show n) ++ " стања пакета ће бити сачувано. Уклонити остатак?"
cleanStates_2 n _        = bt (show n) ++ " package states will be kept. Remove the rest?"

-- NEEDS TRANSLATION
cleanStates_3 :: Language -> String
cleanStates_3 Japanese = "何も削除しないで終了。"
cleanStates_3 Croatian = "Nijedno stanje paketa nije uklonjeno."
cleanStates_3 Serbian  = "Ниједно стање пакета није уклоњено."
cleanStates_3 _        = "No package states were removed."

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
getDowngradeChoice_1 p French     = "Quelle version de " ++ bt p ++ " voulez-vous ?"
getDowngradeChoice_1 p Russian    = "Какую версию " ++ bt p ++ " вы хотите?"
getDowngradeChoice_1 p Italian    = "Quale versione di " ++ bt p ++ " preferisci?"
getDowngradeChoice_1 p Serbian    = "Коју верзију " ++ bt p ++ "-а желите?"

backupCache_1 :: Language -> String
backupCache_1 English    = "No backup location given."
backupCache_1 Japanese   = "バックアップ先を入力してください。"
backupCache_1 Polish     = "Nie podano lokalizacji kopii zapasowych."
backupCache_1 Croatian   = "Lokacija sigurnosne kopije nije specifirana."
backupCache_1 Swedish    = "Ingen backup-plats specifierad."
backupCache_1 German     = "Kein Sicherungsort angegeben."
backupCache_1 Spanish    = "No se ha especificado localización para la copia de seguridad."
backupCache_1 Portuguese = "Ainda não disse onde quer guardar o backup..."
backupCache_1 French     = "Aucun lieu pour les copies de sauvegardes n'est spécifié."
backupCache_1 Russian    = "Не указан путь к бэкапу."
backupCache_1 Italian    = "Path per il salvataggio non specificato."
backupCache_1 Serbian    = "Није дата путања ка бекапу."

backupCache_2 :: Language -> String
backupCache_2 English    = "You must be root to backup the cache."
backupCache_2 Japanese   = "rootじゃないとバックアップはできない。"
backupCache_2 Polish     = "Musisz być rootem, by zrobić kopię zapasową pamięci podręcznej."
backupCache_2 Croatian   = "Za stvaranje sigurnosne kopije skladišta potrebne su root ovlasti."
backupCache_2 Swedish    = "Du måste vara root för att ta backup på cache-filer."
backupCache_2 German     = "Sie müssen root sein um den Cache zu sichern."
backupCache_2 Spanish    = "Debes ser root para hacer una copia de seguridad de la caché."
backupCache_2 Portuguese = "Precisa ser root para fazer um backup do cache."
backupCache_2 French     = "Vous devez être `root` pour faire une copie de sauvegarde du cache."
backupCache_2 Russian    = "Чтобы создать бэкап кэша, вы должны быть рутом"
backupCache_2 Italian    = "Devi essere root per salvare la cache."
backupCache_2 Serbian    = "Морате бити root да бисте бекаповали кеш."

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

backupCache_5 :: Int -> Language -> String
backupCache_5 n English    = "Package files to backup: " ++ bt (show n)
backupCache_5 n Japanese   = "パッケージのファイル数：" ++ bt (show n)
backupCache_5 n Polish     = "Pliki będące częścią kopii zapasowej: " ++ bt (show n)
backupCache_5 n Croatian   = "Datoteke koje su dio sigurnosne kopije: " ++ bt (show n)
backupCache_5 n Swedish    = "Paket-filer att ta backup på: " ++ bt (show n)
backupCache_5 n German     = "Zu sichernde Paketdateien: " ++ bt (show n)
backupCache_5 n Spanish    = "Ficheros de paquetes de los que se hará copia de seguridad: " ++ bt (show n)
backupCache_5 n Portuguese = "Arquivos de pacotes para backup: " ++ bt (show n)
backupCache_5 n French     = "Copie de sauvegarde des fichiers de paquets suivants : " ++ bt (show n)
backupCache_5 n Russian    = "Упакуйте файлы для бэкапа: " ++ bt (show n)
backupCache_5 n Italian    = "File del pacchetto da salvare: " ++ bt (show n)
backupCache_5 n Serbian    = "Датотеке за бекап: " ++ bt (show n)

backupCache_6 :: Language -> String
backupCache_6 English    = "Proceed with backup?"
backupCache_6 Japanese   = "バックアップを実行する？"
backupCache_6 Polish     = "Kontynuować tworzenie kopii zapasowej?"
backupCache_6 Croatian   = "Nastavi sa stvaranjem sigurnosne kopije?"
backupCache_6 Swedish    = "Fortsätt med backup?"
backupCache_6 German     = "Sicherung fortsetzen?"
backupCache_6 Spanish    = "¿Proceder con la copia de seguridad?"
backupCache_6 Portuguese = "Proceder com o backup?"
backupCache_6 French     = "Procéder à la copie de sauvegarde ?"
backupCache_6 Russian    = "Продолжить создание бэкапа?"
backupCache_6 Italian    = "Procedere con il salvataggio?"
backupCache_6 Serbian    = "Наставити бекаповање?"

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

backupCache_8 :: Language -> String
backupCache_8 English    = "Backing up. This may take a few minutes..."
backupCache_8 Japanese   = "バックアップ中。数分かかるかもしれない。"
backupCache_8 Polish     = "Tworzenie kopii zapasowej. To może potrwać kilka minut..."
backupCache_8 Croatian   = "Stvaranje sigurnosne kopije. Ovo može potrajati nekoliko minuta..."
backupCache_8 Swedish    = "Tar backup. Det här kan ta ett tag..."
backupCache_8 German     = "Sichere. Dies kann ein paar Minuten dauern..."
backupCache_8 Spanish    = "Haciendo copia de seguridad. Esto puede tardar unos minutos..."
backupCache_8 Portuguese = "Efetuando backup. Isso pode levar alguns minutos..."
backupCache_8 French     = "Copie de sauvegarde en cours. Ceci peut prendre quelques minutes…"
backupCache_8 Russian    = "Создается бэкап. Это может занять пару минут..."
backupCache_8 Italian    = "Salvataggio. Questo potrebbe richiedere qualche minuto..."
backupCache_8 Serbian    = "Бекапујем. Ово може да потраје пар минута..."

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

cleanCache_1 :: Language -> String
cleanCache_1 English    = "Invalid number given."
cleanCache_1 Japanese   = "入力の数字は適切ではない。"
cleanCache_1 Polish     = "Nieprawidłowa liczba."
cleanCache_1 Croatian   = "Pogrešan broj."
cleanCache_1 Swedish    = "Ogiltigt nummer specifierat."
cleanCache_1 German     = "Ungültige Nummer gegeben."
cleanCache_1 Spanish    = "Número inválido."
cleanCache_1 Portuguese = "Número inválido."
cleanCache_1 French     = "Nombre invalide."
cleanCache_1 Russian    = "Дано невалидное число."
cleanCache_1 Italian    = "Numero non valido."
cleanCache_1 Serbian    = "Број није валидан."

cleanCache_2 :: Language -> String
cleanCache_2 English    = "This will delete the ENTIRE package cache."
cleanCache_2 Japanese   = "パッケージ・キャッシュは完全に削除される。"
cleanCache_2 Polish     = "To usunie WSZYSTKIE pakiety z pamięci podręcznej."
cleanCache_2 Croatian   = "Ovo će izbrisati CIJELO skladište paketa."
cleanCache_2 Swedish    = "Detta kommer ta bort HELA paket-cachen."
cleanCache_2 German     = "Das wird den GESAMTEN Paketcache leeren."
cleanCache_2 Spanish    = "Esto eliminará POR COMPLETO la caché de paquetes."
cleanCache_2 Portuguese = "Isso eliminara TODOS OS PACOTES do cache."
cleanCache_2 French     = "Ceci va COMPLÉTEMENT supprimer le cache des paquets."
cleanCache_2 Russian    = "Это действие ВСЕЦЕЛО уничтожит кэш пакетов."
cleanCache_2 Italian    = "Questo cancellera l'INTERA cache dei pacchetti."
cleanCache_2 Serbian    = "Ово ће избрисати ЦЕО кеш пакета."

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

cleanCache_4 :: Language -> String
cleanCache_4 English    = "The rest will be deleted. Okay?"
cleanCache_4 Japanese   = "残りは全部削除される。承知する？"
cleanCache_4 Polish     = "Wszystko inne zostanie usunięte. Na pewno?"
cleanCache_4 Croatian   =  "Ostali paketi će biti izbrisani iz skladišta. Jeste li sigurni?"
cleanCache_4 Swedish    = "Resten kommer att tas bort. Är det OK?"
cleanCache_4 German     = "Der Rest wird gelöscht. Ist das OK?"
cleanCache_4 Spanish    = "El resto se eliminará. ¿OK?"
cleanCache_4 Portuguese = "O resto será deletado. OK?"
cleanCache_4 French     = "Le reste sera supprimé. Êtes-vous d'accord ?"
cleanCache_4 Russian    = "Всё остальное будет удалено. Годится?"
cleanCache_4 Italian    = "Il resto verrà mantenuto. Continuare?"
cleanCache_4 Serbian    = "Остатак ће бити избрисан. Да ли је то у реду?"

cleanCache_5 :: Language -> String
cleanCache_5 English    = "Cache cleaning manually aborted."
cleanCache_5 Japanese   = "削除の続行は意図的に阻止された。"
cleanCache_5 Polish     = "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."
cleanCache_5 Croatian   = "Čišćenje skladišta paketa prekinuto od strane korisnika."
cleanCache_5 Swedish    = "Cache-rensning avbröts manuellt."
cleanCache_5 German     = "Säubern des Caches durch Benutzer abgebrochen."
cleanCache_5 Spanish    = "Limpieza de la caché abortada manualmente."
cleanCache_5 Portuguese = "Limpeza do cache abortada manualmente."
cleanCache_5 French     = "Le nettoyage du cache a été arrêté manuellement."
cleanCache_5 Russian    = "Очистка кэша прервана пользователем."
cleanCache_5 Italian    = "Pulitura manuale della cache interrotta."
cleanCache_5 Serbian    = "Чишћење кеша је ручно прекинуто."

cleanCache_6 :: Language -> String
cleanCache_6 English    = "Cleaning package cache..."
cleanCache_6 Japanese   = "パッケージ・キャッシュを掃除中・・・"
cleanCache_6 Polish     = "Czyszczenie pamięci podręcznej..."
cleanCache_6 Croatian   = "Čišćenje skladišta paketa..."
cleanCache_6 Swedish    = "Rensar paket-cache..."
cleanCache_6 German     = "Säubere Paketcache..."
cleanCache_6 Spanish    = "Limpiando la caché de paquetes..."
cleanCache_6 Portuguese = "Limpando cache de pacotes..."
cleanCache_6 French     = "Nettoyage du cache des paquets…"
cleanCache_6 Russian    = "Очистка кэша пакета..."
cleanCache_6 Italian    = "Ripulisco la cache..."
cleanCache_6 Serbian    = "Чишћење кеша..."

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
logLookUpFields French     = [ "Paquet","Première installation","Mises à jours ","Actions récentes" ]
logLookUpFields Russian    = [ "Пакет","Первая установка","Обновления","Недавние действия" ]
logLookUpFields Italian    = [ "Package","Prima installazione","Upgrades","Azioni recenti" ]
logLookUpFields Serbian    = [ "Пакет","Прва инсталација","Ажурирања","Недавне радње" ]

reportNotInLog_1 :: Language -> String
reportNotInLog_1 English    = "These have not appeared in the log file:"
reportNotInLog_1 Japanese   = "logファイルには出ていない："
reportNotInLog_1 Polish     = "Tych pakietów nie ma w dzienniku:"
reportNotInLog_1 Croatian   = "Ovih paketa nema u dnevniku:"
reportNotInLog_1 Swedish    = "Dessa har inte framkommit i loggfiler:"
reportNotInLog_1 German     = "Diese sind nicht in der Logdatei aufgetaucht:"
reportNotInLog_1 Spanish    = "Estos no aparecen en el fichero log:"
reportNotInLog_1 Portuguese = "Os seguintes não apareceram no log de arquivo:"
reportNotInLog_1 French     = "Ceci n'apparaît pas des les journaux (log) :"
reportNotInLog_1 Russian    = "Следующих пакетов нет в лог-файле:"
reportNotInLog_1 Italian    = "Questo non apparirà nei file di log;"
reportNotInLog_1 Serbian    = "Ови пакети се не спомињу у дневнику:"

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

absSy :: Language -> String
absSy _ = green "Perform actions involving the ABS tree.\n" ++ "Default action builds and installs from ABS"

-- NEEDS TRANSLATION
saveS :: Language -> String
saveS Japanese = yellow "パッケージの設置状態に関する処理\n" ++ "デフォルトでインストール状態を保存する。"
saveS Croatian = yellow "Upravlja spremanjem i vraćanjem globalnog stanja paketa.\n" ++ "Prešutna (default) radnja je spremanje trenutnog stanja paketa."
saveS Serbian  = yellow "Управља чувањем и враћањем глобалног стања пакета.\n" ++ "Уобичајена радња чува тренутно стање."
saveS _        = yellow "Manage the [S]aving and restoring of the global package state.\n" ++ "Default action saves this state."

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

-------------------------------
-- Aura/AUR functions
-------------------------------
-- NEEDS TRANSLATION
getAURPkgInfo_1 :: Language -> String
getAURPkgInfo_1 Japanese = "AURのAPIに繋げなかった。ネット接続状態を確認して下さい。"
getAURPkgInfo_1 Croatian = "Pristup AUR-u nije uspio. Provjerite vašu vezu."
getAURPkgInfo_1 Serbian  = "Приступ AUR-у није успео. Проверите вашу везу."
getAURPkgInfo_1 _        = "AUR API lookup failed. Please check your connection."

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
outOfDateMsg True  French     = red "Périmé !"
outOfDateMsg False French     = green "À jour"
outOfDateMsg True  Russian    = red "Устарел!"
outOfDateMsg False Russian    = green "Новейший"
outOfDateMsg True  Italian    = red "Out of Date!"
outOfDateMsg False Italian    = green "Aggiornato"
outOfDateMsg True  Serbian    = red "Застарео!"
outOfDateMsg False Serbian    = green "Ажуран"

-----------------------
-- Aura/ABS functions
-----------------------
-- NEEDS TRANSLATION
absInfoFields :: Language -> [String]
absInfoFields _ = [ "Repository","Name","Version","ABS path","Description" ]

-----------------------
-- Aura/State functions
-----------------------
-- NEEDS TRANSLATION
saveState_1 :: Language -> String
saveState_1 Japanese = "現在パッケージ状態保存完了。"
saveState_1 Croatian = "Stanje paketa spremljeno."
saveState_1 Serbian  = "Сачувано стање пакета."
saveState_1 _        = "Saved package state."

-- NEEDS TRANSLATION
restoreState_1 :: Language -> String
restoreState_1 Japanese = "対象バージョンがないパッケージ："
restoreState_1 Croatian = "Tražene stare verzije nisu dostupne za:"
restoreState_1 Serbian  = "Захтеване старе верзије нису доступне за:"
restoreState_1 _        = "Requested downgrade versions not available for:"

-- NEEDS TRANSLATION
reinstallAndRemove_1 :: Language -> String
reinstallAndRemove_1 Japanese = "パッケージを変更する必要ない。"
reinstallAndRemove_1 Croatian = "Nema paketa kojima su potrebne izmjene."
reinstallAndRemove_1 Serbian  = "Ниједан пакет не захтева измене."
reinstallAndRemove_1 _        = "No packages need changing."

--------------------------------------
-- Aura/Settings/BadPackages functions
--------------------------------------
-- NEEDS TRANSLATION
circDep_1 :: String -> Language -> String
circDep_1 p Japanese = bt p ++ "と互いに従属している。"
circDep_1 p Croatian = "Ima kružnu zavisnost sa " ++ bt p ++ "."
circDep_1 p Serbian  = "Има кружну зависност са " ++ bt p ++ "."
circDep_1 p _        = "Has a circular dependency with " ++ bt p ++ "."

-- NEEDS TRANSLATION
bashisms_1 :: Language -> String
bashisms_1 Japanese = "PKGBUILDのBashコードが複雑すぎる。"
bashisms_1 Croatian = "Previše „bash-izama“ u PKGBUILD-u."
bashisms_1 Serbian  = "Превише „bash-изама“ у PKGBUILD-у."
bashisms_1 _        = "Too many bashisms in PKGBUILD."

------------------------
-- Aura/Pacman functions
------------------------
-- NEEDS TRANSLATION
pacmanFailure_1 :: Language -> String
pacmanFailure_1 Japanese = "入力を確認して下さい。"
pacmanFailure_1 Croatian = "Provjerite vaš unos."
pacmanFailure_1 Serbian  = "Молим Вас, проверите ваш унос."
pacmanFailure_1 _        = "Please check your input."

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
hotEdit_1 p French     = "Voulez-vous éditer le PKGBUILD de " ++ bt p ++ " ?"
hotEdit_1 p Russian    = "Отредактировать PKGBUILD пакета " ++ bt p ++ "?"
hotEdit_1 p Italian    = "Volete modificare il PKGBUILD di " ++ bt p ++ "?"
hotEdit_1 p Serbian    = "Желите ли да измените PKGBUILD за " ++ bt p ++ "?"

customizepkg_1 :: Language -> String
customizepkg_1 Japanese = bt "customizepkg" ++ "はインストールされていない。"
customizepkg_1 Croatian = bt "customizepkg" ++ "nije instaliran."
customizepkg_1 _        = bt "customizepkg" ++ "isn't installed."
