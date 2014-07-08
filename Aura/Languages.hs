{-# LANGUAGE LambdaCase #-}

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
languageNames = \case 
    Japanese   -> [ "ポーランド語","クロアチア語","スウェーデン語","ドイツ語","スペイン語","ポルトガル語","フランス語","ロシア語", "", "", "" ]
    Polish     -> [ "polski","chorwacki","szwedzki","niemiecki","hiszpański","portugalski","francuski","rosyjski", "", "", "" ]
    Croatian   -> [ "poljski","hrvatski","švedski","njemački","španjolski","portugalski","francuski","ruski", "talijanski", "srpski", "norveški" ]
    Swedish    -> [ "polska","kroatiska","svenska","tyska","spanska","portugisiska", "", "", "" ]
    German     -> [ "Polnisch","Kroatisch","Schwedisch","Deutsch","Spanisch","Portugiesisch", "", "", "" ]
    Spanish    -> [ "Polaco","Croata","Sueco","Alemán","Español","Portugués", "", "", "" ]
    Portuguese -> [ "Polonês","Croata","Sueco","Alemão","Espanhol","Português", "", "", "" ]
    French     -> [ "Polonais","Croate","Suédois","Allemand","Espagnol","Portugais", "Français", "Russe", "Italien", "Serbe", "Norvégien" ]
    Russian    -> [ "Польский","Хорватский","Шведский","Немецкий","Испанский","Португальский", "Русский", "Итальянский", "Сербский", "Норвежский" ]
    Italian    -> [ "Polacco", "Croato", "Svedese", "Tedesco", "Spagnolo", "Portoghese", "Francese", "Russo", "Italiano", "", "" ]
    Serbian    -> [ "Пољски","Хрватски","Шведски","Немачки","Шпански","Португалски","Француски","Руски","Италијански","Српски", "" ]
    Norwegian  -> [ "Polsk","Kroatisk","Svensk","Tysk","Spansk","Portugisisk","Fransk","Russisk","Italiensk","Serbisk","Norsk" ]
    _          -> [ "Polish","Croatian","Swedish","German","Spanish","Portuguese","French","Russian", "Italian", "Serbian", "Norwegian" ]

translatorMsgTitle :: Language -> String
translatorMsgTitle = \case
    Japanese   -> "Auraの翻訳者："
    Polish     -> "Tłumacze Aury:"
    Croatian   -> "Aura Prevoditelji:"
    Swedish    -> "Aura Översättare:"
    German     -> "Aura Übersetzer:"
    Spanish    -> "Traductores de Aura:"
    Portuguese -> "Tradutores de Aura:"
    French     -> "Traducteurs d'Aura:"
    Russian    -> "Переводчики Aura:"
    Italian    -> "Traduttori di Aura:"
    Serbian    -> "Преводиоци Аура:"
    Norwegian  -> "Aura Oversettere:"
    .          -> "Aura Translators:"

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
langFromEnv = \case
    "ja" -> Japanese
    "pl" -> Polish
    "hr" -> Croatian
    "sv" -> Swedish
    "de" -> German
    "es" -> Spanish
    "pt" -> Portuguese
    "fr" -> French
    "ru" -> Russian
    "it" -> Italian
    "sr" -> Serbian
    "nb" -> Norwegian
    _    -> English

----------------------
-- Aura/Core functions
----------------------
-- NEEDS TRANSLATION
checkDBLock_1 :: Language -> String
checkDBLock_1 = \case
    Japanese   -> "パッケージデータベースが今閉鎖状態。開放したらキーを押して続行をどうぞ。"
    Croatian   -> "Baza paketa je zaključana. Kad se otključa, pritisnite enter da biste nastavili."
    German     -> "The Paketdatenbank ist gesperrt. Drücken Sie Enter wenn sie entsperrt ist um fortzufahren."
    Norwegian  -> "Pakkedatabasen er låst. Trykk enter når den er åpnet for å fortsette."
    French     -> "La base de données des paquets est bloquée. Appuyez sur enter pour continuer."
    Portuguese -> "Banco de dados de pacote travado. Aperte 'enter' quando estivier detravado para poder continuar."
    Russian    -> "База данных пакетов заблокирована. Нажмите \"Ввод\", когда она разблокируется, чтобы продолжить."
    _          -> "The package database is locked. Press enter when it's unlocked to continue."

-- Packages should not be built if the user is logged in as root!
trueRoot_1 :: Language -> String
trueRoot_1 = \case
    Japanese   -> "本当のrootユーザーとしてパッケージを作成するのが危険。続行？"
    Polish     -> "Nigdy nie powinieneś budować pakietów jako root. Na pewno kontynuować?"
    Croatian   -> "Pakete ne bi trebalo graditi sa root korisničkim računom. Nastaviti?"
    Swedish    -> "Det är starkt rekommenderat att INTE vara inloggad som root när man bygger paket. Vill du fortsätta ändå?"
    German     -> "Sie sollten niemals Pakete als der echte root Nutzer bauen. Sind sie sicher, dass Sie dies tun wollen?"
    Spanish    -> "Nunca deberías construir paquetes como root real. ¿Estás de acuerdo con esto?"
    Portuguese -> "Não deveria compilar pacotes como o root de fato. Ainda assim, deseja prosseguir?"
    French     -> "Il n'est pas recommandé de construire des paquets avec le compte root. Voulez-vous continuer ?"
    Russian    -> "Вам никогда не следует собирать пакеты под настоящим рутом. Договорились?"
    Italian    -> "Non si dovrebbero compilare pacchetti come root. Volete Continuare?"
    Serbian    -> "Не би требало градити пакете са правим root овлашћењима. Желите ли наставити?"
    Norwegian  -> "Du bør aldri bygge pakker som root. Er du helt sikker på at du vil gjøre dette?"
    _          -> "You should never build packages as the true root. Are you okay with this?"

-- This is for when the user decides to refrain from building afterall.
trueRoot_2 :: Language -> String
trueRoot_2 = \case
    Japanese   -> "よしよし。"
    Polish     -> "Postąpiłeś słusznie."
    Croatian   -> "Ispravno ste postupili."
    Swedish    -> "Phew."
    German     -> "Eine weise Entscheidung."
    Spanish    -> "Has tomado la decision correcta."
    Portuguese -> "Ainda bem que tem juízo!"
    French     -> "C'est la bonne décision."
    Russian    -> "Вы выбрали православный путь."
    Italian    -> "Hai fatto la cosa giusta."
    Serbian    -> "Исправно сте поступили."
    Norwegian  -> "Du gjør det rette."
    _          -> "You’ve done the right thing."

mustBeRoot_1 :: Language -> String
mustBeRoot_1 = let sudo = bt "sudo" in \case 
    Japanese   -> sudo ++ "を使わないとそれができない！"
    Polish     -> "Musisz użyć " ++ sudo ++ ", żeby to zrobić."
    Croatian   -> "Morate koristiti" ++ sudo ++ "za ovu radnju."
    Swedish    -> "Du måste använda " ++ sudo ++ " för det."
    German     -> "Sie müssen dafür " ++ sudo ++ " benutzen."
    Spanish    -> "Tienes que utilizar " ++ sudo ++ " para eso."
    Portuguese -> "Utilize " ++ sudo ++ "para isso."
    French     -> "Vous devez utiliser " ++ sudo ++ " pour ça."
    Russian    -> "Необходимо использовать " ++ sudo ++ " для этого."
    Italian    -> "È necessario utilizzare " ++ sudo ++ " per questo."
    Serbian    -> "Морате да користите " ++ sudo ++ " за ову радњу."
    Norwegian  -> "Du må bruke " ++ sudo ++ " for det."
    _          -> "You have to use " ++ sudo ++ " for that."

-----------------------
-- Aura/Build functions
-----------------------
buildPackages_1 :: String -> Language -> String
buildPackages_1 p' = let p = bt p' in \case
    Japanese   -> p ++ "を作成中・・・"
    Polish     -> "Budowanie " ++ p ++ "..."
    Croatian   -> "Gradim " ++ p ++ "..."
    Swedish    -> "Bygger paket " ++ p ++ "..."
    German     -> "Baue Paket " ++ p ++ "..."
    Spanish    -> "Construyendo " ++ p ++ "..."
    Portuguese -> "Compilando " ++ p ++ "..."
    French     -> "Compilation de " ++ p ++ "..."
    Russian    -> "Сборка " ++ p ++ "..."
    Italian    -> "Compilazione di " ++ p ++ "..."
    Serbian    -> "Градим " ++ p ++ "..."
    Norwegian  -> "Bygger " ++ p ++ "..."
    _          -> "Building " ++ p ++ "..."

buildFail_1 :: String -> Language -> String
buildFail_1 p' = let p = bt p' in \case
    Japanese   -> p ++ "の作成は失敗したようだ。"
    Polish     -> "Budowanie " ++ p ++ " zakończyło się niepowodzeniem."
    Croatian   -> "Izgradnja " ++ p ++ " nije uspjela."
    Swedish    -> "Det gick inte att bygga paketet " ++ p ++ "."
    German     -> "Bauen von " ++ p ++ " ist fehlgeschlagen."
    Spanish    -> "La construcción de " ++ p ++ " ha fallado."
    Portuguese -> "Falha na compilação do pacote " ++ p ++ "."
    French     -> "Bon, la compilation de " ++ p ++ " a échouée."
    Russian    -> "Что ж, сборка " ++ p ++ " не удалась."
    Italian    -> "La compilazione di " ++ p ++ "è fallita."
    Serbian    -> "Изградња пакета " ++ p ++ " није успела."
    Norwegian  -> "Bygging av " ++ p ++ " feilet."
    _          -> "Well, building " ++ p ++ " failed."

buildFail_2 :: Language -> String
buildFail_2 = \case 
    Japanese   -> "ちなみに下記のパッケージも作成されなかった："
    Polish     -> "Dodatkowo, następujące pakiety nie zostały zbudowane:"
    Croatian   -> "Osim toga, ni sljedeće nije izgrađeno:"
    Swedish    -> "Det gick heller inte att bygga följande paket:"
    German     -> "Die folgenden Pakete wurden zusätzlich nicht gebaut:"
    Spanish    -> "Los siguientes paquetes no se han construido:"
    Portuguese -> "Os pacotes a seguir não foram compilados:"
    French     -> "En outre, les paquets suivants n'ont pu être compilés :"
    Russian    -> "К тому же следующие пакеты не были собраны:"
    Italian    -> "Inoltre non è stato possibile cotruire i seguenti pacchetti:"
    Serbian    -> "Такође, ни следећи пакети нису изграђени::"
    Norwegian  -> "Det gikk heller ikke an å bygge følgende:"
    _          -> "Also, the following weren’t built:"

buildFail_3 :: Language -> String
buildFail_3 = \case
    Japanese   -> "しかし、以下のパッケージファイルは無事作成された："
    Polish     -> "Następujące pakiety zostały zbudowane pomyślnie:"
    Croatian   -> "Neki paketi su ipak uspješno izgrađeni:"
    Swedish    -> "Vissa paket kanske har byggts ordentligt (Osäker)."
    German     -> "Diese Pakete wurden wiederrum erfolgreich gebaut:"
    Spanish    -> "Sin embargo, los siguientes paquetes se han construido:"
    Portuguese -> "Entretanto, os seguintes pacotes compilaram com sucesso:"
    French     -> "Cependant, les paquets suivants ont été compilés avec succès :"
    Russian    -> "Однако эти пакеты были успешно собраны:"
    Italian    -> "Comunque questi pacchetti sono stato compilati con successo:"
    Serbian    -> "Међутим, ови пакети су успешно изграђени:"
    Norwegian  -> "Heldigvis ble de følgende pakkene bygd:"
    _          -> "However, these packages were successfully built:"

buildFail_4 :: Language -> String
buildFail_4 = \case
    Japanese   -> "できたやつのインストールを続行する？"
    Polish     -> "Czy chcesz je zainstalować?"
    Croatian   -> "Želite li ih instalirati?"
    Swedish    -> "Vill du installera dem?"
    German     -> "Möchten sie diese installieren?"
    Spanish    -> "¿Te gustaría instalarlos?"
    Portuguese -> "Gostaria de instalá-los?"
    French     -> "Voulez-vous les installer ?"
    Russian    -> "Желаете ли вы их установить?"
    Italian    -> "Volete installarli?"
    Serbian    -> "Желите ли их инсталирати?"
    Norwegian  -> "Vil du installere dem?"
    _          -> "Would you like to install them?"

buildFail_5 :: Language -> String
buildFail_5 = \case
    Japanese   -> "パッケージ作成は失敗した。"
    Polish     -> "Budowanie nie powiodło się."
    Croatian   -> "Izgradnja nije uspjela."
    Swedish    -> "Gick inte att bygga paket."
    German     -> "Bauen fehlgeschlagen."
    Spanish    -> "La construcción falló."
    Portuguese -> "Falha na compilação."
    French     -> "Compilation échouée."
    Russian    -> "Сборка не удалась."
    Italian    -> "Compilazione fallita."
    Serbian    -> "Изградња пакета није успела."
    Norwegian  -> "Bygging feilet."
    _          -> "Building failed."

-- NEEDS TRANSLATION
buildFail_6 :: Language -> String
buildFail_6 = \case
    Japanese  -> "それでも続行？"
    Croatian  -> "Želite li svejedno nastaviti?"
    Norwegian -> "Vil du fortsette likevel?"
    Italian   -> "Vuoi continuare comunque?"
    French    -> "Voulez-vous tout de même continuer ?"
    Russian   -> "Продолжить не смотря ни на что?"
    _         -> "Would you like to continue anyway?"

displayBuildErrors_1 :: Language -> String
displayBuildErrors_1 = \case
    Japanese   -> "抑えていたmakepkgの出力を受け取る用意・・・"
    Polish     -> "Wyjście makepkg zostanie wypisane za "
    Croatian   -> "Zapisujem makepkg ispis u "
    Swedish    -> "Dumpar makepkgs utskrift i "
    German     -> "Schreibe makepkg Ausgabe in "
    Spanish    -> "Volcando la salida de makepkg en "
    Portuguese -> "Despejando a saída do makepkg em "
    French     -> "Redirection de la sortie de makepkg vers "
    Russian    -> "Вывод makepkg записывается в "
    Italian    -> "Salvataggio dell'output di makepkg in "
    Serbian    -> "Уписујем излаз makepkg-а у "
    Norwegian  -> "Dumper makepkg's utskrift i "
    _          -> "Dumping makepkg output in "

------------------------------
-- Aura/Dependencies functions
------------------------------
-- Is this still used?
getDepsToInstall_1 :: Language -> String
getDepsToInstall_1 = \case
    Japanese   -> "パッケージは一つも指摘されていない。"
    Polish     -> "Nie podano pakietów z AUR do zainstalowania."
    Croatian   -> "Nijedan AUR paket nije specificiran za instalaciju."
    Swedish    -> "Inga AUR-paket är valda för installation."
    German     -> "Keine AUR Pakete zur Installation vermerkt."
    Spanish    -> "No se han especificado paquetes de AUR para instalar."
    Portuguese -> "Nenhum pacote AUR foi especificado para instalação."
    French     -> "Aucun paquet AUR à installer n'a été spécifié."
    Russian    -> "Пакеты AUR для установки не указаны."
    Italian    -> "Nessun pacchetto di AUR specificato per l'installazione."
    Serbian    -> "Ниједан AUR пакет није специфициран за инсталацију."
    Norwegian  -> "Ingen pakker fra AUR er valgt for installasjon."
    _          -> "No AUR packages specified for install."

getRealPkgConflicts_1 :: String -> String -> String -> Language -> String
getRealPkgConflicts_1 p' r' d' = let p = bt p'; r = bt r'; d = bt d' in \case
    Japanese   -> "パッケージ" ++ p ++ "はバージョン" ++ d ++ "を要するが" ++ "一番最新のバージョンは" ++ r ++ "。"
    Polish     -> "Zależność " ++ p ++ " powinna być w wersji " ++ d ++ ", ale najnowsza wersja to " ++ r ++ "."
    Croatian   -> "Zavisnost " ++ p ++ " zahtjeva verziju " ++ d ++ ", a najnovija dostupna verzija je " ++ r ++ "."
    Swedish    -> "Beroendepaketet " ++ p ++ " kräver version " ++ d ++ "men den senaste versionen är " ++ r ++ "."
    German     -> "Die Abhängigkeit " ++ p ++ " verlangt Version " ++ d ++ "aber die neuste Version ist " ++ r ++ "."
    Spanish    -> "La dependencia " ++ p ++ " duiere la versión " ++ d ++ "pero la versión más reciente es " ++ r ++ "."
    Portuguese -> "A dependência " ++ p ++ " exige a versão " ++ d ++ "mas a versão mais recente é " ++ r ++ "."
    French     -> p ++ " est une dépendance nécessitant la version " ++ d ++ ", mais la plus récente est la version " ++ r ++ "."
    Russian    -> "Зависимость " ++ p ++ " требует версию " ++ d ++ ", однако самой последней версией является " ++ r ++ "."
    Italian    -> "La dipendenza " ++ p ++ " richiede la versione " ++ d ++ "ma la versione disponibile è " ++ r ++ "."
    Serbian    -> "Зависност " ++ p ++ " захтева верзију " ++ d ++ ", али најновија верзија је " ++ r ++ "."
    Norwegian  -> "Avhengigheten " ++ p ++ " krever versjon " ++ d ++", men den nyeste versjonen er " ++ r ++ "."
    _          -> "The dependency " ++ p ++ " demands version " ++ d ++ "but the most recent version is " ++ r ++ "."

getRealPkgConflicts_2 :: String -> Language -> String
getRealPkgConflicts_2 p' = let p = bt p' in \case
    Japanese   -> p ++ "は無視されるパッケージ！`pacman.conf`を参考に。"
    Polish     -> p ++ " jest ignorowany! Sprawdź plik `pacman.conf`."
    Croatian   -> p ++ " je ignoriran paket! Provjerite svoj `pacman.conf`."
    Swedish    -> p ++ " är ett ignorerat paket! Kolla din `pacman.conf`-fil."
    German     -> p ++ " ist ein ignoriertes Paket! Siehe /etc/pacman.conf."
    Spanish    -> "¡" ++ p ++ " es un paquete ignorado! Revisa tu fichero `pacman.conf`."
    Portuguese -> p ++ " é um pacote ignorado conforme configuração em `pacman.conf`!"
    French     -> "Le paquet " ++ p ++ " est ignoré. Vous devriez jeter un œil à votre `pacman.conf`."
    Russian    -> "Пакет " ++ p ++ " игнорируется! Проверьте ваш файл `pacman.conf`."
    Italian    -> p ++ " è un pacchetto ignorato, controllare `pacman.conf`."
    Serbian    -> "Пакет " ++ p ++ " је игнорисан! Видите ваш фајл „pacman.conf“."
    Norwegian  -> p ++ " er en ignorert pakke! Sjekk din `pacman.conf`-fil."
    _          -> p ++ " is an ignored package! See your `pacman.conf` file."

-- NEEDS TRANSLATION
missingPkg_1 :: String -> Language -> String
missingPkg_1 p' = let p = bt p' in \case
    Croatian  -> "Zavisnost  " ++ p ++ " nije pronađena. Pokušajte pronaći paket koji zadovoljava ovu zavisnost."
    Norwegian -> "Avhengigheten " ++ p ++ " ble ikke funnet. Du kan søke etter en pakke som tilfredsstiller avhengigheten."
    Italian   -> "La dipendenza " ++ p ++ " non è stata trovata. Potrebbe essere necessario cercare un pacchetto che possa soddisfarla?"
    French    -> "La dépendance " ++ p ++ " n'a pas pu être trouvée. Il vous faut trouver un paquet pour la satisfaire."
    Russian   -> "Зависимость " ++ p ++ " не найдена. Возможно, вам нужно поискать пакет, чтобы удовлетворить её."
    _         -> "The dependency " ++ p ++ " could not be found. You may need to search for a package to satisfy it."

-----------------
-- aura functions
-----------------
executeOpts_1 :: Language -> String
executeOpts_1 = \case
    Japanese   -> "矛盾しているオプションあり。"
    Polish     -> "Niektóre flagi są w konflikcie ze sobą!"
    Croatian   -> "Neke od danih zastavica nije moguće kombinirati!"
    Swedish    -> "Givna flaggor är i konflikt!"
    German     -> "Gegebene Kommandozeilenflags sind widersprüchlich!"
    Spanish    -> "¡Flags contradictorios!"
    Portuguese -> "Flags conflitantes!"
    French     -> "Arguments contradictoires !"
    Russian    -> "Даны конфликтующие флаги!"
    Italian    -> "Argomenti in conflitto!"
    Serbian    -> "Захтеване опције су контрадикторне!"
    Norwegian  -> "Motstridene flagg er spesifisert!"
    _          -> "Conflicting flags given!"

manpageMsg :: Language -> String
manpageMsg = \case
    Japanese   -> "選択肢の詳しいことは、auraのman pageまで。"
    Polish     -> "W podręczniku man dla aura znajduje się\xa0więcej informacji o opcjach."
    Croatian   -> "Za detalje o opcijama, pogledajte Aura man stranicu."
    Swedish    -> "Hänvisa till auras `man`-sida för detaljerade alternativ."
    German     -> "Lesen Sie die aura man-Seite für Details zu aura Optionen."
    Spanish    -> "Lee la página de manual de aura para detalles sobre las opciones."
    Portuguese -> "Leia a man page do aura para mais detalhes sobre as opções"
    French     -> "Lisez le manuel d'Aura (`man aura`) pour le détail des options."
    Russian    -> "Чтобы узнать подробное описание опций aura, см. мануал."
    Italian    -> "Guardare la man page di Aura per maggiori dettagli sulle opzioni."
    Serbian    -> "За детаље о опцијама, погледајте man страницу Аура."
    Norwegian  -> "Referer til Aura's `man`-side for instillingsdetaljer."
    _          -> "See the aura man page for aura option details."

displayOutputLanguages_1 :: Language -> String
displayOutputLanguages_1 = \case
    Japanese   -> "auraは下記の言語に対応している："
    Polish     -> "Następujące języki są dostępne:"
    Croatian   -> "Dostupni su sljedeći jezici:"
    Swedish    -> "Följande språk är tillängliga:"
    German     -> "Die folgenden Sprachen sind verfügbar:"
    Spanish    -> "Los siguientes idiomas están disponibles:"
    Portuguese -> "Os seguintes idiomas estão disponíveis:"
    French     -> "Les langues suivantes sont disponibles :"
    Russian    -> "Доступны следующие языки:"
    Italian    -> "Sono disponibili le seguenti lingue:"
    Serbian    -> "Доступни су следећи језици:"
    Norwegian  -> "Følgende språk er tilgjengelig:"
    _          -> "The following languages are available:"

----------------------------
-- Aura/Commands/A functions
----------------------------
-- NEEDS TRANSLATION
auraCheck_1 :: Language -> String
auraCheck_1 = \case
    Japanese  -> "Auraアップグレードあり。先にAuraだけを？"
    Croatian  -> "Dostupna je nova verzija Aura. Želite li prvo ažurirati?"
    German    -> "Aura Update verfügbar. Dies zuerst aktualisieren?"
    Norwegian -> "En Aura-oppdatering er tilgjengelig. Oppdater den først?"
    French    -> "Une mise à jour d'Aura est disponible. Voulez-vous la mettre à jour en premier ?"
    _         -> "Aura update available. Update it first?"

install_1 :: Language -> String
install_1 = \case
    Japanese   -> "従属パッケージの確認は以下の理由で失敗した："
    Polish     -> "Sprawdzanie zależności nie powiodło się z następujących powodów:"
    Croatian   -> "Provjera zavisnosti nije uspjela iz sljedećih razloga:"
    Swedish    -> "Beroende-kollen misslyckades pga följande skäl:"
    German     -> "Abhängigkeitsüberprüfung schlug Fehl aus folgenden Gründen:"
    Spanish    -> "La comprobación de dependencias falló por los siguientes motivos:"
    Portuguese -> "Não foi possível checar as dependências pelas seguintes razões:"
    French     -> "La vérification des dépendances a failli pour les raisons suivantes :"
    Russian    -> "Проверка зависимостей не удалась из-за:"
    Italian    -> "Il controllo delle dipendenze è fallito per i seguenti motivi:"
    Serbian    -> "Провера зависности није успела из следећих разлога:"
    Norwegian  -> "Avhengighets-sjekken mislyktes på grunn av følgende:"
    _          -> "Dependency checking failed for these reasons:"

install_2 :: Language -> String
install_2 = \case
    Japanese   -> "適当なパッケージを入力してください。"
    Polish     -> "Nie podano prawidłowych pakietów."
    Croatian   -> "Nije specificiran nijedan ispravan paket."
    Swedish    -> "Inga giltiga paket valda."
    German     -> "Keine gültigen Pakete angegeben."
    Spanish    -> "No se ha especificado ningún paquete válido."
    Portuguese -> "Nenhum pacote válido foi especificado."
    French     -> "Aucun paquet valide n'a été spécifié."
    Russian    -> "Валидные пакеты не указаны."
    Italian    -> "Nessun pacchetto valido specificato."
    Serbian    -> "Ниједан исправан пакет није специфициран."
    Norwegian  -> "Ingen gyldige pakker er valgte."
    _          -> "No valid packages specified."

install_3 :: Language -> String
install_3 = \case
    Japanese   -> "続行？"
    Polish     -> "Kontynuować?"
    Croatian   -> "Nastaviti?"
    Swedish    -> "Fortsätta?"
    German     -> "Fortsetzen?"
    Spanish    -> "¿Continuar?"
    Portuguese -> "Continuar?"
    French     -> "Continuer ?"
    Russian    -> "Продолжить?"
    Italian    -> "Continuare?"
    Serbian    -> "Наставити?"
    Norwegian  -> "Fortsett?"
    _          -> "Continue?"

install_4 :: Language -> String
install_4 = \case
    Japanese   -> "続行は意図的に阻止された。"
    Polish     -> "Instalacja została przerwana przez użytkownika."
    Croatian   -> "Instalacija prekinuta od strane korisnika."
    Swedish    -> "Installationen avbröts manuellt."
    German     -> "Installation durch Benutzer abgebrochen."
    Spanish    -> "Instalación abortada manualmente."
    Portuguese -> "Instalação manual abortada."
    French     -> "Installation manuelle annulée."
    Russian    -> "Пользователь прервал установку."
    Italian    -> "Installazione manuale interrotta."
    Serbian    -> "Инсталација је ручно прекинута."
    Norwegian  -> "Installasjonen ble avbrutt manuelt."
    _          -> "Installation manually aborted."

install_5 :: Language -> String
install_5 = \case
    Japanese   -> "従属パッケージを確認中・・・"
    Polish     -> "Ustalanie zależności..."
    Croatian   -> "Određivanje zavisnosti..."
    Swedish    -> "Avgör beroenden..."
    German     -> "Bestimme Abhängigkeiten..."
    Spanish    -> "Determinando dependencias..."
    Portuguese -> "Determinando as dependências..."
    French     -> "Détermination des dépendances en cours…"
    Russian    -> "Определение зависимостей..."
    Italian    -> "Determinazione dipendenze..."
    Serbian    -> "Утврђивање зависности..."
    Norwegian  -> "Bestemmer avhengigheter..."
    _          -> "Determining dependencies..."

-- NEEDS UPDATE TO REFLECT CHANGED ENGLISH
reportNonPackages_1 :: Language -> String
reportNonPackages_1 = \case
    Japanese   -> "下記はAURパッケージではない："
    Polish     -> "To nie są pakiety:"
    Croatian   -> "Ovo nisu AUR paketi:"
    Swedish    -> "Följande är inte paket:"
    German     -> "Folgende sind keine Pakete:"
    Spanish    -> "Los siguientes no son paquetes:"
    Portuguese -> "Os seguintes não são pacotes:"
    French     -> "Les éléments suivants ne sont pas des paquets AUR :"
    Russian    -> "Ниже указано то, что не является пакетами AUR:"
    Italian    -> "I seguenti pacchetti non sono presenti in AUR:"
    Serbian    -> "Ово нису пакети:"
    Norwegian  -> "Det følgende er ikke AUR-pakker:"
    _          -> "The following are not AUR packages:"

reportIgnoredPackages_1 :: Language -> String
reportIgnoredPackages_1 = \case
    Japanese   -> "下記のパッケージは無視される："
    Polish     -> "Poniższe pakiety zostaną zignorowane:"
    Croatian   -> "Sljedeći paketi će biti ignorirani:"
    Swedish    -> "Följande paket kommer att ignoreras: "
    German     -> "Die folgenden Pakete werden ignoriert:"
    Spanish    -> "Los siguientes paquetes serán ignorados:"
    Portuguese -> "Os seguintes pacotes serão ignorados:"
    French     -> "Les paquets suivants seront ignorés :"
    Russian    -> "Следующие пакеты будут проигнорированы:"
    Italian    -> "I seguenti pacchetti verranno ignorati:"
    Serbian    -> "Следећи пакети ће бити игнорисани:"
    Norwegian  -> "De følgende pakker vil bli ignorert:"
    _          -> "The following packages will be ignored:"

reportPkgsToInstall_1 :: Language -> String
reportPkgsToInstall_1 = \case
    Japanese   -> "Pacmanの従属パッケージ："
    Polish     -> "Zależności z repozytoriów:"
    Croatian   -> "Zavisnosti iz repozitorija:"
    Swedish    -> "Beroenden ifrån lager:"
    German     -> "Abhängigkeiten in den Paketquellen:"
    Spanish    -> "Dependencias en el repositorio:"
    Portuguese -> "Dependências no repositório:"
    French     -> "Dépendances du dépôt :"
    Russian    -> "Зависимости из репозитория:"
    Italian    -> "Dipendenze nei repository:"
    Serbian    -> "Зависности из ризница:"
    Norwegian  -> "Avhengigheter fra depotet:"
    _          -> "Repository dependencies:"

-- NEEDS AN UPDATE
reportPkgsToInstall_2 :: String -> Language -> String
reportPkgsToInstall_2 l = \case
    Japanese  -> l ++ "のパッケージ:"
    Croatian  -> l ++ " Paketi:"
    German    -> l ++ " Pakete:"
    Norwegian -> l ++ " Pakker:"
    Italian   -> l ++ " Pacchetti:"
    French    -> l ++ " Paquets :"
    Russian   -> l ++ " Пакеты:"
    _         -> l ++ " Packages:"

{-}
reportPkgsToInstall_2 :: String -> Language -> String
reportPkgsToInstall_2 l = \case
    Japanese   -> l ++ "の従属パッケージ："
    Polish     -> "Zależności z " ++ l ++ ":"
    Croatian   -> "Zavisnosti iz " ++ l ++ "-a:"
    Swedish    -> "Beroenden ifrån " ++ l ++ ":"
    German     -> "Abhängigkeiten im " ++ l ++ ":"
    Spanish    -> "Dependencias en " ++ l ++ ":"
    Portuguese -> "Dependências no " ++ l ++ ":"
    French     -> "Dépendances " ++ l ++ "\xa0:"
    Russian    -> "Зависимости из " ++ l ++ ":"
    Italian    -> "Dipendenze in " ++ l ++ ":"
    Serbian    -> "Зависности из " ++ l ++ "-а:"
    Norwegian  -> "Avhengigheter fra " ++ l ++ ":"
    _          -> l ++ " dependencies:"

reportPkgsToInstall_3 :: String -> Language -> String
reportPkgsToInstall_3 l = \case
    Japanese   -> "主な" ++ l ++ "パッケージ："
    Polish     -> "Główne pakiety z " ++ l ++ ":"
    Croatian   -> "Glavni " ++ l ++ " paketi:"
    Swedish    -> "Huvudpaket ifrån " ++ l ++ ":"
    German     -> "Hauptpaket aus dem " ++ l ++ ":"
    Spanish    -> "Paquetes principales de " ++ l ++ ":"
    Portuguese -> "Pacotes principais do " ++ l ++ ":"
    French     -> "Principaux paquets " ++ l ++ "\xa0:"
    Russian    -> "Главные пакеты из " ++ l ++ ":"
    Italian    -> "Pacchetto principale di " ++ l ++ ":"
    Serbian    -> "Главни пакети из " ++ l ++ "-а:"
    Norwegian  -> "Hovedpakker fra " ++ l ++ ":"
    _          -> "Main " ++ l ++ " packages:"
-}

-- NEEDS TRANSLATION
reportPkgbuildDiffs_1 :: String -> Language -> String
reportPkgbuildDiffs_1 p' = let p = bt p' in \case
    Japanese  -> p ++ "のPKGBUILDはまだ保存されていない。"
    Polish    -> p ++ " nie ma jeszcze przechowywanego pliku PKGBUILD."
    Croatian  -> p ++ " još nema pohranjen PKGBUILD."
    German    -> p ++ " hat noch keinen gespeicherten PKGBUILD."
    Spanish   -> p ++ " no tiene PKGBUILD todavía."
    French    -> p ++ " n'a pas encore de PKGBUILD enregistré."
    Russian   -> "У " ++ p ++ " ещё нет сохраненного PKGBUILD."
    Italian   -> p ++ " non ci sono PKGBUILD salvati"
    Serbian   -> p ++ " још нема похрањен PKGBUILD."
    Norwegian -> p ++ " har ingen PKGBUILD ennå."
    _         -> p ++ " has no stored PKGBUILD yet."

-- NEEDS TRANSLATION
reportPkgbuildDiffs_2 :: String -> Language -> String
reportPkgbuildDiffs_2 p' = let p = bt p' in \case
    Japanese  -> p ++ "のPKGBUILDは最新。"
    Polish    -> "PKGBUILD pakietu " ++ p ++ " jest aktualny."
    Croatian  -> "PKGBUILD paketa " ++ p ++ " je na najnovijoj verziji."
    German    -> "PKGBUILD von " ++ p ++ " ist aktuell."
    Spanish   -> "El PKGBUILD de " ++ p ++ " está actualizado."
    Russian   -> "PKGBUILD " ++ p ++ " является новейшим."
    French    -> "Le PKGBUILD de " ++ p ++ " est à jour."
    Italian   -> "Il PKGBUILD di " ++ p ++ " è aggiornato."
    Serbian   -> "PKGBUILD пакета " ++ p ++ " је ажуран."
    Norwegian -> p ++ "'s PKGBUILD er oppdatert."
    _         -> p ++ " PKGBUILD is up to date."

-- NEEDS TRANSLATION
reportPkgbuildDiffs_3 :: String -> Language -> String
reportPkgbuildDiffs_3 p' = let p = bt p' in \case
    Japanese  -> p ++ "のPKGBUILD変更報告："
    Polish    -> "Zmiany w PKGBUILD dla " ++ p ++ ":"
    Croatian  -> "Promjene u PKGBUILD-u za " ++ p ++ ":"
    German    -> "PKGBUILD Änderungen von " ++ p ++ ":"
    Spanish   -> "Cambios en el PKGBUILD de " ++ p ++ ":"
    Russian   -> "Изменения, вносимые " ++ p ++ " PKGBUILD:"
    French    -> "Changements du PKGBUILD de " ++ p ++ " :"
    Italian   -> "Cambiamenti nel PKGBUILD di " ++ p ++":"
    Serbian   -> "Промене PKGBUILD-a за " ++ p ++ ":"
    Norwegian -> p ++ "'s endringer i PKGBUILD:"
    _         -> p ++ " PKGBUILD changes:"

-- NEEDS TRANSLATION
reportPkgsToUpgrade_1 :: Language -> String
reportPkgsToUpgrade_1 = \case
    Japanese   -> "アップグレードするAURパッケージ："
    Polish     -> "Pakiety z AUR do zaktualizowania:"
    Croatian   -> "AUR paketi za nadogradnju:"
    Swedish    -> "AUR-paket att uppgradera:"
    German     -> "Zu aktualisierendes AUR Paket:"
    Spanish    -> "Paquetes de AUR a actualizar:"
    Portuguese -> "Pacotes do AUR para atualizar:"
    French     -> "Paquets AUR à mettre à jour :"
    Russian    -> "Пакеты AUR, готовые для обновления:"
    Italian    -> "Pacchetti in AUR da aggiornare:"
    Serbian    -> "Пакети из AUR-а за надоградњу:"
    Norwegian  -> "AUR-pakker å oppgradere:"
    _          -> "AUR Packages to upgrade:"

-- NEEDS UPDATING
reportBadDowngradePkgs_1 :: Language -> String
reportBadDowngradePkgs_1 = \case
    Japanese   -> "このパッケージはキャッシュには入っていないので、格下げはできない。"
    Polish     -> "Poniższe pakeity nie są zainstalowane, i nie mogą być zainstalowane w starszej wersji:"
    Croatian   -> "Sljedeći paketi nisu instalirani te se stoga ne mogu vratiti na stare verzije:"
    Swedish    -> "Följande paket är inte installerade, och kan därför inte bli nergraderade:"
    German     -> "Folgende Pakete sind nicht installiert und können daher nicht downgraded werden:"
    Spanish    -> "Los siguientes paquetes no están instalados, por lo que no se pueden retornar a versiones antiguas:"
    Portuguese -> "Os seguintes pacotes não estão instalados, logo não podem retornar a uma versão anterior:"
    French     -> "Aucune version des paquets suivants n'est présente dans le cache ; ils ne peuvent pas être mis à niveau à une version antérieure :"
    Russian    -> "Следующих пакетов нет в кэше. Следовательно, они не могут быть откачены к старой версии:"
    Italian    -> "I seguenti pacchetti non hanno versioni in cache e non posso essere retrocessi:"
    Serbian    -> "Следећи пакети нису ни инсталирани, те се не могу вратити на старију верзију:"
    Norwegian  -> "Følgende pakker har ingen versjoner i cache, og kan derfor ikke bli nedgradert:"
    _          -> "The following have no versions in the cache, and thus can’t be downgraded:"

upgradeAURPkgs_1 :: Language -> String
upgradeAURPkgs_1 = \case
    Japanese   -> "パッケージ情報をダウンロード中・・・"
    Polish     -> "Pobieranie informacji o pakietach..."
    Croatian   -> "Preuzimanje podataka o paketima..."
    Swedish    -> "Hämtar paketinformation..."
    German     -> "Rufe Paketinformationen ab..."
    Spanish    -> "Obteniendo información de paquetes..."
    Portuguese -> "Obtendo informação dos pacotes..."
    French     -> "Obtention des informations des paquets en cours…"
    Russian    -> "Сборка информации о пакетах..."
    Italian    -> "Ottengo le informazioni del pacchetto..."
    Serbian    -> "Преузимање информација о пакетима..."
    Norwegian  -> "Henter pakkeinformasjon..."
    _          -> "Fetching package information..."

upgradeAURPkgs_2 :: Language -> String
upgradeAURPkgs_2 = \case
    Japanese   -> "バージョンを比較中・・・"
    Polish     -> "Porównywanie wersji pakietów..."
    Croatian   -> "Uspoređivanje verzija paketa..."
    Swedish    -> "Jämför paket-versioner..."
    German     -> "Vergleiche Paketversionen..."
    Spanish    -> "Comparando versiones de paquetes..."
    Portuguese -> "Comparando versões dos pacotes..."
    French     -> "Comparaison des versions des paquets en cours…"
    Russian    -> "Сравнение версий пакетов..."
    Italian    -> "Confronto le ersioni del pacchetto..."
    Serbian    -> "Упоређивање верзија пакета..."
    Norwegian  -> "Sammenligner pakkeversjoner..."
    _          -> "Comparing package versions..."

upgradeAURPkgs_3 :: Language -> String
upgradeAURPkgs_3 = \case
    Japanese   -> "アップグレードは必要ない。"
    Polish     -> "Nie jest wymagana aktualizacja pakietów z AUR."
    Croatian   -> "Svi AUR paketi su ažurirani."
    Swedish    -> "Inga AUR-paketsuppgraderingar behövs."
    German     -> "Keine AUR Paketaktualisierungen notwendig."
    Spanish    -> "No ha sido necesario actualizar paquetes de AUR."
    Portuguese -> "Nenhum pacote do AUR precisa de atualização."
    French     -> "Aucune mise à jour de paquet AUR n'est nécessaire."
    Russian    -> "Обновление пакетов из AUR не требуется."
    Italian    -> "Non è necessario aggiornare pacchetti di AUR."
    Serbian    -> "Ажурирање пакета из AUR-а није потребно."
    Norwegian  -> "Ingen pakkeoppgradering fra AUR nødvendig."
    _          -> "No AUR package upgrades necessary."

downloadTarballs_1 :: String -> Language -> String
downloadTarballs_1 p' = let p = bt p' in \case
    Japanese   -> p ++ "のソースコードのターボールをダウンロード中・・・"
    Polish     -> "Pobieranie paczki źródłowej " ++ p ++ "..."
    Croatian   -> "Preuzimanje izvornog paketa (tarball) " ++ p ++ "..."
    Swedish    -> "Laddar ner " ++ p ++ " källkodspaket (tarball)..."
    German     -> "Lade Quelltext von " ++ p ++ " (tarball)..."
    Spanish    -> "Descargando los fuentes comprimidos (tarball) de " ++ p ++ " ..."
    Portuguese -> "Baixando os fontes (tarball) de " ++ p ++ " ..."
    French     -> "Téléchargement de l'archive de " ++ p ++ " en cours…"
    Russian    -> "Загрузка исходного архива " ++ p ++ "..."
    Italian    -> "Downlaod del tarball di " ++ p ++ " in corso..."
    Serbian    -> "Преузимање архиве изворног кода за " ++ p ++ "..."
    Norwegian  -> "Laster ned " ++ p ++ " kildekodepakken (tarball)..."
    _          -> "Downloading " ++ p ++ " source tarball..."

displayPkgbuild_1 :: String -> Language -> String
displayPkgbuild_1 p' = let p = bt p' in \case
    Japanese   -> p ++ "は存在しない。"
    Polish     -> p ++ " nie istnieje."
    Croatian   -> p ++ " ne postoji."
    Swedish    -> p ++ " finns inte."
    German     -> p ++ " existiert nicht."
    Spanish    -> p ++ " no existe."
    Portuguese -> p ++ " não existe."
    French     -> p ++ " n'existe pas."
    Russian    -> p ++ " не существует."
    Italian    -> p ++ " inesistente."
    Serbian    -> p ++ " не постоји."
    Norwegian  -> p ++ " finnes ikke."
    _          -> p ++ " does not exist."

removeMakeDepsAfter_1 :: Language -> String
removeMakeDepsAfter_1 = \case
    Japanese   -> "あと片付け。必要ないパッケージを削除："
    Polish     -> "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."
    Croatian   -> "Uklanjanje nepotrebnih zavisnosti vezanih uz izgradnju..."
    Swedish    -> "Tar bort obehövda beroenden för `make`..."
    German     -> "Entferne nicht mehr benötigte make Abhängigkeiten..."
    Spanish    -> "Removiendo dependencias make innecesarias..."
    Portuguese -> "Removendo dependências `make` desnecessárias..."
    French     -> "Suppression des dépendances inutiles…"
    Russian    -> "Удаление ненужных зависимостей make..."
    Italian    -> "Rimuovo le dipendenze di compilazione..."
    Serbian    -> "Уклањање непотребних зависности за изградњу..."
    Norwegian  -> "Fjerner unødvendige make-avhengigheter..."
    _          -> "Removing unneeded make dependencies..."

----------------------------
-- Aura/Commands/B functions
----------------------------
-- NEEDS TRANSLATION
cleanStates_1 :: Language -> String
cleanStates_1 = \case
    Japanese  -> "入力は数字ではない。"
    Croatian  -> "Unos ne predstavlja broj."
    German    -> "Eingabe ist keine gültige Zahl."
    Serbian   -> "Улаз није валидан број."
    Norwegian -> "Oppføringen er ikke et gyldig nummer."
    Italian   -> "Non è un numero valido."
    French    -> "La valeur entrée n'est pas un nombre valide."
    Russian   -> "Введенные данные -- не валидное число."
    _         -> "Input isn't a valid number."

-- NEEDS TRANSLATION
cleanStates_2 :: Int -> Language -> String
cleanStates_2 n' = let n = bt (show n') in \case
    Japanese  -> n ++ "個のパッケージ状態記録だけが残される。その他削除？"
    Croatian  -> n ++ " stanja paketa će biti zadržano. Ukloniti ostatak?"
    German    -> n ++ " Paketzustände werden behalten. Den Rest entfernen?"
    Serbian   -> n ++ " стања пакета ће бити сачувано. Уклонити остатак?"
    Norwegian -> n ++ " pakketilstander vil bli beholdt. Vil du fjerne resten?"
    Italian   -> n ++ " lo stato dei pacchetti sarà mantenuto. Rimuovere i rimanenti?"
    French    -> n ++ " états des paquets vont être conservés. Supprimer le reste ?"
    Russian   -> n ++ " -- столько состояний пакетов будут оставлены. Удалить оставшиеся?"
    _         -> n ++ " package states will be kept. Remove the rest?"

-- NEEDS TRANSLATION
cleanStates_3 :: Language -> String
cleanStates_3 = \case
    Japanese  -> "何も削除しないで終了。"
    Croatian  -> "Nijedno stanje paketa nije uklonjeno."
    German    -> "Keine Paketzustände wurden entfernt."
    Serbian   -> "Ниједно стање пакета није уклоњено."
    Norwegian -> "Ingen pakketilstander ble fjernet."
    Italian   -> "Nessuno stato di pacchetto verrà rimosso."
    French    -> "Aucun état des paquets n'a été supprimé."
    Russian   -> "Состояния пакетов отались нетронутыми."
    _         -> "No package states were removed."

----------------------------
-- Aura/Commands/C functions
----------------------------
getDowngradeChoice_1 :: String -> Language -> String
getDowngradeChoice_1 p' = let p = bt p' in \case
    Japanese   -> p ++ "はどのバージョンにする？"
    Polish     -> "Którą wersję pakietu " ++ p ++ " zainstalować?"
    Croatian   -> "Koju verziju paketa " ++ p ++ " želite?"
    Swedish    -> "Vilken version av " ++ p ++ " vill du ha?"
    German     -> "Welche Version von " ++ p ++ " möchten Sie haben?"
    Spanish    -> "¿Qué versión de " ++ p ++ " quieres?"
    Portuguese -> "Qual versão de " ++ p ++ " deseja?"
    French     -> "Quelle version de " ++ p ++ " voulez-vous ?"
    Russian    -> "Какую версию " ++ p ++ " вы хотите?"
    Italian    -> "Quale versione di " ++ p ++ " preferisci?"
    Serbian    -> "Коју верзију " ++ p ++ "-а желите?"
    Norwegian  -> "Hvilken versjon av " ++ p ++ " vil du ha?"
    _          -> "What version of " ++ p ++ " do you want?"

backupCache_1 :: Language -> String
backupCache_1 = \case
    Japanese   -> "バックアップ先を入力してください。"
    Polish     -> "Nie podano lokalizacji kopii zapasowych."
    Croatian   -> "Lokacija sigurnosne kopije nije specifirana."
    Swedish    -> "Ingen backup-plats specifierad."
    German     -> "Kein Sicherungsort angegeben."
    Spanish    -> "No se ha especificado localización para la copia de seguridad."
    Portuguese -> "Ainda não disse onde quer guardar o backup..."
    French     -> "Aucun chemin pour les copies de sauvegarde n'est spécifié."
    Russian    -> "Не указан путь к бэкапу."
    Italian    -> "Path per il salvataggio non specificato."
    Serbian    -> "Није дата путања ка бекапу."
    Norwegian  -> "Ingen backup-plass spesifisert."
    _          -> "No backup location given."

backupCache_2 :: Language -> String
backupCache_2 = \case
    Japanese   -> "rootじゃないとバックアップはできない。"
    Polish     -> "Musisz być rootem, by zrobić kopię\xa0zapasową pamięci podręcznej."
    Croatian   -> "Za stvaranje sigurnosne kopije cache-a potrebne su root ovlasti."
    Swedish    -> "Du måste vara root för att ta backup på cache-filer."
    German     -> "Sie müssen root sein um den Cache zu sichern."
    Spanish    -> "Debes ser root para hacer una copia de seguridad de la caché."
    Portuguese -> "Precisa ser root para fazer um backup do cache."
    French     -> "Vous devez être `root` pour faire une copie de sauvegarde du cache."
    Russian    -> "Чтобы создать бэкап кэша, вы должны быть рутом"
    Italian    -> "Devi essere root per salvare la cache."
    Serbian    -> "Морате бити root да бисте бекаповали кеш."
    Norwegian  -> "Du må være root for å ta backup på cache."
    _          -> "You must be root to backup the cache."

backupCache_3 :: Language -> String
backupCache_3 = \case
    Japanese   -> "バックアップ先は存在しない。"
    Polish     -> "Lokalizacja kopii zapasowych nie istnieje."
    Croatian   -> "Lokacija sigurnosne kopije ne postoji."
    Swedish    -> "Specifierad backup-plats finns inte."
    German     -> "Der Sicherungsort existiert nicht."
    Spanish    -> "La localización para copia de seguridad no existe."
    Portuguese -> "O caminho indicado para o backup não existe."
    French     -> "Le chemin des copies de sauvegarde spécifié n'existe pas."
    Russian    -> "Путь к бэкапу не существует."
    Italian    -> "L'indirizzo del salvataggio non esiste."
    Serbian    -> "Путања ка бекапу не постоји."
    Norwegian  -> "Spesifisert backup-plass finnes ikke."
    _          -> "The backup location does not exist."

backupCache_4 :: FilePath -> Language -> String
backupCache_4 dir' = let dir = bt dir' in \case
    Japanese   -> "キャッシュのバックアップ先：" ++ dir
    Polish     -> "Tworzenie kopii zapasowej pamięci podręcznej w " ++ dir
    Croatian   -> "Stvaram sigurnosnu kopiju u " ++ dir
    Swedish    -> "Tar backup på cache-filer till " ++ dir
    German     -> "Sichere Cache in " ++ dir
    Spanish    -> "Haciendo una copia de seguridad de la caché en " ++ dir
    Portuguese -> "Backup do cache sendo feito em " ++ dir
    French     -> "Copie de sauvegarde dans " ++ dir ++ "."
    Russian    -> "Бэкап создается в директории " ++ dir
    Italian    -> "Salvataggio della chace in " ++ dir
    Serbian    -> "Бекапујем кеш у " ++ dir
    Norwegian  -> "Tar backup på cache til " ++ dir
    _          -> "Backing up cache to " ++ dir

backupCache_5 :: Int -> Language -> String
backupCache_5 n' = let n = bt (show n') in \case
    Japanese   -> "パッケージのファイル数：" ++ n
    Polish     -> "Pliki będące częścią\xa0kopii zapasowej: " ++ n
    Croatian   -> "Datoteke koje su dio sigurnosne kopije: " ++ n
    Swedish    -> "Paket-filer att ta backup på: " ++ n
    German     -> "Zu sichernde Paketdateien: " ++ n
    Spanish    -> "Ficheros de paquetes de los que se hará copia de seguridad: " ++ n
    Portuguese -> "Arquivos de pacotes para backup: " ++ n
    French     -> "Copie de sauvegarde des fichiers de paquets suivants : " ++ n
    Russian    -> "Упакуйте файлы для бэкапа: " ++ n
    Italian    -> "File del pacchetto da salvare: " ++ n
    Serbian    -> "Датотеке за бекап: " ++ n
    Norwegian  -> "Pakker som blir tatt backup på: " ++ n
    _          -> "Package files to backup: " ++ n

backupCache_6 :: Language -> String
backupCache_6 = \case
    Japanese   -> "バックアップを実行する？"
    Polish     -> "Kontynuować tworzenie kopii zapasowej?"
    Croatian   -> "Nastavi sa stvaranjem sigurnosne kopije?"
    Swedish    -> "Fortsätt med backup?"
    German     -> "Sicherung fortsetzen?"
    Spanish    -> "¿Proceder con la copia de seguridad?"
    Portuguese -> "Proceder com o backup?"
    French     -> "Procéder à la copie de sauvegarde ?"
    Russian    -> "Продолжить создание бэкапа?"
    Italian    -> "Procedere con il salvataggio?"
    Serbian    -> "Наставити бекаповање?"
    Norwegian  -> "Fortsett med backup?"
    _          -> "Proceed with backup?"

backupCache_7 :: Language -> String
backupCache_7 = \case
    Japanese   -> "バックアップは意図的に阻止された。"
    Polish     -> "Tworzenie kopii zapasowej zostało przerwane przez użytkownika."
    Croatian   -> "Stvaranje sigurnosne kopije prekinuto od strane korisnika."
    Swedish    -> "Backup avbröts manuellt."
    German     -> "Backup durch Benutzer abgebrochen."
    Spanish    -> "Copia de seguridad abortada manualmente."
    Portuguese -> "Backup manualmente abortado."
    French     -> "Copie de sauvegarde manuelle annulée."
    Russian    -> "Создание бэкапа прервано пользователем."
    Italian    -> "Salvataggio manuale interrotto."
    Serbian    -> "Бекаповање је ручно прекинуто."
    Norwegian  -> "Backup ble avbrutt manuelt."
    _          -> "Backup manually aborted."

backupCache_8 :: Language -> String
backupCache_8 = \case
    Japanese   -> "バックアップ中。数分かかるかもしれない。"
    Polish     -> "Tworzenie kopii zapasowej. To może potrwać kilka minut..."
    Croatian   -> "Stvaranje sigurnosne kopije. Ovo može potrajati nekoliko minuta..."
    Swedish    -> "Tar backup. Det här kan ta ett tag..."
    German     -> "Sichere. Dies kann ein paar Minuten dauern..."
    Spanish    -> "Haciendo copia de seguridad. Esto puede tardar unos minutos..."
    Portuguese -> "Efetuando backup. Isso pode levar alguns minutos..."
    French     -> "Copie de sauvegarde en cours. Ceci peut prendre quelques minutes…"
    Russian    -> "Создается бэкап. Это может занять пару минут..."
    Italian    -> "Salvataggio. Questo potrebbe richiedere qualche minuto..."
    Serbian    -> "Бекапујем. Ово може да потраје пар минута..."
    Norwegian  -> "Tar backup. Dette kan ta en stund..."
    _          -> "Backing up. This may take a few minutes..."

copyAndNotify_1 :: Int -> Language -> String
copyAndNotify_1 n' = let n = cyan (show n') in \case
    Japanese   -> "#[" ++ n ++"]をコピー中・・・"
    Polish     -> "Kopiowanie #[" ++ n ++ "]"
    Croatian   -> "Kopiranje #[" ++ n ++ "]"
    Swedish    -> "Kopierar #[" ++ n ++ "]"
    German     -> "Kopiere #[" ++ n ++ "]"
    Spanish    -> "Copiando #[" ++ n ++ "]"
    Portuguese -> "Copiando #[" ++ n ++ "]"
    French     -> "Copie de #[" ++ n ++ "]"
    Russian    -> "Копируется #[" ++ n ++ "]"
    Italian    -> "Copiando #[" ++n ++ "]"
    Serbian    -> "Копирам #[" ++ n ++ "]"
    Norwegian  -> "Kopierer #[" ++ n ++ "]"
    _          -> "Copying #[" ++ n ++ "]"

preCleanCache_1 :: String -> Language -> String
preCleanCache_1 n' = let n = n' in \case
    Japanese   -> n ++ "は数字はない。"
    Polish     -> n ++ " nie jest liczbą."
    Croatian   -> n ++ " nije broj. "
    Swedish    -> n ++ " är inte ett nummer."
    German     -> n ++ " ist keine Nummer."
    Spanish    -> n ++ " no es un número."
    Portuguese -> n ++ " não é um número."
    French     -> n ++ " n'est pas un nombre."
    Russian    -> n ++ " не является числом."
    Italian    -> n ++ " non è un numero."
    Serbian    -> n ++ " није број."
    Norwegian  -> n ++ " er ikke et nummer."
    _          -> n ++ " is not a number."

cleanCache_1 :: Language -> String
cleanCache_1 = \case
    Japanese   -> "入力の数字は適切ではない。"
    Polish     -> "Nieprawidłowa liczba."
    Croatian   -> "Broj nije ispravan."
    Swedish    -> "Ogiltigt nummer specifierat."
    German     -> "Ungültige Nummer gegeben."
    Spanish    -> "Número inválido."
    Portuguese -> "Número inválido."
    French     -> "Nombre donné invalide."
    Russian    -> "Дано невалидное число."
    Italian    -> "Numero non valido."
    Serbian    -> "Број није валидан."
    Norwegian  -> "Ugyldig number spesifisert."
    _          -> "Invalid number given."

cleanCache_2 :: Language -> String
cleanCache_2 = \case
    Japanese   -> "パッケージ・キャッシュは完全に削除される。"
    Polish     -> "To usunie WSZYSTKIE pakiety z pamięci podręcznej."
    Croatian   -> "Ovo će izbrisati CIJELI cache paketa."
    Swedish    -> "Detta kommer ta bort HELA paket-cachen."
    German     -> "Dies wird den GESAMTEN Paketcache leeren."
    Spanish    -> "Esto eliminará POR COMPLETO la caché de paquetes."
    Portuguese -> "Isso eliminara TODOS OS PACOTES do cache."
    French     -> "Ceci va supprimer la TOTALITÉ du cache des paquets."
    Russian    -> "Это действие ВСЕЦЕЛО уничтожит кэш пакетов."
    Italian    -> "Questo cancellera l'INTERA cache dei pacchetti."
    Serbian    -> "Ово ће избрисати ЦЕО кеш пакета."
    Norwegian  -> "Dette vil slette HELE pakke-cachen."
    _          -> "This will delete the ENTIRE package cache."

cleanCache_3 :: Int -> Language -> String
cleanCache_3 n' = let n = bt (show n') in \case
    Japanese   -> "パッケージ・ファイルは" ++ n ++ "個保存される。"
    Polish     -> n ++ " wersji każdego pakietu zostanie zachowane."
    Croatian   -> n ++ " zadnjih verzija svakog paketa će biti zadržano."
    Swedish    -> n ++ " av varje paketfil kommer att sparas."
    German     -> n ++ " jeder Paketdatei wird behalten."
    Spanish    -> n ++ " ficheros de cada paquete se mantendrán."
    Portuguese -> n ++ " arquivos de cada pacote serão mantidos."
    French     -> n ++ " fichiers de chaque paquet sera conservé."
    Russian    -> n ++ " версии каждого пакета будут нетронуты."
    Italian    -> n ++ " di ciascun pacchetto sarà mantenuto."
    Serbian    -> n ++ " верзије сваког од пакета ће бити сачуване."
    Norwegian  -> n ++ " av hver pakkefil blir beholdt."
    _          -> n ++ " of each package file will be kept."

cleanCache_4 :: Language -> String
cleanCache_4 = \case
    Japanese   -> "残りは全部削除される。承知する？"
    Polish     -> "Wszystko inne zostanie usunięte. Na pewno?"
    Croatian   -> "Ostali paketi će biti izbrisani. Jeste li sigurni?"
    Swedish    -> "Resten kommer att tas bort. Är det OK?"
    German     -> "Der Rest wird gelöscht. Ist das OK?"
    Spanish    -> "El resto se eliminará. ¿OK?"
    Portuguese -> "O resto será deletado. OK?"
    French     -> "Le reste sera supprimé. Êtes-vous d'accord ?"
    Russian    -> "Всё остальное будет удалено. Годится?"
    Italian    -> "Il resto verrà mantenuto. Continuare?"
    Serbian    -> "Остатак ће бити избрисан. Да ли је то у реду?"
    Norwegian  -> "Resten vil bli slettet. Er det OK?"
    _          -> "The rest will be deleted. Okay?"

cleanCache_5 :: Language -> String
cleanCache_5 = \case
    Japanese   -> "削除の続行は意図的に阻止された。"
    Polish     -> "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."
    Croatian   -> "Čišćenje cache-a paketa prekinuto od strane korisnika."
    Swedish    -> "Cache-rensning avbröts manuellt."
    German     -> "Säubern des Caches durch Benutzer abgebrochen."
    Spanish    -> "Limpieza de la caché abortada manualmente."
    Portuguese -> "Limpeza do cache abortada manualmente."
    French     -> "Le nettoyage du cache a été arrêté manuellement."
    Russian    -> "Очистка кэша прервана пользователем."
    Italian    -> "Pulitura manuale della cache interrotta."
    Serbian    -> "Чишћење кеша је ручно прекинуто."
    Norwegian  -> "Cache-rensing ble avbrutt manuelt."
    _          -> "Cache cleaning manually aborted."

cleanCache_6 :: Language -> String
cleanCache_6 = \case
    Japanese   -> "パッケージ・キャッシュを掃除中・・・"
    Polish     -> "Czyszczenie pamięci podręcznej..."
    Croatian   -> "Čišćenje cache-a paketa..."
    Swedish    -> "Rensar paket-cache..."
    German     -> "Säubere Paketcache..."
    Spanish    -> "Limpiando la caché de paquetes..."
    Portuguese -> "Limpando cache de pacotes..."
    French     -> "Nettoyage du cache des paquets…"
    Russian    -> "Очистка кэша пакета..."
    Italian    -> "Ripulisco la cache..."
    Serbian    -> "Чишћење кеша..."
    Norwegian  -> "Renser pakke-cache..."
    _          -> "Cleaning package cache..."

-- NEEDS TRANSLATION
cleanNotSaved_1 :: Language -> String
cleanNotSaved_1 = \case
    Japanese  -> "不要パッケージファイルを確認・・・"
    Croatian  -> "Pronalazim nepotrebne datoteke paketa..."
    German    -> "Bestimme nicht benötigte Paketdateien..."
    Norwegian -> "Finner unødige pakkefiler..."
    Italian   -> "Determino i pacchetti non più necessari..."
    French    -> "Détermination des fichiers de paquet inutiles…"
    Russian   -> "Вычисляются ненужные файлы пакетов..."
    _         -> "Determining unneeded package files..."

-- NEEDS TRANSLATION
cleanNotSaved_2 :: Int -> Language -> String
cleanNotSaved_2 s' = let s = cyan (show s') in \case
    Japanese  -> "「" ++ s ++ "」の不要パッケージファイルあり。削除？"
    Croatian  -> s ++ " nepotrebnih datoteka pronađeno. Obrisati?"
    German    -> s ++ " nicht benötigte Paketdateien gefunden. Löschen?"
    Norwegian -> s ++ " unødige pakkefiler funnet. Vil du slette?"
    Italian   -> s ++ " pacchetti non necessari trovati. Cancellarli?"
    French    -> s ++ " paquets inutiles trouvés. Les supprimer ?"
    Russian   -> s ++ " -- столько ненужных пакетных файлов обнаружено. Удалить?"
    _         -> s ++ " unneeded package files found. Delete?"

----------------------------
-- Aura/Commands/L functions
----------------------------
logLookUpFields :: Language -> [String]
logLookUpFields = \case
    Japanese   -> [ "パッケージ","初インストール","アップグレード回数","近況" ]
    Polish     -> [ "Pakiet","Pierwsza instalacja","Aktualizacje","Ostatnie akcje" ]
    Croatian   -> [ "Paket","Prva instalacija","Nadogradnje","Nedavne radnje" ]
    Swedish    -> [ "Paket","Första installation","Uppgraderingar","Nyliga händelser" ]
    German     -> [ "Paket","Erste Installation","Aktualisierungen","Letzte Aktionen" ]
    Spanish    -> [ "Paquete","Primera instalación","Actualizaciones","Acciones Recientes" ]
    Portuguese -> [ "Pacote","Primeira instalação","Atualizações","Ações Recentes" ]
    French     -> [ "Paquet","Première installation","Mises à jours","Actions récentes" ]
    Russian    -> [ "Пакет","Первая установка","Обновления","Недавние действия" ]
    Italian    -> [ "Package","Prima installazione","Upgrades","Azioni recenti" ]
    Serbian    -> [ "Пакет","Прва инсталација","Ажурирања","Недавне радње" ]
    Norwegian  -> [ "Pakke","Første installasjon","Oppgraderinger","Nylige hendelser" ]
    _          -> [ "Package","First Install","Upgrades","Recent Actions" ]

reportNotInLog_1 :: Language -> String
reportNotInLog_1 = \case
    Japanese   -> "logファイルには出ていない："
    Polish     -> "Tych pakietów nie ma w dzienniku:"
    Croatian   -> "Ovih paketa nema u dnevniku:"
    Swedish    -> "Dessa har inte framkommit i loggfiler:"
    German     -> "Diese sind nicht in der Logdatei aufgetaucht:"
    Spanish    -> "Estos no aparecen en el fichero log:"
    Portuguese -> "Os seguintes não apareceram no log de arquivo:"
    French     -> "Ceci n'apparaît pas des les journaux (log) :"
    Russian    -> "Следующих пакетов нет в лог-файле:"
    Italian    -> "Questo non apparirà nei file di log;"
    Serbian    -> "Ови пакети се не спомињу у дневнику:"
    Norwegian  -> "Følgende har ikke vist seg i loggen:"
    _          -> "These have not appeared in the log file:"

----------------------------
-- Aura/Commands/M functions
----------------------------
-- NEEDS TRANSLATION
cleanABSTree_1 :: Language -> String
cleanABSTree_1 = \case
    Japanese  -> "ABS Treeの中身を削除？"
    Croatian  -> "Obrisati cijelo ABS stablo?"
    German    -> "Den gesamten ABS Baum löschen?"
    Norwegian -> "Slett hele ABS-treet?"
    Italian   -> "Cancellare l'intero albero ABS?"
    French    -> "Supprimer la totalité de l'arbre ABS ?"
    Russian   -> "Удалить дерево ABS полностью?"
    _         -> "Delete the entire ABS Tree?"

-- NEEDS TRANSLATION
cleanABSTree_2 :: Language -> String
cleanABSTree_2 = \case
    Japanese  -> "ABS Treeの中身を削除中・・・"
    Croatian  -> "Brišem ABS stablo..."
    German    -> "Lösche ABS Baum..."
    Norwegian -> "Renser ABS-treet..."
    Italian   -> "Ripulisco l'abero ABS..."
    French    -> "Suppression de l'arbre ABS…"
    Russian   -> "Удаляю дерево ABS..."
    _         -> "Clearing out ABS Tree..."

----------------------
-- Aura/Flags functions
----------------------
inheritedOperTitle :: Language -> String
inheritedOperTitle = \case 
    Japanese    -> "Pacmanからの引継選択肢"
    Polish      -> "Operacje z Pacmana"
    Croatian    -> "Pacman operacije"
    Swedish     -> "Ärvda pacman-operationer"
    German      -> "Von Pacman geerbte Operationen"
    Spanish     -> "Operaciones Heredadas de Pacman"
    Portuguese  -> "Operações herdadas do Pacman"
    French      -> "Opérations héritées de Pacman"
    Russian     -> "Позаимствованные из pacman действия"
    Italian     -> "Operazioni riguardanti Pacman"
    Serbian     -> "Наслеђене pacman-ове операције"
    Norwegian   -> "Arvede `pacman`-operasjoner"
    _           -> "Inherited Pacman Operations"

auraOperTitle :: Language -> String
auraOperTitle = \case
    Japanese   -> "Auraだけの選択肢："
    Polish     -> "Operacje Aury:"
    Croatian   -> "Aura operacije:"
    Swedish    -> "Aura-specifika operationer:"
    German     -> "Aura-spezifische Operationen:"
    Spanish    -> "Operaciones Exclusivas de Aura:"
    Portuguese -> "Operações exclusivas do Aura:"
    French     -> "Opérations propres à Aura :"
    Russian    -> "Специфичные для aura действия:"
    Italian    -> "Operazioni esclusive di Aura:"
    Serbian    -> "Аура-специфичне операције:"
    Norwegian  -> "Aura-spesifikke operasjoner:"
    _          -> "Aura Only Operations:"

aurSy :: Language -> String
aurSy = green . \case
    Japanese   -> "[A]URに関連する処理\n" ++ "デフォルトでAURからインストール"
    Polish     -> "Wykonuje akcje związane z [A]UR.\n" ++ "Domyślnie instaluje pakiety z AUR."
    Croatian   -> "Izvršava radnje s [A]UR-om.\n" ++ "Uobičajena (default) radnja je instaliranje paketa iz AUR-a."
    Swedish    -> "Utför åtgärder involverandes [A]UR.\n" ++ "Standard-åtgärd installerar ifrån AUR."
    German     -> "Führe Aktionen aus die das [A]UR betreffen.\n" ++ "Standardaktion installiert aus dem AUR."
    Spanish    -> "Realizar acciones relacionadas con el [A]UR.\n" ++ "La acción por defecto es instalar desde AUR."
    Portuguese -> "Realizar ações envolvendo o [A]UR.\n" ++ "Ação padrão instala do AUR."
    French     -> "Actions impliquant [A]UR.\n" ++ "Par défaut, installe depuis AUR."
    Russian    -> "Совершить действия с участием [A]UR.\n" ++ "Действие по умолчанию устанавливает из AUR."
    Italian    -> "Azioni riguardanti [A]UR.\n" ++ "Di default installa da AUR."
    Serbian    -> "Извршава радње везане за [A]UR.\n" ++ "Уобичајена радња инсталира из AUR-а."
    Norwegian  -> "Utfør handlinger som innebærer [A]UR.\n" ++ "Standard-handling installerer fra AUR."
    _          -> "Perform actions involving the [A]UR.\n" ++ "Default action installs from the AUR."

-- NEEDS TRANSLATION
absSy :: Language -> String
absSy = magenta . \case
    Croatian  -> "Izvršava operacije sa ABS stablom.\n" ++ "Uobičajena (default) radnja je ručna izgradnja iz ABS stabla ([M]anual)."
    German    -> "Führe Aktionen aus die den ABS Baum betreffen.\n" ++ "Standardaktion baut [M]anuell aus ABS."
    Norwegian -> "Utfør handlinger som involverer ABS-treet.\n" ++ "Standard-handling bygger [M]anuelt fra ABS."
    French    -> "Effectue une action impliquant l'arbre ABS.\n" ++ "Par défaut, installe [M]anuellement depuis ABS."
    _         -> "Perform actions involving the ABS tree.\n" ++ "Default action [M]anually builds from ABS."

-- NEEDS TRANSLATION
saveS :: Language -> String
saveS = yellow . \case
    Japanese  -> "パッケージの設置状態に関する処理\n" ++ "デフォルトでインストール状態を保存する。"
    Croatian  -> "Upravlja spremanjem i vraćanjem globalnog stanja paketa.\n" ++ "Uobičajena (default) radnja je spremanje trenutnog stanja paketa."
    German    -> "Verwalte das [S]peichern und Wiederherstellen des globalen Paketzustände.\n" ++ "Standardaktion sichert die Zustände."
    Serbian   -> "Управља чувањем и враћањем глобалног стања пакета.\n" ++ "Уобичајена радња чува тренутно стање."
    Norwegian -> "Administer lagring og gjenoppretting av den globale pakketilstanden.\n" ++ "Standard-handling lagrer denne tilstanden."
    Italian   -> "Gestisco il [S]alvataggio e ripristino dello stato globale dei pacchetti.\n" ++ "Salva lo stato in maniera predefinita."
    French    -> "Gestion de la [S]auvegarde et de la restauration de l'état global des paquets.\n" ++ "Par défaut, sauvegarde l'état actuel."
    Russian   -> "Настроить [S]охранение и восстановление глобального состояние пакетов.\n" ++ "Действие по умолчанию сохраняет это состояние."
    _         -> "Manage the [S]aving and restoring of the global package state.\n" ++ "Default action saves this state."

downG :: Language -> String
downG = red . \case
    Japanese   -> "キャッシュに関連する処理\n" ++ "デフォルトでパッケージをダウングレード"
    Polish     -> "Wykonuje akcje związane z pamięcią podręczną ([C]ache) pakietów.\n" ++ "Domyślnie instaluje starsze wersje podanych pakietów."
    Croatian   -> "Izvršava radnje sa [C]ache-om paketa.\n" ++ "Uobičajena (default) radnja je vraćanje paketa na prijašnju verziju."
    Swedish    -> "Utför åtgärder involverandes paket-[C]ache.\n" ++ "Standard-åtgärd nergraderar valda paket."
    German     -> "Führe Aktionen aus die den Paket[C]ache betreffen.\n" ++ "Standardaktion downgradet gegebene Pakete."
    Spanish    -> "Realizar acciones relacionadas con la [C]aché.\n" ++ "La acción por defecto es retornar a versiones antiguas de los paquetes especificados."
    Portuguese -> "Realiza ações relacionadas ao [C]ache.\n" ++ "Ação padrão retorna os pacotes informados às suas versões anteriores."
    French     -> "Actions impliquant le [C]ache des paquets.\n" ++ "Par défaut, mets les paquets spécifiés à niveau vers une version antérieure."
    Russian    -> "Совершить действия с участием кэша пакета ([C]ache).\n" ++ "Действие по умолчанию откатывает данные пакеты к старым версиям."
    Italian    -> "Azioni riguardanti la [C]ache dei pacchetti.\n" ++ "Di default retrocede il pacchetti."
    Serbian    -> "Извршава радње везане за кеш пакета.\n" ++ "Уобичајена радња враћа претходну верзију датих пакета."
    Norwegian  -> "Utfør handlinger som involverer pakke-[C]achen.\n" ++ "Standard-handling nedgraderer den valgte pakken."
    _          -> "Perform actions involving the package [C]ache.\n" ++ "Default action downgrades given packages."

viewL :: Language -> String
viewL = cyan . \case
    Japanese   -> "[L]ogfileに関連する処理\n" ++ "デフォルトでlogfileを閲覧用に開く"
    Polish     -> "Wykonuje akcje związane z dziennikiem ([L]ogiem) pacmana.\n" ++ "Domyślnie otwiera log w trybie tylko do odczytu."
    Croatian   -> "Izvršavanje radnje sa pacman dnevnikom ([L]ogfile).\n" ++ "Uobičajena (default) radnja je ispis dnevnika."
    Swedish    -> "Utför åtgärder involverandes pacmans [L]ogfil.\n" ++ "Standard-åtgärd öppnar loggen med read-only-attribut."
    German     -> "Führe Aktionen aus die die Pacman [L]ogdatei betreffen.\n" ++ "Standardaktion öffnet den Log (nur Lesen)"
    Spanish    -> "Realizar acciones relacionadas con el fichero [L]og de pacman.\n" ++ "La acción por defecto es abrir el log en modo sólo lectura."
    Portuguese -> "Realiza ações relacionadas ao [L]ogfile do Pacman.\n" ++ "Ação padrão abre o arquivo de log apenas para leitura."
    French     -> "Actions impliquant le fichier de [L]og (journal) de Pacman.\n" ++ "Par défaut, ouvre le journal en lecture seule."
    Russian    -> "Совершить действия с участием [L]og-файлов pacman.\n" ++ "Действие по умолчанию открывает лог для просмотра в режиме для чтения."
    Italian    -> "Azioni riguardanti i [L]ogfile di pacman.\n" ++ "Di default visualizza il log in sola lettura."
    Serbian    -> "Извршава радње везане за pacman-ов дневник.\n" ++ "Уобичајена радња даје преглед дневника."
    Norwegian  -> "Utfør handlinger som involverer `pacman`'s [L]oggfil.\n" ++ "Standard-handling åpner loggen for skrivebeskyttet lesing."
    _          -> "Perform actions involving the pacman [L]ogfile.\n" ++ "Default action opens the log for read-only viewing."

orpha :: Language -> String
orpha = blue . \case
    Japanese   -> "必要とされていない従属パッケージに関する処理\n" ++ "デフォルトでその従属パッケージの名前を出力"
    Polish     -> "Wykonuje akcje związane z [O]sieroconymi pakietami.\n" ++ "Domyślnie wyświetla wszystkie osierocone pakiety."
    Croatian   -> "Izvršava radnje s paketima bez roditelja ([O]rphan).\n" ++ "Uobičajena (default) radnja je izlistavanje paketa bez roditelja."
    Swedish    -> "Utför åtgärder involverandes [O]rphan-paket.\n" ++ "Standard-åtgärd listar alla orphan-paket."
    German     -> "Führe Aktionen aus die verwaiste ([O]rphans) Pakete betreffen.\n" ++ "Standardaktion listet alle verwaisten Pakete auf."
    Spanish    -> "Realizar acciones relacionadas con paquetes huérfanos ([O]rphan).\n" ++ "La acción por defecto es listar todos los paquetes huérfanos."
    Portuguese -> "Realiza ações com pacotes [O]rfãos.\n" ++ "Ação padrão lista todos os pactes orfãos."
    French     -> "Actions impliquant les paquets [O]rphelins.\n" ++ "Par défaut, liste l'ensemble des paquets orphelins."
    Russian    -> "Совершить действия с участием [O]сиротевших пакетов.\n" ++ "Действие по умолчанию берёт в расчёт все осиротевшие пакеты."
    Italian    -> "Azioni riguardanti i pacchetti [O]rfani.\n" ++ "Di default elenca i pacchetti orfani."
    Serbian    -> "Извршава радње везане за пакете без родитеља.\n" ++ "Уобичајена радња листа пакете без родитеља."
    Norwegian  -> "Utfør handlinger som involverer foreldreløse pakker ([O]rphans).\n" ++ "Standard-handling åpner alle foreldreløse pakker."
    _          -> "Perform actions involving [O]rphan packages.\n" ++ "Default action lists all orphan packages."

-------------------------------
-- Aura/AUR functions
-------------------------------
-- NEEDS TRANSLATION
getAURPkgInfo_1 :: Language -> String
getAURPkgInfo_1 = \case
    Japanese  -> "AURのAPIに繋げなかった。ネット接続状態を確認して下さい。"
    Croatian  -> "Pristup AUR-u nije uspio. Provjerite svoju vezu."
    German    -> "AUR API Suche fehlgeschlagen. Bitte überprüfen Sie Ihre Verbindung."
    Serbian   -> "Приступ AUR-у није успео. Проверите вашу везу."
    Norwegian -> "AUR API-oppslag feilet. Vennligst sjekk tilkoblingen din."
    Italian   -> "connessione ad AUR API fallita. Controllare la propria connessione."
    French    -> "La recherche dans l'API AUR a échouée. Veuillez vérifiez votre connexion."
    Russian   -> "Запрос к AUR API не удался. Пожалуйста, проверьте ваше соединение."
    _         -> "AUR API lookup failed. Please check your connection."

-- `Maintainer` value NEEDS UPDATING!
infoFields :: Language -> [String]
infoFields = \case
    Japanese   -> [ "リポジトリ","名前","バージョン","パッケージ状態","管理者","プロジェクト","パッケージページ","ライセンス","従属パッケージ","作成時従属パ","投票数","概要" ]
    Polish     -> [ "Repository","Nazwa","Wersja","Status w AUR","Maintainer","URL Projektu","URL w AUR","Licencja","Depends On","Build Deps","Głosy","Opis" ]
    Croatian   -> [ "Repozitorij","Ime","Verzija","AUR Stanje","Maintainer","URL Projekta","AUR URL","Licenca","Depends On","Build Deps","Glasovi","Opis" ]
    Swedish    -> [ "Repository","Namn","Version","AUR Status","Maintainer","Projekt URL","AUR URL","Licens","Depends On","Build Deps","Röster","Beskrivning" ]
    German     -> [ "Repository","Name","Version","AUR Status","Maintainer","Projekt URL","AUR URL","Lizenz","Depends On","Build Deps","Stimmen","Beschreibung" ]
    Spanish    -> [ "Repository","Nombre","Versión","Estado en AUR","Maintainer","URL del proyecto","URL en AUR","Licencia","Depends On","Build Deps","Votos","Descripción" ]
    Portuguese -> [ "Repositório","Nome","Versão","Estado no AUR","Maintainer","URL do projeto","URL no AUR","Licença","Depends On","Build Deps","Votos","Descrição" ]
    French     -> [ "Dépôt","Nom","Version","Statut de AUR","Mainteneur","URL du projet","URL AUR","Licence","Dépends de","Dépendances de compilation","Votes","Description" ]
    Russian    -> [ "Репозиторий","Название","Версия","Статус в AUR","Ответственный","URL проекта","URL в AUR","Лицензия","Depends On","Build Deps","Рейтинг","Описание" ]
    Italian    -> [ "Repository","Nome","Versione","Stato in AUR","Maintainer","URL del progetto","URL AUR","Licenza","Depends On","Build Deps","Voti","Descrizione" ]
    Serbian    -> [ "Ризница","Име","Верзија","Статус у AUR-у","Maintainer","Страница пројекта","Страница у AUR-у","Лиценца","Depends On","Build Deps","Гласови","Опис" ]
    Norwegian  -> [ "Depot","Navn","Versjon","AUR Status","Vedlikeholder","Prosjekt-URL","AUR URL","Lisens","Depends On","Build Deps","Stemmer","Beskrivelse" ]
    _          -> [ "Repository","Name","Version","AUR Status","Maintainer","Project URL","AUR URL","License","Depends On","Build Deps","Votes","Description" ]

outOfDateMsg :: Bool -> Language -> String
outOfDateMsg True = red . \case
    Japanese   -> "AURで要更新！"
    Polish     -> "Nieaktualny!"
    Croatian   -> "Zastarjelo!"
    Swedish    -> "Utdaterad!"
    German     -> "Veraltet!"
    Spanish    -> "¡Desactualizado!"
    Portuguese -> "Desatualizado!"
    French     -> "Périmé !"
    Russian    -> "Устарел!"
    Italian    -> "Out of Date!"
    Serbian    -> "Застарео!"
    Norwegian  -> "Utdatert!"
    _          -> "Out of Date!"
outOfDateMsg False = green . \case
    Japanese   -> "最新"
    Polish     -> "Aktualny"
    Croatian   -> "Ažurirano"
    Swedish    -> "Aktuell"
    German     -> "Aktuell"
    Spanish    -> "Actualizado"
    Portuguese -> "Atualizado"
    French     -> "À jour"
    Russian    -> "Новейший"
    Italian    -> "Aggiornato"
    Serbian    -> "Ажуран"
    Norwegian  -> "Oppdatert"
    _          -> "Up to Date"

-- NEEDS TRANSLATION
orphanedMsg :: Maybe String -> Language -> String
orphanedMsg (Just m) _   = bForeground m
orphanedMsg Nothing = red . \case
    Japanese  -> "いない"
    Croatian  -> "Nema roditelja!"
    German    -> "Verwaist!"
    Norwegian -> "Foreldreløs!"
    French    -> "Abandonné !"
    _         -> "Orphaned!"

-----------------------
-- Aura/ABS functions
-----------------------
-- NEEDS TRANSLATION
absSync_1 :: Language -> String
absSync_1 = \case
    Japanese  -> "ローカルABS Treeを同期？"
    Croatian  -> "Sinkronizirati lokalno ABS stablo?"
    German    -> "Lokalen ABS Baum synchronisieren?"
    Norwegian -> "Synkroniser det lokale ABS-treet?"
    Italian   -> "Sincronizzare l'albero ABS locale?"
    French    -> "Synchroniser l'arbre ABS local ?"
    Russian   -> "Синхронизировать локальное дерево ABS?"
    _         -> "Sync the local ABS Tree?"

absSync_2 :: Language -> String
absSync_2 = \case
    Japanese  -> "ローカルABS Treeを同期中・・・"
    Croatian  -> "Sinkroniziram lokalno ABS stablo..."
    German    -> "Synchronisiere lokalen ABS Baum..."
    Norwegian -> "Synkroniserer det lokale ABS-treet..."
    Italian   -> "Sincronizzo l'albero ABS locale..."
    French    -> "Synchronisation de l'arbre ABS local…"
    Russian   -> "Синхронизируется локальное дерево ABS..."
    _         -> "Syncing local ABS Tree..."

singleSync_1 :: String -> Language -> String
singleSync_1 p' = let p = bt p' in \case
    Japanese  -> p ++ "をABS Treeに同期・・・"
    Croatian  -> "Sinkroniziram " ++ p ++ " u lokalnom stablu..."
    German    -> "Synchronisiere " ++ p ++ " in den lokalen ABS Baum..."
    Norwegian -> "Synkroniserer " ++ p ++ " til det lokale ABS-treet..."
    Italian   -> "Sincronizzo " ++ p ++ " nell'albero ABS locale..."
    French    -> "Synchronisation de " ++ p ++ " dans l'arbre ABS local…"
    Russian   -> p ++ " синхронизируется с локальным дерево ABS..."
    _         -> "Syncing " ++ p ++ " to the local ABS Tree..."

absInfoFields :: Language -> [String]
absInfoFields = \case
    Croatian  -> [ "Repozitorij","Ime","Verzija","Zavisnosti","Make Zavisnosti","Opis" ]
    German    -> [ "Quelle","Name","Version","Hängt ab von","Make Abh.","Beschreibung"]
    Norwegian -> [ "Depot","Navn","Versjon","Er avhengig av","Make Deps","Beskrivelse"]
    Italian   -> [ "Repository","Nome","Versione","Dipende da","Make Deps","Descrizione" ]
    French    -> [ "Dépôt","Nom","Version","Dépendances","Dépendances de compilation","Description" ]
    Russian   -> [ "Репозиторий","Название","Версия","Зависит от","Зависимости Make","Описание" ]
    _         -> [ "Repository","Name","Version","Depends On","Make Deps","Description" ]

repository_1 :: String -> Language -> String
repository_1 p = \case
    Japanese  -> p ++ "はどのリポジトリにもない。"
    Croatian  -> p ++ "nije paket u repozitoriju."
    German    -> p ++ " ist kein Paket in irgendeiner Quelle."
    Norwegian -> p ++ " er ikke en pakke i noe depot."
    Italian   -> p ++ " non è un pacchetto di nessun repository."
    French    -> p ++ " n'est pas un paquet dans aucun des dépôts."
    Russian   -> "Пакет " ++ p ++ " не найден ни в одном репозитории."
    _         -> p ++ " is not a package in any repository."

pkgbuildKeyMissing :: String -> Language -> String
pkgbuildKeyMissing key = \case
    Croatian  -> "Nemoguće izvući vrijednost za " ++ key ++ " iz PKGBUILD-a."
    German    -> "Kann Schlüssel " ++ key ++ " nicht aus PKGBUILD parsen."
    Norwegian -> "Forstår ikke " ++ key ++ " fra PKGBUILD."
    Italian   -> "Inpossibile elaborare la chiave " ++ key ++ " dal PKGBUILD."
    French    -> "Impossible d'analyser la clef " ++ key ++ " depuis le PKGBUILD."
    Russian   -> "Не получилось разобрать ключ " ++ key ++ " из PKGBUILD."
    _         -> "Unable to parse key " ++ key ++ " from PKGBUILD."

missingDescription :: Language -> String
missingDescription = \case
    Croatian  -> "Nema opisa."
    German    -> "Keine Beschreibung."
    Norwegian -> "Ingen beskrivelse."
    Italian   -> "Nessuna Descrizione."
    French    -> "Aucune description."
    Russian   -> "Без описания."
    _         -> "No description."

-----------------------
-- Aura/State functions
-----------------------
-- NEEDS TRANSLATION
saveState_1 :: Language -> String
saveState_1 = \case
    Japanese  -> "現在パッケージ状態保存完了。"
    Croatian  -> "Stanje paketa spremljeno."
    German    -> "Paketzustände gesichert."
    Serbian   -> "Сачувано стање пакета."
    Norwegian -> "Lagret pakketilstand."
    Italian   -> "Stato del pacchetto salvato."
    French    -> "État des paquets sauvegardé."
    Russian   -> "Состояние пакетов сохранено."
    _         -> "Saved package state."

-- NEEDS TRANSLATION
restoreState_1 :: Language -> String
restoreState_1 = \case
    Japanese  -> "対象バージョンがないパッケージ："
    Croatian  -> "Tražene stare verzije nisu dostupne za:"
    German    -> "Gewünschte Downgradeversionen nicht Verfügbar für:"
    Serbian   -> "Захтеване старе верзије нису доступне за:"
    Norwegian -> "De spesifiserte nedgraderingsversjonene er ikke tilgjengelig for:"
    Italian   -> "Richiesta di retrocessione di versione non disponibile per:"
    French    -> "Version antérieure requise non disponible pour :"
    Russian   -> "Запрошенные версии для отката не доступны для:"
    _         -> "Requested downgrade versions not available for:"

-- NEEDS TRANSLATION
reinstallAndRemove_1 :: Language -> String
reinstallAndRemove_1 = \case
    Japanese  -> "パッケージを変更する必要ない。"
    Croatian  -> "Nema paketa kojima su potrebne izmjene."
    German    -> "Keine Pakete brauchen Änderungen."
    Serbian   -> "Ниједан пакет не захтева измене."
    Norwegian -> "Ingen pakker trenger forandring."
    Italian   -> "Nessun pacchetto necessita cambiamenti."
    French    -> "Aucun paquet n'a besoin de changement."
    Russian   -> "Пакеты не нуждаются в изменениях."
    _         -> "No packages need changing."

--------------------------------------
-- Aura/Settings/BadPackages functions
--------------------------------------
-- NEEDS TRANSLATION
circDep_1 :: String -> Language -> String
circDep_1 p' = let p = bt p' in \case
    Japanese  -> p ++ "と互いに従属している。"
    Croatian  -> "Ima kružnu zavisnost sa " ++ p ++ "."
    German    -> "Hat eine zirkuläre Abhängigkeit mit " ++ p ++ "."
    Serbian   -> "Има кружну зависност са " ++ p ++ "."
    Norwegian -> "Har en sirkulær avhengighet med " ++ p ++ "."
    Italian   -> "E' una dipendenza circolare di " ++ p ++ "."
    French    -> "A une dépendance circulaire avec " ++ p ++ "."
    Russian   -> "Имеет круговую зависимость с " ++ p ++ "."
    _         -> "Has a circular dependency with " ++ p ++ "."

-- NEEDS TRANSLATION
bashisms_1 :: Language -> String
bashisms_1 = \case
    Japanese  -> "PKGBUILDのBashコードが複雑すぎる。"
    Croatian  -> "Previše „bash-izama“ u PKGBUILD-u."
    German    -> "Zu viele „bashismen“ im PKGBUILD."
    Serbian   -> "Превише „bash-изама“ у PKGBUILD-у."
    Norwegian -> "For mange „bashismer“ i PKGBUILD."
    Italian   -> "Troppo 'bashisms' nel PKGBUILD."
    French    -> "Trop de « bashisms » dans le PKGBUILD."
    Russian   -> "В PKGBUILD слишком много башизмов."
    _         -> "Too many bashisms in PKGBUILD."

------------------------
-- Aura/Pacman functions
------------------------
-- NEEDS TRANSLATION
pacmanFailure_1 :: Language -> String
pacmanFailure_1 = \case
    Japanese  -> "入力を確認して下さい。"
    Croatian  -> "Molim vas, provjerite svoj unos."
    German    -> "Bitte überprüfen Sie Ihre Eingabe."
    Serbian   -> "Молим Вас, проверите ваш унос."
    Norwegian -> "Vennligst sjekk din oppføring."
    Italian   -> "Controllare il proprio input."
    French    -> "Merci de vérifier les donnés entrées."
    Russian   -> "Пожалуйста, проверьте ваши введенные данные."
    _         -> "Please check your input."

----------------------------------
-- Aura/Pkgbuild/Editing functions
----------------------------------
hotEdit_1 :: String -> Language -> String
hotEdit_1 p' = let p = bt p' in \case
    Japanese   -> p ++ "のPKGBUILDを編成？"
    Polish     -> "Czy chcesz edytować PKGBUILD " ++ p ++ "?"
    Croatian   -> "Želite li izmjeniti PKGBUILD " ++ p ++ "?"
    Swedish    -> "Vill du ändra PKGBUILD-filen ifrån " ++ p ++ "?"
    German     -> "Möchten Sie die PKGBUILD-Datei für " ++ p ++ " bearbeiten?"
    Spanish    -> "¿Te gustaría editar el PKGBUILD de " ++ p ++ "?"
    Portuguese -> "Desejaria editar o PKGBUILD de " ++ p ++ "?"
    French     -> "Voulez-vous éditer le PKGBUILD de " ++ p ++ " ?"
    Russian    -> "Отредактировать PKGBUILD пакета " ++ p ++ "?"
    Italian    -> "Volete modificare il PKGBUILD di " ++ p ++ "?"
    Serbian    -> "Желите ли да измените PKGBUILD за " ++ p ++ "?"
    Norwegian  -> "Vil du endre PKGBUILD for " ++ p ++ "?"
    _          -> "Would you like to edit the PKGBUILD of " ++ p ++ "?"

customizepkg_1 :: Language -> String
customizepkg_1 = let customizepkg = bt "customizepkg" in \case
    Japanese  -> customizepkg ++ "はインストールされていない。"
    Croatian  -> customizepkg ++ "nije instaliran."
    German    -> customizepkg ++ "ist nicht installiert."
    Norwegian -> customizepkg ++ "er ikke installert."
    Italian   -> customizepkg ++ "non è installato."
    French    -> customizepkg ++ "n'est pas installé."
    Russian   -> customizepkg ++ "не установлен."
    _         -> customizepkg ++ "isn't installed."
