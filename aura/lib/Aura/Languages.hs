{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module    : Aura.Languages.Fields
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- All output strings that a user can be shown.
-- All normal restrictions on line length do not apply for this file, and this file only.

{- AURA TRANSLATORS - Thank you all
Chris "Kwpolska" Warrick                      | Polish
Denis Kasak / "stranac"                       | Croatian
Fredrik Haikarainen / Daniel Beecham          | Swedish
Lukas Niederbremer / Jonas Platte             | German
Alejandro Gómez / Sergio Conde                | Spanish
Henry Kupty / Thiago Perrotta / Wagner Amaral | Portuguese
Ma Jiehong / Fabien Dubosson                  | French
Kyrylo Silin                                  | Russian
Bob Valantin                                  | Italian
Filip Brcic                                   | Serbian
"chinatsun"                                   | Norwegian
"pak tua Greg"                                | Indonesia
Kai Zhang                                     | Chinese
Onoue Takuro                                  | Japanese
-}

module Aura.Languages
  ( module Aura.Languages
  , Language(..) ) where

import           Aura.Colour
import qualified Aura.Languages.Fields as Fields
import           Aura.Types
import           BasePrelude hiding ((<+>))
import qualified Data.Map.Strict as Map (Map, (!), fromList, toList, mapWithKey)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

---

translators :: Map.Map Language T.Text
translators = Map.fromList
    [ (Polish,     "Chris Warrick")
    , (Croatian,   "Denis Kasak / \"stranac\"")
    , (Swedish,    "Fredrik Haikarainen / Daniel Beecham")
    , (German,     "Lukas Niederbremer / Jonas Platte")
    , (Spanish,    "Alejandro Gómez / Sergio Conde")
    , (Portuguese, "Henry Kupty / Thiago Perrotta / Wagner Amaral")
    , (French,     "Ma Jiehong / Fabien Dubosson")
    , (Russian,    "Kyrylo Silin / Alexey Kotlyarov")
    , (Italian,    "Bob Valantin")
    , (Serbian,    "Filip Brcic")
    , (Norwegian,  "\"chinatsun\"")
    , (Indonesia,  "\"pak tua Greg\"")
    , (Chinese,    "Kai Zhang")
    , (Japanese,   "Onoue Takuro")
    ]

-- These need updating! Or removing...
languageNames :: Language -> Map.Map Language T.Text
languageNames = Map.fromList . zip [ Japanese, Polish, Croatian, Swedish, German, Spanish, Portuguese, French, Russian, Italian, Serbian, Norwegian, Indonesia, Chinese ] . \case
    Japanese   -> [ "日本語", "ポーランド語", "クロアチア語", "スウェーデン語", "ドイツ語", "スペイン語", "ポルトガル語", "フランス語", "ロシア語", "イタリア語", "セルビア語", "ノルウェー語", "インドネシア語", "中国語" ]
    Polish     -> [ "Japanese", "polski", "chorwacki", "szwedzki", "niemiecki", "hiszpański", "portugalski", "francuski", "rosyjski", "", "", "", "Indonesian", "Chinese" ]
    Croatian   -> [ "Japanese", "poljski", "hrvatski", "švedski", "njemački", "španjolski", "portugalski", "francuski", "ruski", "talijanski", "srpski", "norveški", "Indonesian", "Chinese" ]
    Swedish    -> [ "Japanese", "Polska", "Kroatiska", "Svenska", "Tyska", "Spanska", "Portugisiska", "Franska", "Ryska", "Italienska", "Serbiska", "Norska", "Indonesian", "Chinese" ]
    German     -> [ "Japanisch", "Polnisch", "Kroatisch", "Schwedisch", "Deutsch", "Spanisch", "Portugiesisch", "Französisch", "Russisch", "Italienisch", "Serbisch", "Norwegisch", "Indonesisch", "Chinese" ]
    Spanish    -> [ "Japanese", "Polaco", "Croata", "Sueco", "Alemán", "Español", "Portugués", "Francés", "Ruso", "Italiano", "Serbio", "Noruego", "Indonesio", "Chinese" ]
    Portuguese -> [ "Japonês", "Polonês", "Croata", "Sueco", "Alemão", "Espanhol", "Português", "Francês", "Russo", "Italiano", "Sérvio", "Norueguês", "Indonésio", "Chinês" ]
    French     -> [ "Japanese", "Polonais", "Croate", "Suédois", "Allemand", "Espagnol", "Portugais", "Français", "Russe", "Italien", "Serbe", "Norvégien", "Indonesian", "Chinese" ]
    Russian    -> [ "Японский", "Польский", "Хорватский", "Шведский", "Немецкий", "Испанский", "Португальский", "Французский", "Русский", "Итальянский", "Сербский", "Норвежский", "Индонезийский", "Китайский" ]
    Italian    -> [ "Giapponese", "Polacco", "Croato", "Svedese", "Tedesco", "Spagnolo", "Portoghese", "Francese", "Russo", "Italiano", "", "", "Indonesian", "Chinese" ]
    Serbian    -> [ "Japanese", "Пољски", "Хрватски", "Шведски", "Немачки", "Шпански", "Португалски", "Француски", "Руски", "Италијански", "Српски", "", "Indonesian", "Chinese" ]
    Norwegian  -> [ "Japanese", "Polsk", "Kroatisk", "Svensk", "Tysk", "Spansk", "Portugisisk", "Fransk", "Russisk", "Italiensk", "Serbisk", "Norsk", "Indonesian", "Chinese" ]
    Indonesia  -> [ "Japanese", "Polandia", "Kroasia", "Swedia", "Jerman", "Spanyol", "Portugis", "Prancis", "Rusia", "Italia", "Serbia", "Norwegia", "Indonesian", "Chinese" ]
    Chinese    -> [ "日语", "波兰语", "克罗地亚语", "瑞典语", "德语", "西班牙语", "葡萄牙语", "法语", "俄语", "意大利语", "塞尔维亚语", "挪威语", "印度尼西亚语", "中文" ]
    _          -> [ "Japanese", "Polish", "Croatian", "Swedish", "German", "Spanish", "Portuguese", "French", "Russian", "Italian", "Serbian", "Norwegian", "Indonesian", "Chinese" ]

translatorMsgTitle :: Language -> T.Text
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
    Indonesia  -> "Penerjemah Aura:"
    Chinese    -> "Aura 的翻译者："
    _          -> "Aura Translators:"

translatorMsg :: Language -> [T.Text]
translatorMsg lang = title : names
  where title = translatorMsgTitle lang
        names = fmap snd . Map.toList $
            Map.mapWithKey (\l t -> formatLang (assocLang l t)) translators
        assocLang lang' translator = (translator, langNames Map.! lang')
        formatLang (translator, lang') = " (" <> lang' <> ") " <> translator
        langNames = languageNames lang

-- Wrap a String in backticks
bt :: T.Text -> Doc AnsiStyle
bt cs = "`" <> pretty cs <> "`"

whitespace :: Language -> Char
whitespace Japanese = '　'  -- \12288
whitespace _ = ' '          -- \32

langFromLocale :: T.Text -> Language
langFromLocale = T.take 2 >>> \case
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
    "id" -> Indonesia
    "zh" -> Chinese
    _    -> English

----------------------
-- Aura/Core functions
----------------------
-- NEEDS TRANSLATION
checkDBLock_1 :: Language -> Doc AnsiStyle
checkDBLock_1 = \case
    Japanese   -> "パッケージデータベースが閉鎖されている状態です。開放したらキーを押して続行してください。"
    Polish     -> "Baza pakietów jest zablokowana. Kiedy zostanie odblokowana naciśnij enter aby kontynuować"
    Croatian   -> "Baza paketa je zaključana. Kad se otključa, pritisnite enter da biste nastavili."
    German     -> "Die Paketdatenbank ist gesperrt. Drücken Sie Enter wenn sie entsperrt ist um fortzufahren."
    Spanish    -> "La base de datos de paquetes está bloqueada. Presiona enter cuando esté desbloqueada para continuar."
    Norwegian  -> "Pakkedatabasen er låst. Trykk enter når den er åpnet for å fortsette."
    French     -> "La base de données des paquets est bloquée. Appuyez sur enter pour continuer."
    Portuguese -> "Banco de dados de pacote travado. Aperte 'enter' quando estiver destravado para poder continuar."
    Russian    -> "База данных пакетов заблокирована. Нажмите \"Ввод\", когда она разблокируется, чтобы продолжить."
    Chinese    -> "包数据库已锁定。请在解锁后按下回车以继续。"
    Swedish    -> "Paketdatabasen är låst. Klicka på enter när den är upplåst."
    _          -> "The package database is locked. Press enter when it's unlocked to continue."

trueRoot_3 :: Language -> Doc AnsiStyle
trueRoot_3 = \case
    Japanese   -> "「root」としてパッケージを作成するのは「makepkg v4.2」で不可能になりました。"
    German     -> "Seit makepkg v4.2 ist es nicht mehr möglich als root zu bauen."
    Spanish    -> "Desde makepkg v4.2 no es posible compilar paquetes como root."
    Portuguese -> "A partir da versão v4.2 de makepkg, não é mais possível compilar como root."
    Russian    -> "С версии makepkg v4.2 сборка от имени root более невозможна."
    Chinese    -> "自从 makepkg v4.2 以后，就不能以根用户身份构建软件了。"
    Swedish    -> "I makepkg v4.2 och uppåt är det inte tillåtet att bygga som root."
    _          -> "As of makepkg v4.2, building as root is no longer possible."

mustBeRoot_1 :: Language -> Doc AnsiStyle
mustBeRoot_1 = let sudo = bt "sudo" in \case
    Japanese   -> sudo <> "を使わないとそれができない！"
    Polish     -> "Musisz użyć " <> sudo <> ", żeby to zrobić."
    Croatian   -> "Morate koristiti" <> sudo <> "za ovu radnju."
    Swedish    -> "Du måste använda " <> sudo <> " för det."
    German     -> "Sie müssen dafür " <> sudo <> " benutzen."
    Spanish    -> "Tienes que utilizar " <> sudo <> " para eso."
    Portuguese -> "Utilize " <> sudo <> " para isso."
    French     -> "Vous devez utiliser " <> sudo <> " pour ça."
    Russian    -> "Необходимо использовать " <> sudo <> " для этого."
    Italian    -> "È necessario utilizzare " <> sudo <> " per questo."
    Serbian    -> "Морате да користите " <> sudo <> " за ову радњу."
    Norwegian  -> "Du må bruke " <> sudo <> " for det."
    Indonesia  -> "Anda harus menggunakan " <> sudo <> " untuk melakukannya."
    Chinese    -> "除非是根用户，否则不能执行此操作。"
    _          -> "You cannot perform this operation without using sudo."

-----------------------
-- Aura/Build functions
-----------------------
buildPackages_1 :: T.Text -> Language -> Doc AnsiStyle
buildPackages_1 (bt -> p) = \case
    Japanese   -> p <> "を作成中・・・"
    Polish     -> "Budowanie " <> p <> "..."
    Croatian   -> "Gradim " <> p <> "..."
    Swedish    -> "Bygger paket " <> p <> "..."
    German     -> "Baue Paket " <> p <> "..."
    Spanish    -> "Compilando " <> p <> "..."
    Portuguese -> "Compilando " <> p <> "..."
    French     -> "Compilation de " <> p <> "..."
    Russian    -> "Сборка " <> p <> "..."
    Italian    -> "Compilazione di " <> p <> "..."
    Serbian    -> "Градим " <> p <> "..."
    Norwegian  -> "Bygger " <> p <> "..."
    Indonesia  -> "Membangun " <> p <> "..."
    Chinese    -> p <> " 正在构建中..."
    _          -> "Building " <> p <> "..."

buildFail_1 :: T.Text -> Language -> Doc AnsiStyle
buildFail_1 (bt -> p) = \case
    Japanese   -> p <> "の作成は失敗しました。"
    Polish     -> "Budowanie " <> p <> " zakończyło się niepowodzeniem."
    Croatian   -> "Izgradnja " <> p <> " nije uspjela."
    Swedish    -> "Det gick inte att bygga paketet " <> p <> "."
    German     -> "Bauen von " <> p <> " ist fehlgeschlagen."
    Spanish    -> "La compilación de " <> p <> " ha fallado."
    Portuguese -> "Falha na compilação do pacote " <> p <> "."
    French     -> "Bon, la compilation de " <> p <> " a échouée."
    Russian    -> "Что ж, сборка " <> p <> " не удалась."
    Italian    -> "La compilazione di " <> p <> "è fallita."
    Serbian    -> "Изградња пакета " <> p <> " није успела."
    Norwegian  -> "Bygging av " <> p <> " feilet."
    Indonesia  -> "Gagal membangun " <> p
    Chinese    -> p <> " 构建失败。"
    _          -> "Well, building " <> p <> " failed."

buildFail_5 :: Language -> Doc AnsiStyle
buildFail_5 = \case
    Japanese   -> "パッケージ作成に失敗しました。"
    Polish     -> "Budowanie nie powiodło się."
    Croatian   -> "Izgradnja nije uspjela."
    Swedish    -> "Gick inte att bygga paket."
    German     -> "Bauen fehlgeschlagen."
    Spanish    -> "La compilación falló."
    Portuguese -> "Falha na compilação."
    French     -> "Compilation échouée."
    Russian    -> "Сборка не удалась."
    Italian    -> "Compilazione fallita."
    Serbian    -> "Изградња пакета није успела."
    Norwegian  -> "Bygging feilet."
    Indonesia  -> "Proses gagal."
    Chinese    -> "构建失败。"
    _          -> "Building failed."

-- NEEDS TRANSLATION
buildFail_6 :: Language -> Doc AnsiStyle
buildFail_6 = \case
    Japanese   -> "それでも続行しますか？"
    Polish     -> "Czy mimo to chcesz kontynuować?"
    Croatian   -> "Želite li svejedno nastaviti?"
    German     -> "Möchten Sie trotzdem fortfahren?"
    Spanish    -> "¿Deseas continuar de todas formas?"
    Norwegian  -> "Vil du fortsette likevel?"
    Italian    -> "Vuoi continuare comunque?"
    Portuguese -> "Gostaria de continuar mesmo assim?"
    French     -> "Voulez-vous tout de même continuer ?"
    Russian    -> "Продолжить, несмотря ни на что?"
    Indonesia  -> "Apakah anda tetap ingin melanjutkan?"
    Chinese    -> "你仍然希望继续吗？"
    Swedish    -> "Vill du fortsätta ändå?"
    _          -> "Would you like to continue anyway?"

-- NEEDS TRANSLATION
buildFail_7 :: T.Text -> Language -> Doc AnsiStyle
buildFail_7 (bt -> p) = \case
    Japanese   -> p <> "のビルドスクリプトを収得できませんでした。"
    Polish     -> "Nie udało się pozyskać skryptów budowania dla " <> p <> "."
    German     -> "Herunterladen der Build-Skripte für " <> p <> " fehlgeschlagen."
    Spanish    -> "No se han podido obtener los scripts de compilación de " <> p <> "."
    Portuguese -> "Falha ao obter scripts de compilação para " <> p <> "."
    Indonesia  -> "Gagal mendapatkan skrip untuk " <> p <> "."
    Russian    -> "Не удалось получить сценарии сборки для " <> p <> "."
    Chinese    -> "无法获得 " <> p <> " 的构建脚本。"
    Swedish    -> "Kunde inte hämta byggskript för " <> p <> "."
    _          -> "Failed to obtain build scripts for " <> p <> "."

buildFail_8 :: Language -> Doc AnsiStyle
buildFail_8 = \case
    Japanese   -> "makepkgは失敗しました。"
    Portuguese -> "Ocorreu um erro ao executar makepkg"
    Russian    -> "Произошла ошибка makepkg."
    _          -> "There was a makepkg failure."

buildFail_9 :: Language -> Doc AnsiStyle
buildFail_9 = \case
  _ -> "Failed to detect any built package files (*.pkg.tar.xz)."

buildFail_10 :: Language -> Doc AnsiStyle
buildFail_10 = \case
  _ -> "Every package failed to build."

------------------------------
-- Aura/Dependencies functions
------------------------------
-- NEEDS UPDATE TO MATCH NEW ENGLISH
getRealPkgConflicts_1 :: T.Text -> T.Text -> T.Text -> T.Text -> Language -> Doc AnsiStyle
getRealPkgConflicts_1 (bt -> prnt) (bt -> p) (bt -> r) (bt -> d) = \case
    Japanese   -> "パッケージ" <> p <> "はバージョン" <> d <> "を要するが" <> "一番最新のバージョンは" <> r <> "。"
    Polish     -> "Zależność " <> p <> " powinna być w wersji " <> d <> ", ale najnowsza wersja to " <> r <> "."
    Croatian   -> "Zavisnost " <> p <> " zahtjeva verziju " <> d <> ", a najnovija dostupna verzija je " <> r <> "."
    Swedish    -> "Beroendepaketet " <> p <> " kräver version " <> d <> " men den senaste versionen är " <> r <> "."
    German     -> "Die Abhängigkeit " <> p <> " verlangt Version " <> d <> ", aber die neuste Version ist " <> r <> "."
    Spanish    -> "La dependencia " <> p <> " requiere la versión " <> d <> " pero la versión más reciente es " <> r <> "."
    Portuguese -> "A dependência " <> p <> " exige a versão " <> d <> " mas a versão mais recente é " <> r <> "."
    French     -> p <> " est une dépendance nécessitant la version " <> d <> ", mais la plus récente est la version " <> r <> "."
    Russian    -> "Зависимость " <> p <> " требует версию " <> d <> ", однако самой последней версией является " <> r <> "."
    Italian    -> "La dipendenza " <> p <> " richiede la versione " <> d <> " ma la versione disponibile è " <> r <> "."
    Serbian    -> "Зависност " <> p <> " захтева верзију " <> d <> ", али најновија верзија је " <> r <> "."
    Norwegian  -> "Avhengigheten " <> p <> " krever versjon " <> d <>", men den nyeste versjonen er " <> r <> "."
    Indonesia  -> "Dependensi " <> p <> " meminta versi " <> d <> " namun versi paling baru adalah " <> r <> "."
    Chinese    -> "依赖 " <> p <> " 需要版本 " <> d <> "，但是最新的版本是 " <> r <> "。"
    _          -> "The package " <> prnt <> " depends on version " <> d <> " of " <> p <> ", but the most recent version is " <> r <> "."

getRealPkgConflicts_2 :: T.Text -> Language -> Doc AnsiStyle
getRealPkgConflicts_2 (bt -> p) = \case
    Japanese   -> p <> "は無視されるパッケージ！`pacman.conf`を参考に。"
    Polish     -> p <> " jest ignorowany! Sprawdź plik `pacman.conf`."
    Croatian   -> p <> " je ignoriran paket! Provjerite svoj `pacman.conf`."
    Swedish    -> p <> " är ett ignorerat paket! Kolla din `pacman.conf`-fil."
    German     -> p <> " ist ein ignoriertes Paket! Siehe /etc/pacman.conf."
    Spanish    -> "¡" <> p <> " es un paquete ignorado! Revisa tu fichero `pacman.conf`."
    Portuguese -> p <> " é um pacote ignorado conforme configuração em `pacman.conf`!"
    French     -> "Le paquet " <> p <> " est ignoré. Vous devriez jeter un œil à votre `pacman.conf`."
    Russian    -> "Пакет " <> p <> " игнорируется! Проверьте ваш файл `pacman.conf`."
    Italian    -> p <> " è un pacchetto ignorato, controllare `pacman.conf`."
    Serbian    -> "Пакет " <> p <> " је игнорисан! Видите ваш фајл „pacman.conf“."
    Norwegian  -> p <> " er en ignorert pakke! Sjekk din `pacman.conf`-fil."
    Indonesia  -> p <> " merupakan paket yang diabaikan! Lihat `pacman.conf` anda."
    Chinese    -> p <> " 是一个被忽略的包！请查看你的 `pacman.conf` 文件。"
    _          -> p <> " is an ignored package! See your `pacman.conf` file."

missingPkg_2 :: [DepError] -> Language -> Doc AnsiStyle
missingPkg_2 ps l = vsep $ map (depError l) ps

depError :: Language -> DepError -> Doc AnsiStyle
depError _ (VerConflict s) = s
depError _ (Ignored s)     = s
depError l (NonExistant s) = case l of
  Portuguese -> "A dependência " <> bt s <> " não foi encontrada."
  Russian    -> "Зависимость " <> bt s <> " не найдена."
  _          -> "The dependency " <> bt s <> " couldn't be found."
depError l (UnparsableVersion s) = case l of
  Portuguese -> "A versão de " <> bt s <> " não pôde ser interpretada."
  Russian    -> "Версия для " <> bt s <> " не распознана."
  _          -> "The version number for " <> bt s <> " couldn't be parsed."
depError l (BrokenProvides pkg pro name) = case l of
  Russian    -> "Пакету " <> bt pkg <> " требуется " <> bt name <> ", предоставляющий " <> bt pro <> "."
  _          -> "The package " <> bt pkg <> " needs " <> bt name <> ", which provides " <> bt pro <> "."

missingPkg_3 :: Language -> Doc AnsiStyle
missingPkg_3 = \case
  _ -> "There was an error reorganizing the dependency graph. If you see this, something is very wrong."

-----------------
-- aura functions
-----------------
displayOutputLanguages_1 :: Language -> Doc AnsiStyle
displayOutputLanguages_1 = \case
    Japanese   -> "aura は下記の言語に対応しています："
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
    Indonesia  -> "Berikut ini adalah bahasa yang tersedia:"
    Chinese    -> "以下语言是可用的："
    _          -> "The following languages are available:"

----------------------------
-- Aura/Commands/A functions
----------------------------
-- NEEDS TRANSLATION
auraCheck_1 :: Language -> Doc AnsiStyle
auraCheck_1 = \case
    Japanese   -> "Aura が更新されています。Auraだけ先に更新しますか？"
    Polish     -> "Dostępna jest nowa wersja Aura. Czy chcesz ją najpierw aktualizować?"
    Croatian   -> "Dostupna je nova verzija Aura. Želite li prvo ažurirati?"
    German     -> "Ein Update für aura ist verfügbar. Dies zuerst aktualisieren?"
    Spanish    -> "Hay una actualización de aura disponible. ¿Deseas actualizar aura primero?"
    Norwegian  -> "En Aura-oppdatering er tilgjengelig. Oppdater den først?"
    Portuguese -> "Uma atualização para Aura está disponível. Deseja atualizar antes?"
    French     -> "Une mise à jour d'Aura est disponible. Voulez-vous la mettre à jour en premier ?"
    Russian    -> "Доступно обновление Aura. Обновить сперва её?"
    Indonesia  -> "Pemutakhiran aura tersedia. Mutakhirkan aura dulu?"
    Chinese    -> "Aura 可以升级。先升级 aura？"
    Swedish    -> "Det finns en uppdatering tillgänglig till Aura. Vill du uppdatera Aura först?"
    _          -> "Aura update available. Update it first?"

install_2 :: Language -> Doc AnsiStyle
install_2 = \case
    Japanese   -> "適切なパッケージを入力してください。"
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
    Indonesia  -> "Tidak ada paket valid yang dispesifikkan."
    Chinese    -> "没有指定有效的包。"
    _          -> "No valid packages specified."

install_3 :: Language -> Doc AnsiStyle
install_3 = \case
    Japanese   -> "続行しますか？"
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
    Indonesia  -> "Lanjut?"
    Chinese    -> "继续？"
    _          -> "Continue?"

install_4 :: Language -> Doc AnsiStyle
install_4 = \case
    Japanese   -> "続行は意図的に阻止されました。"
    Polish     -> "Instalacja została przerwana przez użytkownika."
    Croatian   -> "Instalacija prekinuta od strane korisnika."
    Swedish    -> "Installationen avbröts manuellt."
    German     -> "Installation durch Benutzer abgebrochen."
    Spanish    -> "Instalación abortada manualmente."
    Portuguese -> "Instalação cancelada manualmente."
    French     -> "Installation manuelle annulée."
    Russian    -> "Пользователь прервал установку."
    Italian    -> "Installazione manuale interrotta."
    Serbian    -> "Инсталација је ручно прекинута."
    Norwegian  -> "Installasjonen ble avbrutt manuelt."
    Indonesia  -> "Instalasi dibatalkan secara paksa."
    Chinese    -> "手动安装已中止。"
    _          -> "Installation manually aborted."

install_5 :: Language -> Doc AnsiStyle
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
    Indonesia  -> "Menentukan dependensi..."
    Chinese    -> "确定依赖中..."
    _          -> "Determining dependencies..."

-- 2014 December  7 @ 14:45 - NEEDS TRANSLATIONS
confirmIgnored_1 :: T.Text -> Language -> Doc AnsiStyle
confirmIgnored_1 (bt -> p) = \case
    Japanese   -> p <> "は無視されるはずのパッケージです。それでも続行しますか？"
    Polish     -> p <> " jest oznaczony jako ignorowany. Zainstalować mimo tego?"
    Spanish    -> p <> " está marcado como ignorado. ¿Deseas instalarlo de todas formas?"
    Portuguese -> p <> " está marcado como Ignorado. Deseja instalar mesmo assim?"
    Russian    -> p <> " отмечен как игнорируемый. Всё равно установить?"
    Chinese    -> p <> " 已被标记为忽略。仍然安装？"
    Swedish    -> p <> " är markerad som ignorerad. Vill du installera ändå?"
    _          -> p <> " is marked as Ignored. Install anyway?"

-- NEEDS UPDATE TO REFLECT CHANGED ENGLISH
reportNonPackages_1 :: Language -> Doc AnsiStyle
reportNonPackages_1 = \case
    Japanese   -> "下記はAURパッケージではありません："
    Polish     -> "To nie są pakiety:"
    Croatian   -> "Ovo nisu AUR paketi:"
    Swedish    -> "Följande är inte paket:"
    German     -> "Folgende sind keine AUR-Pakete:"
    Spanish    -> "Los siguientes paquetes no son de AUR:"
    Portuguese -> "Os seguintes não são pacotes AUR:"
    French     -> "Les éléments suivants ne sont pas des paquets AUR :"
    Russian    -> "Ниже указано то, что не является пакетами AUR:"
    Italian    -> "I seguenti pacchetti non sono presenti in AUR:"
    Serbian    -> "Ово нису пакети:"
    Norwegian  -> "Det følgende er ikke AUR-pakker:"
    Indonesia  -> "Paket berikut ini bukan merupakan paket AUR:"
    Chinese    -> "以下软件不是 AUR 包："
    _          -> "The following are not AUR packages:"

-- NEEDS TRANSLATION
reportUnneededPackages_1 :: Language -> Doc AnsiStyle
reportUnneededPackages_1 = \case
    Japanese   -> "下記のパッケージは既にインストールされています："
    Polish     -> "Następujące pakiety zostały już zainstalowane:"
    Portuguese -> "Os seguintes pacotes já estão instalados:"
    Russian    -> "Следующие пакеты уже установлены:"
    German     -> "Die folgenden Pakete sind bereits installiert:"
    Spanish    -> "Los siguientes paquetes ya están instalados:"
    Chinese    -> "以下包已被安装："
    Swedish    -> "Följande paket är redan installerade:"
    _          -> "The following packages are already installed:"

reportPkgsToInstall_1 :: Language -> Doc AnsiStyle
reportPkgsToInstall_1 = \case
    Japanese   -> "Pacmanの従属パッケージ："
    Polish     -> "Zależności z repozytoriów:"
    Croatian   -> "Zavisnosti iz repozitorija:"
    Swedish    -> "Beroenden ifrån lager:"
    German     -> "Abhängigkeiten in den Paketquellen:"
    Spanish    -> "Dependencias del repositorio:"
    Portuguese -> "Dependências no repositório:"
    French     -> "Dépendances du dépôt :"
    Russian    -> "Зависимости из репозитория:"
    Italian    -> "Dipendenze nei repository:"
    Serbian    -> "Зависности из ризница:"
    Norwegian  -> "Avhengigheter fra depotet:"
    Indonesia  -> "Dependensi dari repositori:"
    Chinese    -> "仓库依赖："
    _          -> "Repository dependencies:"

-- NEEDS AN UPDATE
reportPkgsToInstall_2 :: T.Text -> Language -> Doc AnsiStyle
reportPkgsToInstall_2 (pretty -> l) = \case
    Japanese   -> l <> "のパッケージ:"
    Polish     -> l <> " Pakiety:"
    Croatian   -> l <> " Paketi:"
    German     -> l <> " Pakete:"
    Spanish    -> l <> " Paquetes:"
    Norwegian  -> l <> " Pakker:"
    Italian    -> l <> " Pacchetti:"
    Portuguese -> l <> " Pacotes:"
    French     -> l <> " Paquets :"
    Russian    -> l <> " Пакеты:"
    Indonesia  -> l <> " Paket:"
    Chinese    -> l <> " 包："
    Swedish    -> l <> " Paket:"
    _          -> l <> " Packages:"

reportPkgsToInstall_3 :: Language -> Doc AnsiStyle
reportPkgsToInstall_3 = \case
    Japanese   -> "AURの従属パッケージ："
    Polish     -> "Zależności z AUR:"
    Croatian   -> "Zavisnosti iz AUR-a:"
    Swedish    -> "Beroenden ifrån AUR:"
    German     -> "Abhängigkeiten im AUR:"
    Spanish    -> "Dependencias en AUR:"
    Portuguese -> "Dependências no AUR:"
    French     -> "Dépendances AUR\xa0:"
    Russian    -> "Зависимости из AUR:"
    Italian    -> "Dipendenze in AUR:"
    Serbian    -> "Зависности из AUR-а:"
    Norwegian  -> "Avhengigheter fra AUR:"
    _          -> "AUR dependencies:"

-- NEEDS TRANSLATION
reportPkgbuildDiffs_1 :: T.Text -> Language -> Doc AnsiStyle
reportPkgbuildDiffs_1 (bt -> p) = \case
    Japanese   -> p <> "のPKGBUILDはまだ保存されていません。"
    Polish     -> p <> " nie ma jeszcze przechowywanego pliku PKGBUILD."
    Croatian   -> p <> " još nema pohranjen PKGBUILD."
    German     -> p <> " hat noch keinen gespeicherten PKGBUILD."
    Spanish    -> p <> " no tiene un PKGBUILD almacenado aún."
    Portuguese -> p <> " não possui PKGBUILD."
    French     -> p <> " n'a pas encore de PKGBUILD enregistré."
    Russian    -> "У " <> p <> " ещё нет сохраненного PKGBUILD."
    Italian    -> p <> " non ci sono PKGBUILD salvati"
    Serbian    -> p <> " још нема похрањен PKGBUILD."
    Norwegian  -> p <> " har ingen PKGBUILD ennå."
    Indonesia  -> p <> " tidak mempunyai PKGBUILD yang tersimpan untuk saat ini."
    Chinese    -> p <> " 还没有保存的 PKGBUILD。"
    Swedish    -> p <> " har ännu ingen PKGBUILD."
    _          -> p <> " has no stored PKGBUILD yet."

-- NEEDS TRANSLATION
reportPkgbuildDiffs_2 :: T.Text -> Language -> Doc AnsiStyle
reportPkgbuildDiffs_2 (bt -> p) = \case
    Japanese   -> p <> "のPKGBUILDは最新です。"
    Polish     -> "PKGBUILD pakietu " <> p <> " jest aktualny."
    Croatian   -> "PKGBUILD paketa " <> p <> " je na najnovijoj verziji."
    German     -> "PKGBUILD von " <> p <> " ist aktuell."
    Spanish    -> "El PKGBUILD de " <> p <> " está actualizado."
    Portuguese -> "O PKGBUILD de " <> p <> "está atualizado."
    Russian    -> "PKGBUILD " <> p <> " является новейшим."
    French     -> "Le PKGBUILD de " <> p <> " est à jour."
    Italian    -> "Il PKGBUILD di " <> p <> " è aggiornato."
    Serbian    -> "PKGBUILD пакета " <> p <> " је ажуран."
    Norwegian  -> p <> "'s PKGBUILD er oppdatert."
    Indonesia  -> "PKGBUILD dari paket " <> p <> " sudah mutakhir."
    Chinese    -> p <> " 的 PKGBUILD 已经最新。"
    Swedish    -> "PKGBUILD för " <> p <> " är aktuell."
    _          -> p <> " PKGBUILD is up to date."

-- NEEDS TRANSLATION
reportPkgbuildDiffs_3 :: T.Text -> Language -> Doc AnsiStyle
reportPkgbuildDiffs_3 (bt -> p) = \case
    Japanese   -> p <> "のPKGBUILD変更報告："
    Polish     -> "Zmiany w PKGBUILD dla " <> p <> ":"
    Croatian   -> "Promjene u PKGBUILD-u za " <> p <> ":"
    German     -> "PKGBUILD-Änderungen von " <> p <> ":"
    Spanish    -> "Cambios en el PKGBUILD de " <> p <> ":"
    Portuguese -> "Mudanças no PKGBUILD de " <> p <> ":"
    Russian    -> "Изменения, вносимые " <> p <> " PKGBUILD:"
    French     -> "Changements du PKGBUILD de " <> p <> " :"
    Italian    -> "Cambiamenti nel PKGBUILD di " <> p <>":"
    Serbian    -> "Промене PKGBUILD-a за " <> p <> ":"
    Norwegian  -> p <> "'s endringer i PKGBUILD:"
    Indonesia  -> "Perubahan PKGBUILD " <> p <> ":"
    Chinese    -> p <> " 的 PKGBUILD 变化："
    Swedish    -> "Förändringar i PKGBUILD för " <> p <> ":"
    _          -> p <> " PKGBUILD changes:"

-- NEEDS TRANSLATION
reportPkgsToUpgrade_1 :: Language -> Doc AnsiStyle
reportPkgsToUpgrade_1 = \case
    Japanese   -> "アップグレードするAURパッケージ："
    Polish     -> "Pakiety z AUR do zaktualizowania:"
    Croatian   -> "AUR paketi za nadogradnju:"
    Swedish    -> "AUR-paket att uppgradera:"
    German     -> "Zu aktualisierendes AUR-Paket:"
    Spanish    -> "Paquetes de AUR a actualizar:"
    Portuguese -> "Pacotes do AUR para atualizar:"
    French     -> "Paquets AUR à mettre à jour :"
    Russian    -> "Пакеты AUR, готовые для обновления:"
    Italian    -> "Pacchetti in AUR da aggiornare:"
    Serbian    -> "Пакети из AUR-а за надоградњу:"
    Norwegian  -> "AUR-pakker å oppgradere:"
    Indonesia  -> "Paket AUR yang akan ditingkatkan:"
    Chinese    -> "要升级的 AUR 包："
    _          -> "AUR Packages to upgrade:"

-- NEEDS UPDATING
reportBadDowngradePkgs_1 :: Language -> Doc AnsiStyle
reportBadDowngradePkgs_1 = \case
    Japanese   -> "このパッケージはキャッシュには入っていないので、ダウングレードできません。"
    Polish     -> "Poniższe pakeity nie są zainstalowane, i nie mogą być zainstalowane w starszej wersji:"
    Croatian   -> "Sljedeći paketi nisu instalirani te se stoga ne mogu vratiti na stare verzije:"
    Swedish    -> "Följande paket är inte installerade, och kan därför inte bli nergraderade:"
    German     -> "Folgende Pakete sind in keiner Version im Cache und können daher nicht gedowngradet werden:"
    Spanish    -> "Los siguientes paquetes no tienen versiones en la caché, por lo que no se pueden bajar a versiones anteriores:"
    Portuguese -> "Os seguintes pacotes não possuem versões no cache, logo não podem retornar a uma versão anterior:"
    French     -> "Aucune version des paquets suivants n'est présente dans le cache ; ils ne peuvent pas être mis à niveau à une version antérieure :"
    Russian    -> "Следующих пакетов нет в кэше. Следовательно, они не могут быть откачены к старой версии:"
    Italian    -> "I seguenti pacchetti non hanno versioni in cache e non posso essere retrocessi:"
    Serbian    -> "Следећи пакети нису ни инсталирани, те се не могу вратити на старију верзију:"
    Norwegian  -> "Følgende pakker har ingen versjoner i cache, og kan derfor ikke bli nedgradert:"
    Indonesia  -> "Berikut ini tidak mempunyai versi pada cache, sehingga tidak akan diturunkan:"
    Chinese    -> "以下包在缓存中没有版本，所以无法被降级："
    _          -> "The following have no versions in the cache, and thus can’t be downgraded:"

reportBadDowngradePkgs_2 :: T.Text -> Language -> Doc AnsiStyle
reportBadDowngradePkgs_2 p = \case
  _ -> pretty p <+> "has no version in the cache."

upgradeAURPkgs_1 :: Language -> Doc AnsiStyle
upgradeAURPkgs_1 = \case
    Japanese   -> "パッケージ情報をダウンロード中・・・"
    Polish     -> "Pobieranie informacji o pakietach..."
    Croatian   -> "Preuzimanje podataka o paketima..."
    Swedish    -> "Hämtar paketinformation..."
    German     -> "Rufe Paketinformationen ab..."
    Spanish    -> "Obteniendo información de los paquetes..."
    Portuguese -> "Obtendo informação dos pacotes..."
    French     -> "Obtention des informations des paquets en cours…"
    Russian    -> "Сборка информации о пакетах..."
    Italian    -> "Ottengo le informazioni del pacchetto..."
    Serbian    -> "Преузимање информација о пакетима..."
    Norwegian  -> "Henter pakkeinformasjon..."
    Indonesia  -> "Mengambil informasi paket..."
    Chinese    -> "正在获取包信息..."
    _          -> "Fetching package information..."

upgradeAURPkgs_2 :: Language -> Doc AnsiStyle
upgradeAURPkgs_2 = \case
    Japanese   -> "バージョンを比較中・・・"
    Polish     -> "Porównywanie wersji pakietów..."
    Croatian   -> "Uspoređivanje verzija paketa..."
    Swedish    -> "Jämför paket-versioner..."
    German     -> "Vergleiche Paketversionen..."
    Spanish    -> "Comparando versiones de los paquetes..."
    Portuguese -> "Comparando versões dos pacotes..."
    French     -> "Comparaison des versions des paquets en cours…"
    Russian    -> "Сравнение версий пакетов..."
    Italian    -> "Confronto le ersioni del pacchetto..."
    Serbian    -> "Упоређивање верзија пакета..."
    Norwegian  -> "Sammenligner pakkeversjoner..."
    Indonesia  -> "Membandingkan versi paket..."
    Chinese    -> "正在比较包的版本..."
    _          -> "Comparing package versions..."

upgradeAURPkgs_3 :: Language -> Doc AnsiStyle
upgradeAURPkgs_3 = \case
    Japanese   -> "アップグレードは必要ありません。"
    Polish     -> "Nie jest wymagana aktualizacja pakietów z AUR."
    Croatian   -> "Svi AUR paketi su ažurirani."
    Swedish    -> "Inga AUR-paketsuppgraderingar behövs."
    German     -> "Keine Aktualisierungen für AUR-Paket notwendig."
    Spanish    -> "No es necesario actualizar paquetes de AUR."
    Portuguese -> "Nenhum pacote do AUR precisa de atualização."
    French     -> "Aucune mise à jour de paquet AUR n'est nécessaire."
    Russian    -> "Обновление пакетов из AUR не требуется."
    Italian    -> "Non è necessario aggiornare pacchetti di AUR."
    Serbian    -> "Ажурирање пакета из AUR-а није потребно."
    Norwegian  -> "Ingen pakkeoppgradering fra AUR nødvendig."
    Indonesia  -> "Tidak ada peningkatan AUR yang dibutuhkan."
    Chinese    -> "没有需要升级的 AUR 包。"
    _          -> "No AUR package upgrades necessary."

removeMakeDepsAfter_1 :: Language -> Doc AnsiStyle
removeMakeDepsAfter_1 = \case
    Japanese   -> "あと片付け。必要ないパッケージを削除："
    Polish     -> "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."
    Croatian   -> "Uklanjanje nepotrebnih zavisnosti vezanih uz izgradnju..."
    Swedish    -> "Tar bort obehövda beroenden för `make`..."
    German     -> "Entferne nicht benötigte make-Abhängigkeiten..."
    Spanish    -> "Removiendo dependencias `make` innecesarias..."
    Portuguese -> "Removendo dependências `make` desnecessárias..."
    French     -> "Suppression des dépendances inutiles…"
    Russian    -> "Удаление ненужных зависимостей make..."
    Italian    -> "Rimuovo le dipendenze di compilazione..."
    Serbian    -> "Уклањање непотребних зависности за изградњу..."
    Norwegian  -> "Fjerner unødvendige make-avhengigheter..."
    Indonesia  -> "Menghapus dependensi `make` yang tidak dibutuhkan..."
    Chinese    -> "移除不需要的 make 依赖..."
    _          -> "Removing unneeded make dependencies..."

----------------------------
-- Aura/Commands/B functions
----------------------------
-- NEEDS TRANSLATION
cleanStates_2 :: Int -> Language -> Doc AnsiStyle
cleanStates_2 n@(bt . T.pack . show -> s) = \case
    Japanese   -> s <> "個のパッケージ状態記録だけが残される。その他削除？"
    Polish     -> s <> " stan pakietów zostanie zachowany. Usunąć resztę?"
    Croatian   -> s <> " stanja paketa će biti zadržano. Ukloniti ostatak?"
    German     -> s <> " Paketzustände werden behalten. Den Rest entfernen?"
    Spanish    -> "El estado del paquete" <> s <> " se mantendrá. ¿Deseas eliminar el resto?"
    Serbian    -> s <> " стања пакета ће бити сачувано. Уклонити остатак?"
    Norwegian  -> s <> " pakketilstander vil bli beholdt. Vil du fjerne resten?"
    Italian    -> s <> " lo stato dei pacchetti sarà mantenuto. Rimuovere i rimanenti?"
    Portuguese -> s <> " estados de pacotes serão mantidos. Remover o resto?"
    French     -> s <> " états des paquets vont être conservés. Supprimer le reste ?"
    Russian    -> s <> (pluralRussian " состояние пакетов будет оставлено." " состояния пакетов будут оставлены." " состояний пакетов будет оставлено." n) <> " Удалить оставшиеся?"
    Indonesia  -> s <> " paket akan tetap sama. Hapus yang lainnya?"
    Chinese    -> s <> " 个包的状态将会保留。删除其它的？"
    Swedish    -> s <> " paket kommer att bevaras. Ta bort resten?"
    _          -> s <> " package states will be kept. Remove the rest?"

-- NEEDS TRANSLATION
cleanStates_3 :: Language -> Doc AnsiStyle
cleanStates_3 = \case
    Japanese   -> "何も削除しないで終了します。"
    Polish     -> "Żaden stan pakietu nie został usunięty."
    Croatian   -> "Nijedno stanje paketa nije uklonjeno."
    German     -> "Keine Paketzustände wurden entfernt."
    Spanish    -> "No se han eliminado estados de los paquetes."
    Serbian    -> "Ниједно стање пакета није уклоњено."
    Norwegian  -> "Ingen pakketilstander ble fjernet."
    Italian    -> "Nessuno stato di pacchetto verrà rimosso."
    Portuguese -> "Nenhum estado de pacote será removido."
    French     -> "Aucun état des paquets n'a été supprimé."
    Russian    -> "Состояния пакетов отались нетронутыми."
    Indonesia  -> "Tidak ada paket yang dihapus."
    Chinese    -> "没有删除任何包。"
    Swedish    -> "Inga paket togs bort."
    _          -> "No package states were removed."

cleanStates_4 :: Int -> Language -> Doc AnsiStyle
cleanStates_4 n = \case
  Japanese -> "現在のパッケージ状態記録：" <> pretty n <> "個。"
  Russian  -> "У вас сейчас " <+> pretty n <+> (pluralRussian " сохраненное состояние пакета" " сохраненных состояний пакета" " сохраненных состояний пакетов." n)
  _        -> "You currently have" <+> pretty n <+> "saved package states."

cleanStates_5 :: T.Text -> Language -> Doc AnsiStyle
cleanStates_5 t = \case
  Japanese -> "一番最近に保存されたのは：" <> pretty t
  Russian  -> "Последнее сохраненное:" <+> pretty t
  _        -> "Mostly recently saved:" <+> pretty t

cleanStates_6 :: Int -> Language -> Doc AnsiStyle
cleanStates_6 n = \case
  _ -> pretty n <+> "of these are pinned, and won't be removed."

readState_1 :: Language -> Doc AnsiStyle
readState_1 = \case
    Portuguese -> "O arquivo de estado não pôde ser interpretado. É um arquivo JSON válido?"
    Russian    -> "Это состояние не распознано. Это корректный JSON?"
    _          -> "That state file failed to parse. Is it legal JSON?"

----------------------------
-- Aura/Commands/C functions
----------------------------
getDowngradeChoice_1 :: T.Text -> Language -> Doc AnsiStyle
getDowngradeChoice_1 (bt -> p) = \case
    Japanese   -> p <> "はどのバージョンにしますか？"
    Polish     -> "Którą wersję pakietu " <> p <> " zainstalować?"
    Croatian   -> "Koju verziju paketa " <> p <> " želite?"
    Swedish    -> "Vilken version av " <> p <> " vill du ha?"
    German     -> "Welche Version von " <> p <> " möchten Sie haben?"
    Spanish    -> "¿Qué versión de " <> p <> " deseas?"
    Portuguese -> "Qual versão de " <> p <> " deseja?"
    French     -> "Quelle version de " <> p <> " voulez-vous ?"
    Russian    -> "Какую версию " <> p <> " вы хотите?"
    Italian    -> "Quale versione di " <> p <> " preferisci?"
    Serbian    -> "Коју верзију " <> p <> "-а желите?"
    Norwegian  -> "Hvilken versjon av " <> p <> " vil du ha?"
    Indonesia  -> "Versi dari paket " <> p <> " mana yang anda inginkan?"
    Chinese    -> "你希望安装 " <> p <> " 的哪个版本？"
    _          -> "What version of " <> p <> " do you want?"

backupCache_3 :: Language -> Doc AnsiStyle
backupCache_3 = \case
    Japanese   -> "バックアップ先は存在しません。"
    Polish     -> "Lokalizacja kopii zapasowych nie istnieje."
    Croatian   -> "Lokacija sigurnosne kopije ne postoji."
    Swedish    -> "Specifierad backup-plats finns inte."
    German     -> "Der Sicherungsort existiert nicht."
    Spanish    -> "La localización para copia de seguridad no existe."
    Portuguese -> "Localização do backup não existe."
    French     -> "Le chemin des copies de sauvegarde spécifié n'existe pas."
    Russian    -> "Путь к бэкапу не существует."
    Italian    -> "L'indirizzo del salvataggio non esiste."
    Serbian    -> "Путања ка бекапу не постоји."
    Norwegian  -> "Spesifisert backup-plass finnes ikke."
    Indonesia  -> "Lokasi `backup` tidak ada."
    Chinese    -> "备份位置不存在。"
    _          -> "The backup location does not exist."

backupCache_4 :: FilePath -> Language -> Doc AnsiStyle
backupCache_4 (bt . T.pack -> dir) = \case
    Japanese   -> "キャッシュのバックアップ先：" <> dir
    Polish     -> "Tworzenie kopii zapasowej pamięci podręcznej w " <> dir
    Croatian   -> "Stvaram sigurnosnu kopiju u " <> dir
    Swedish    -> "Tar backup på cache-filer till " <> dir
    German     -> "Sichere Cache in " <> dir
    Spanish    -> "Haciendo una copia de seguridad de la caché en " <> dir
    Portuguese -> "Backup do cache sendo feito em " <> dir
    French     -> "Copie de sauvegarde dans " <> dir <> "."
    Russian    -> "Бэкап создается в директории " <> dir
    Italian    -> "Salvataggio della chace in " <> dir
    Serbian    -> "Бекапујем кеш у " <> dir
    Norwegian  -> "Tar backup på cache til " <> dir
    Indonesia  -> "Melakukan `backup` pada direktori " <> dir
    Chinese    -> "正在将缓存备份到 " <> dir
    _          -> "Backing up cache to " <> dir

backupCache_5 :: Int -> Language -> Doc AnsiStyle
backupCache_5 (bt . T.pack . show -> n) = \case
    Japanese   -> "パッケージのファイル数：" <> n
    Polish     -> "Pliki będące częścią\xa0kopii zapasowej: " <> n
    Croatian   -> "Datoteke koje su dio sigurnosne kopije: " <> n
    Swedish    -> "Paket-filer att ta backup på: " <> n
    German     -> "Zu sichernde Paketdateien: " <> n
    Spanish    -> "Ficheros de paquetes de los que se hará copia de seguridad: " <> n
    Portuguese -> "Arquivos de pacotes para backup: " <> n
    French     -> "Copie de sauvegarde des fichiers de paquets suivants : " <> n
    Russian    -> "Файлы пакета для бэкапа: " <> n
    Italian    -> "File del pacchetto da salvare: " <> n
    Serbian    -> "Датотеке за бекап: " <> n
    Norwegian  -> "Pakker som blir tatt backup på: " <> n
    Indonesia  -> "Jumlah paket yang di-`backup`: " <> n
    Chinese    -> "将要备份的包文件：" <> n
    _          -> "Package files to backup: " <> n

backupCache_6 :: Language -> Doc AnsiStyle
backupCache_6 = \case
    Japanese   -> "バックアップを実行しますか？"
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
    Indonesia  -> "Lanjutkan dengan `backup`?"
    Chinese    -> "开始备份？"
    _          -> "Proceed with backup?"

backupCache_7 :: Language -> Doc AnsiStyle
backupCache_7 = \case
    Japanese   -> "バックアップは意図的に阻止されました。"
    Polish     -> "Tworzenie kopii zapasowej zostało przerwane przez użytkownika."
    Croatian   -> "Stvaranje sigurnosne kopije prekinuto od strane korisnika."
    Swedish    -> "Backup avbröts manuellt."
    German     -> "Backup durch Benutzer abgebrochen."
    Spanish    -> "Copia de seguridad abortada manualmente."
    Portuguese -> "Backup cancelado manualmente."
    French     -> "Copie de sauvegarde manuelle annulée."
    Russian    -> "Создание бэкапа прервано пользователем."
    Italian    -> "Salvataggio manuale interrotto."
    Serbian    -> "Бекаповање је ручно прекинуто."
    Norwegian  -> "Backup ble avbrutt manuelt."
    Indonesia  -> "Proses `backup` dibatalkan secara paksa."
    Chinese    -> "手动备份已中止。"
    _          -> "Backup manually aborted."

backupCache_8 :: Language -> Doc AnsiStyle
backupCache_8 = \case
    Japanese   -> "バックアップ中。数分かかるかもしれません。"
    Polish     -> "Tworzenie kopii zapasowej. To może potrwać kilka minut..."
    Croatian   -> "Stvaranje sigurnosne kopije. Ovo može potrajati nekoliko minuta..."
    Swedish    -> "Tar backup. Det här kan ta ett tag..."
    German     -> "Sichere. Dies kann einige Minuten dauern..."
    Spanish    -> "Haciendo copia de seguridad. Esto puede tardar unos minutos..."
    Portuguese -> "Efetuando backup. Isso pode levar alguns minutos..."
    French     -> "Copie de sauvegarde en cours. Ceci peut prendre quelques minutes…"
    Russian    -> "Создается бэкап. Это может занять пару минут..."
    Italian    -> "Salvataggio. Questo potrebbe richiedere qualche minuto..."
    Serbian    -> "Бекапујем. Ово може да потраје пар минута..."
    Norwegian  -> "Tar backup. Dette kan ta en stund..."
    Indonesia  -> "Melakukan `backup`. Proses ini akan berjalan untuk beberapa menit..."
    Chinese    -> "正在备份中。可能需要几分钟的时间..."
    _          -> "Backing up. This may take a few minutes..."

copyAndNotify_1 :: Int -> Language -> Doc AnsiStyle
copyAndNotify_1 (cyan . pretty -> n) = \case
    Japanese   -> "#[" <> n <> "]をコピー中・・・"
    Polish     -> "Kopiowanie #[" <> n <> "]"
    Croatian   -> "Kopiranje #[" <> n <> "]"
    Swedish    -> "Kopierar #[" <> n <> "]"
    German     -> "Kopiere #[" <> n <> "]"
    Spanish    -> "Copiando #[" <> n <> "]"
    Portuguese -> "Copiando #[" <> n <> "]"
    French     -> "Copie de #[" <> n <> "]"
    Russian    -> "Копируется #[" <> n <> "]"
    Italian    -> "Copiando #[" <>n <> "]"
    Serbian    -> "Копирам #[" <> n <> "]"
    Norwegian  -> "Kopierer #[" <> n <> "]"
    Indonesia  -> "Menyalin #[" <> n <> "]"
    Chinese    -> "正在复制 #[" <> n <> "]"
    _          -> "Copying #[" <> n <> "]"

cleanCache_2 :: Language -> Doc AnsiStyle
cleanCache_2 = \case
    Japanese   -> "パッケージ・キャッシュは完全に削除されます。"
    Polish     -> "To usunie WSZYSTKIE pakiety z pamięci podręcznej."
    Croatian   -> "Ovo će izbrisati CIJELI cache paketa."
    Swedish    -> "Detta kommer ta bort HELA paket-cachen."
    German     -> "Dies wird den GESAMTEN Paketcache leeren."
    Spanish    -> "Esto eliminará POR COMPLETO la caché de paquetes."
    Portuguese -> "Isso removerá TODOS OS PACOTES do cache."
    French     -> "Ceci va supprimer la TOTALITÉ du cache des paquets."
    Russian    -> "Это действие ВСЕЦЕЛО уничтожит кэш пакетов."
    Italian    -> "Questo cancellera l'INTERA cache dei pacchetti."
    Serbian    -> "Ово ће избрисати ЦЕО кеш пакета."
    Norwegian  -> "Dette vil slette HELE pakke-cachen."
    Indonesia  -> "Akan menghapus SEMUA `cache` paket"
    Chinese    -> "这将会删除全部的包缓存。"
    _          -> "This will delete the ENTIRE package cache."

cleanCache_3 :: Word -> Language -> Doc AnsiStyle
cleanCache_3 n@(bt . T.pack . show -> s) = \case
    Japanese   -> "パッケージ・ファイルは" <> s <> "個保存されます。"
    Polish     -> s <> " wersji każdego pakietu zostanie zachowane."
    Croatian   -> s <> " zadnjih verzija svakog paketa će biti zadržano."
    Swedish    -> s <> " av varje paketfil kommer att sparas."
    German     -> s <> " jeder Paketdatei wird behalten."
    Spanish    -> "Se mantendrán " <> s <> " ficheros de cada paquete."
    Portuguese -> s <> " arquivos de cada pacote serão mantidos."
    French     -> s <> " fichiers de chaque paquet sera conservé."
    Russian    -> s <> pluralRussian " версия каждого пакета будет нетронута." " версии каждого пакета будут нетронуты." " версий каждого пакета будут нетронуты." n
    Italian    -> s <> " di ciascun pacchetto sarà mantenuto."
    Serbian    -> s <> " верзије сваког од пакета ће бити сачуване."
    Norwegian  -> s <> " av hver pakkefil blir beholdt."
    Indonesia  -> s <> " berkas dari tiap paket akan disimpan."
    Chinese    -> "每个包文件将会保存 " <> s <> " 个版本。"
    _          -> s <> " of each package file will be kept."

cleanCache_4 :: Language -> Doc AnsiStyle
cleanCache_4 = \case
    Japanese   -> "残りは全部削除されます。承知していますか？"
    Polish     -> "Wszystko inne zostanie usunięte. Na pewno?"
    Croatian   -> "Ostali paketi će biti izbrisani. Jeste li sigurni?"
    Swedish    -> "Resten kommer att tas bort. Är det OK?"
    German     -> "Der Rest wird gelöscht. Ist das OK?"
    Spanish    -> "El resto se eliminarán. ¿De acuerdo?"
    Portuguese -> "O resto será removido. OK?"
    French     -> "Le reste sera supprimé. Êtes-vous d'accord ?"
    Russian    -> "Всё остальное будет удалено. Годится?"
    Italian    -> "Il resto verrà mantenuto. Continuare?"
    Serbian    -> "Остатак ће бити избрисан. Да ли је то у реду?"
    Norwegian  -> "Resten vil bli slettet. Er det OK?"
    Indonesia  -> "Selainnya akan dihapus. Ikhlas kan?"
    Chinese    -> "其余的将会被删除。确定？"
    _          -> "The rest will be deleted. Okay?"

cleanCache_5 :: Language -> Doc AnsiStyle
cleanCache_5 = \case
    Japanese   -> "削除の続行は意図的に阻止されました。"
    Polish     -> "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."
    Croatian   -> "Čišćenje cache-a paketa prekinuto od strane korisnika."
    Swedish    -> "Cache-rensning avbröts manuellt."
    German     -> "Leeren des Caches durch Benutzer abgebrochen."
    Spanish    -> "Limpieza de la caché abortada manualmente."
    Portuguese -> "Limpeza do cache cancelada manualmente."
    French     -> "Le nettoyage du cache a été arrêté manuellement."
    Russian    -> "Очистка кэша прервана пользователем."
    Italian    -> "Pulitura manuale della cache interrotta."
    Serbian    -> "Чишћење кеша је ручно прекинуто."
    Norwegian  -> "Cache-rensing ble avbrutt manuelt."
    Indonesia  -> "Pembersihan `cache` dibatalkan secara paksa."
    Chinese    -> "手动清理缓存已中止。"
    _          -> "Cache cleaning manually aborted."

cleanCache_6 :: Language -> Doc AnsiStyle
cleanCache_6 = \case
    Japanese   -> "パッケージ・キャッシュを掃除中・・・"
    Polish     -> "Czyszczenie pamięci podręcznej..."
    Croatian   -> "Čišćenje cache-a paketa..."
    Swedish    -> "Rensar paket-cache..."
    German     -> "Leere Paketcache..."
    Spanish    -> "Limpiando la caché de paquetes..."
    Portuguese -> "Limpando cache de pacotes..."
    French     -> "Nettoyage du cache des paquets…"
    Russian    -> "Очистка кэша пакета..."
    Italian    -> "Ripulisco la cache..."
    Serbian    -> "Чишћење кеша..."
    Norwegian  -> "Renser pakke-cache..."
    Indonesia  -> "Membersihkan `cache` paket..."
    Chinese    -> "正在清理包缓存..."
    _          -> "Cleaning package cache..."

-- NEEDS TRANSLATION
cleanNotSaved_1 :: Language -> Doc AnsiStyle
cleanNotSaved_1 = \case
    Japanese   -> "不要パッケージファイルを確認・・・"
    Polish     -> "Określanie niepotrzebnych plków pakietów"
    Croatian   -> "Pronalazim nepotrebne datoteke paketa..."
    German     -> "Bestimme nicht benötigte Paketdateien..."
    Spanish    -> "Determinando ficheros de paquetes innecesarios..."
    Norwegian  -> "Finner unødige pakkefiler..."
    Italian    -> "Determino i pacchetti non più necessari..."
    Portuguese -> "Determinando pacotes não necessários..."
    French     -> "Détermination des fichiers de paquet inutiles…"
    Russian    -> "Вычисляются ненужные файлы пакетов..."
    Indonesia  -> "Menentukan berkas paket yang tidak dibutuhkan..."
    Chinese    -> "正在确定不需要的包文件..."
    Swedish    -> "Beräknar onödiga paketfiler..."
    _          -> "Determining unneeded package files..."

-- NEEDS TRANSLATION
cleanNotSaved_2 :: Int -> Language -> Doc AnsiStyle
cleanNotSaved_2 n@(cyan . pretty -> s) = \case
    Japanese   -> "「" <> s <> "」の不要パッケージファイルがあります。削除しますか？"
    Polish     -> s <> " niepotrzebnych plików zostało znalezionych. Usunąć?"
    Croatian   -> s <> " nepotrebnih datoteka pronađeno. Obrisati?"
    German     -> s <> " nicht benötigte Paketdateien gefunden. Löschen?"
    Spanish    -> s <> " ficheros innecesarios de paquetes encontrados. ¿Deseas eliminarlos?"
    Norwegian  -> s <> " unødige pakkefiler funnet. Vil du slette?"
    Italian    -> s <> " pacchetti non necessari trovati. Cancellarli?"
    Portuguese -> s <> " pacotes não necessários encontrados. Removê-los?"
    French     -> s <> " paquets inutiles trouvés. Les supprimer ?"
    Russian    -> pluralRussian ("Обнаружен " <> s <> " ненужный файл пакета.") ("Обнаружены " <> s <> " ненужных файла пакетов.") ("Обнаружено " <> s <> " ненужных файлов пакетов.") n <> " Удалить?"
    Indonesia  -> s <> " berkas paket yang tidak dibutuhkan ditemukan. Hapus?"
    Chinese    -> "发现了 " <> s <> " 个不需要的包文件。是否删除？"
    Swedish    -> s <> " oanvända paket hittades. Ta bort?"
    _          -> s <> " unneeded package files found. Delete?"

----------------------------
-- Aura/Commands/L functions
----------------------------
logLookUpFields :: Language -> [T.Text]
logLookUpFields = sequence [ Fields.package
                           , Fields.firstInstall
                           , Fields.upgrades
                           , Fields.recentActions ]

reportNotInLog_1 :: Language -> Doc AnsiStyle
reportNotInLog_1 = \case
    Japanese   -> "logファイルには出ていない："
    Polish     -> "Tych pakietów nie ma w dzienniku:"
    Croatian   -> "Ovih paketa nema u dnevniku:"
    Swedish    -> "Dessa har inte framkommit i loggfiler:"
    German     -> "Diese sind nicht in der Logdatei aufgetaucht:"
    Spanish    -> "Estos no aparecen en el fichero log:"
    Portuguese -> "Os seguintes não apareceram no arquivo de log:"
    French     -> "Ceci n'apparaît pas des les journaux (log) :"
    Russian    -> "Следующих пакетов нет в лог-файле:"
    Italian    -> "Questo non apparirà nei file di log;"
    Serbian    -> "Ови пакети се не спомињу у дневнику:"
    Norwegian  -> "Følgende har ikke vist seg i loggen:"
    Indonesia  -> "Tidak terlihat pada berkas log:"
    Chinese    -> "这些没有在日志文件中出现："
    _          -> "These have not appeared in the log file:"

-------------------------------
-- Aura/AUR functions
-------------------------------
infoFields :: Language -> [T.Text]
infoFields = sequence [ Fields.repository
                      , Fields.name
                      , Fields.version
                      , Fields.aurStatus
                      , Fields.maintainer
                      , Fields.projectUrl
                      , Fields.aurUrl
                      , Fields.license
                      , Fields.dependsOn
                      , Fields.buildDeps
                      , Fields.votes
                      , Fields.popularity
                      , Fields.description
                      ]

outOfDateMsg :: Maybe Int -> Language -> Doc AnsiStyle
outOfDateMsg (Just _) = red . \case
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
    Indonesia  -> "Ketinggalan Zaman!"
    Chinese    -> "过期！"
    _          -> "Out of Date!"
outOfDateMsg Nothing = green . \case
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
    Indonesia  -> "Mutakhir"
    Chinese    -> "最新"
    _          -> "Up to Date"

-- NEEDS TRANSLATION
orphanedMsg :: Maybe T.Text -> Language -> Doc AnsiStyle
orphanedMsg (Just m) = const (pretty m)
orphanedMsg Nothing = red . \case
    Japanese   -> "孤児です!"
    Polish     -> "Osierocony!"
    Croatian   -> "Nema roditelja!"
    German     -> "Verwaist!"
    Spanish    -> "¡Huérfano!"
    Norwegian  -> "Foreldreløs!"
    Portuguese -> "Órfão!"
    French     -> "Abandonné !"
    Russian    -> "Осиротевший!"
    Indonesia  -> "Tak dipelihara!"
    Chinese    -> "孤包！"
    Swedish    -> "Föräldralös!"
    _          -> "Orphaned!"

-----------------------
-- Aura/State functions
-----------------------
-- NEEDS TRANSLATION
saveState_1 :: Language -> Doc AnsiStyle
saveState_1 = \case
    Japanese   -> "パッケージ状態の保存完了。"
    Polish     -> "Zachowano stan pakietów"
    Croatian   -> "Stanje paketa spremljeno."
    German     -> "Paketzustand gesichert."
    Spanish    -> "Estado del paquete salvado."
    Serbian    -> "Сачувано стање пакета."
    Norwegian  -> "Lagret pakketilstand."
    Italian    -> "Stato del pacchetto salvato."
    Portuguese -> "Estado de pacote salvo."
    French     -> "État des paquets sauvegardé."
    Russian    -> "Состояние пакетов сохранено."
    Indonesia  -> "Kondisi paket tersimpan."
    Chinese    -> "已保存包状态。"
    Swedish    -> "Det lokala pakettillståndet har sparats."
    _          -> "Saved package state."

-- NEEDS TRANSLATION
restoreState_1 :: Language -> Doc AnsiStyle
restoreState_1 = \case
    Japanese   -> "対象バージョンがないパッケージ："
    Polish     -> "Starsze wersje nie są dostępne dla:"
    Croatian   -> "Tražene stare verzije nisu dostupne za:"
    German     -> "Gewünschte Downgrade-Versionen nicht verfügbar für:"
    Spanish    -> "Versiones anteriores no disponibles para:"
    Serbian    -> "Захтеване старе верзије нису доступне за:"
    Norwegian  -> "De spesifiserte nedgraderingsversjonene er ikke tilgjengelig for:"
    Italian    -> "Richiesta di retrocessione di versione non disponibile per:"
    Portuguese -> "Versões anteriores requisitadas não disponívels para:"
    French     -> "Version antérieure requise non disponible pour :"
    Russian    -> "Запрошенные версии для отката не доступны для:"
    Indonesia  -> "Versi yang diturunkan tidak tersedia untuk: "
    Chinese    -> "请求的降级版本对以下包不可用："
    Swedish    -> "Den begärda nedgraderingen finns inte tillgänglig för:"
    _          -> "Requested downgrade versions not available for:"

restoreState_2 :: Language -> Doc AnsiStyle
restoreState_2 = \case
    Japanese   -> "保存されたパッケージ状態がない。作るには「-B」を。"
    Portuguese -> "Nenhum estado disponível para ser recuperado. (Utilize -B para salvar o estado atual)"
    Russian    -> "Нет сохраненных состояний для восстановления. (Используйте -B для сохранения текущего состояния)"
    Chinese    -> "没有要恢复的已保存状态。（使用 -B 保存当前状态）"
    Swedish    -> "Inga sparade tillstånd att återhämta. (Använd -B för att spara det nuvarande tillståndet)"
    _          -> "No saved states to be restored. (Use -B to save the current state)"

-- NEEDS TRANSLATION
reinstallAndRemove_1 :: Language -> Doc AnsiStyle
reinstallAndRemove_1 = \case
    Japanese   -> "パッケージを変更する必要はありません。"
    Polish     -> "Żaden pakiet nie wymaga zmian"
    Croatian   -> "Nema paketa kojima su potrebne izmjene."
    German     -> "Keine Pakete brauchen Änderungen."
    Spanish    -> "Ningún paquete necesita cambios."
    Serbian    -> "Ниједан пакет не захтева измене."
    Norwegian  -> "Ingen pakker trenger forandring."
    Italian    -> "Nessun pacchetto necessita cambiamenti."
    Portuguese -> "Nenhum pacote requer alteração."
    French     -> "Aucun paquet n'a besoin de changement."
    Russian    -> "Пакеты не нуждаются в изменениях."
    Indonesia  -> "Tidak ada paket yang diubah."
    Chinese    -> "没有包需要改变。"
    Swedish    -> "Inga paket behöver ändras."
    _          -> "No packages need changing."

--------------------------------------
-- Aura/Settings/BadPackages functions
--------------------------------------
whoIsBuildUser_1 :: Language -> Doc AnsiStyle
whoIsBuildUser_1 = \case
    Portuguese -> "Não foi possível determinal o usuário que executará a compilação."
    Russian    -> "Не удается определить, от имени какого пользователя производить сборку."
    _          -> "Can't determine which user account to build with."

------------------------
-- Aura/Pacman functions
------------------------
-- NEEDS TRANSLATION
pacmanFailure_1 :: Language -> Doc AnsiStyle
pacmanFailure_1 = \case
    Japanese   -> "入力を確認して下さい。"
    Polish     -> "Sprawdź swoje dane wejściowe"
    Croatian   -> "Molim vas, provjerite svoj unos."
    German     -> "Bitte überprüfen Sie Ihre Eingabe."
    Spanish    -> "Por favor comprueba los datos proporcionados."
    Serbian    -> "Молим Вас, проверите ваш унос."
    Norwegian  -> "Vennligst sjekk din oppføring."
    Italian    -> "Controllare il proprio input."
    Portuguese -> "Por favor, verifique os dados informados."
    French     -> "Merci de vérifier les donnés entrées."
    Russian    -> "Пожалуйста, проверьте ваши введенные данные."
    Indonesia  -> "Mohon periksa masukan anda."
    Chinese    -> "请检查你的输入。"
    Swedish    -> "Var god dubbelkolla indata."
    _          -> "Please check your input."

confParsing_1 :: Language -> Doc AnsiStyle
confParsing_1 = \case
    Portuguese -> "Não foi possível interpretar o arquivo pacman.conf ."
    Russian    -> "Не удается распознать формат вашего файла pacman.conf."
    _          -> "Unable to parse your pacman.conf file."

provides_1 :: T.Text -> Doc AnsiStyle
provides_1 pro =
  bt pro <> " is required as a dependency, which is provided by multiple packages. Please select one:"

----------------------------------
-- Aura/Pkgbuild/Editing functions
----------------------------------
hotEdit_1 :: T.Text -> Language -> Doc AnsiStyle
hotEdit_1 (bt -> p) = \case
    Japanese   -> p <> "のPKGBUILDを編成しますか？"
    Polish     -> "Czy chcesz edytować PKGBUILD " <> p <> "?"
    Croatian   -> "Želite li izmjeniti PKGBUILD " <> p <> "?"
    Swedish    -> "Vill du ändra PKGBUILD-filen ifrån " <> p <> "?"
    German     -> "Möchten Sie die PKGBUILD-Datei für " <> p <> " bearbeiten?"
    Spanish    -> "¿Deseas editar el PKGBUILD de " <> p <> "?"
    Portuguese -> "Deseja editar o PKGBUILD de " <> p <> "?"
    French     -> "Voulez-vous éditer le PKGBUILD de " <> p <> " ?"
    Russian    -> "Отредактировать PKGBUILD пакета " <> p <> "?"
    Italian    -> "Volete modificare il PKGBUILD di " <> p <> "?"
    Serbian    -> "Желите ли да измените PKGBUILD за " <> p <> "?"
    Norwegian  -> "Vil du endre PKGBUILD for " <> p <> "?"
    Indonesia  -> "Apakah anda ingin menyunting PKGBUILD untuk paket " <> p <> "?"
    Chinese    -> "你希望编辑 " <> p <> " 的 PKGBUILD 文件吗？"
    _          -> "Would you like to edit the PKGBUILD of " <> p <> "?"

customizepkg_1 :: Language -> Doc AnsiStyle
customizepkg_1 = let customizepkg = bt "customizepkg" in \case
    Japanese   -> customizepkg <> "はインストールされていません。"
    Polish     -> customizepkg <> "nie zainstalowany."
    Croatian   -> customizepkg <> "nije instaliran."
    German     -> customizepkg <> "ist nicht installiert."
    Spanish    -> customizepkg <> "no está instalado."
    Norwegian  -> customizepkg <> "er ikke installert."
    Italian    -> customizepkg <> "non è installato."
    Portuguese -> customizepkg <> "não está instalado."
    French     -> customizepkg <> "n'est pas installé."
    Russian    -> customizepkg <> "не установлен."
    Indonesia  -> customizepkg <> "tidak terinstal."
    Chinese    -> customizepkg <> " 没有被安装。"
    Swedish    -> customizepkg <> "är inte installerad"
    _          -> customizepkg <> "isn't installed."

-----------------------
-- Aura/Utils functions
-----------------------
yesNoMessage :: Language -> Doc ann
yesNoMessage = \case
    Polish     -> "[T/n]"
    Croatian   -> "[D/n]"
    German     -> "[J/n]"
    Spanish    -> "[S/n]"
    Norwegian  -> "[J/n]"
    Italian    -> "[S/n]"
    Portuguese -> "[S/n]"
    French     -> "[O/n]"
    Russian    -> "[Д/н]"
    _          -> "[Y/n]"

yesPattern :: Language -> [T.Text]
yesPattern = \case
    Polish     -> ["t", "tak"]
    Croatian   -> ["d", "da"]
    German     -> ["j", "ja"]
    Spanish    -> ["s", "si"]
    Norwegian  -> ["j", "ja"]
    Italian    -> ["s", "si"]
    Portuguese -> ["s", "sim"]
    French     -> ["o", "oui"]
    Russian    -> ["д", "да"]
    _          -> ["y", "yes"]

----------------------
-- Pluralization rules
----------------------
pluralRussian :: Integral n => a -> a -> a -> n -> a
pluralRussian singular plural1 plural2 n | n % 10 == 1 && n % 100 /= 11 = singular
                                         | n % 10 `elem` [2, 3, 4] = plural1
                                         | otherwise = plural2
