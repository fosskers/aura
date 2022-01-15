{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- |
-- Module    : Aura.Languages
-- Copyright : (c) Colin Woodbury, 2012 - 2021
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- All output strings that a user can be shown.
-- All normal restrictions on line length do not apply for this file, and this file only.

module Aura.Languages where

import           Aura.Colour
import qualified Aura.Languages.Fields as Fields
import           Aura.Types
import           Data.Ratio ((%))
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Map.Partial as M
import qualified RIO.Text as T

---

-- | Thank you all!
translators :: Map Language Text
translators = M.fromList
    [ (Polish,     "Chris Warrick / Michał Kurek")
    , (Croatian,   "Denis Kasak / \"stranac\"")
    , (Swedish,    "Fredrik Haikarainen / Daniel Beecham")
    , (German,     "Lukas Niederbremer / Jonas Platte")
    , (Spanish,    "Alejandro Gómez / Sergio Conde / Max Ferrer")
    , (Portuguese, "Henry Kupty / Thiago Perrotta / Wagner Amaral")
    , (French,     "Ma Jiehong / Fabien Dubosson")
    , (Russian,    "Kyrylo Silin / Alexey Kotlyarov")
    , (Italian,    "Bob Valantin / Cristian Tentella")
    , (Serbian,    "Filip Brcic")
    , (Norwegian,  "\"chinatsun\"")
    , (Indonesia,  "\"pak tua Greg\"")
    , (Chinese,    "Kai Zhang")
    , (Japanese,   "Onoue Takuro / Colin Woodbury")
    , (Esperanto,  "Zachary Matthews")
    , (Dutch,      "Joris Blanken")
    , (Turkish,    "Cihan Alkan")
    , (Arabic,     "\"Array in a Matrix\"")
    , (Ukrainian,  "Andriy Cherniy")
    , (Romanian,   "90 / benone")
    , (Vietnamese, "\"Kritiqual\"")
    ]

-- These need updating! Or removing...
languageNames :: Language -> Map Language T.Text
languageNames = M.fromList . zip [ Japanese, Polish, Croatian, Swedish, German, Spanish, Portuguese, French, Russian, Italian, Serbian, Norwegian, Indonesia, Chinese, Esperanto, Dutch, Turkish, Arabic, Ukrainian, Romanian ] . \case
    Japanese   -> [ "日本語", "ポーランド語", "クロアチア語", "スウェーデン語", "ドイツ語", "スペイン語", "ポルトガル語", "フランス語", "ロシア語", "イタリア語", "セルビア語", "ノルウェー語", "インドネシア語", "中国語", "エスペラント", "オランダ語", "トルコ語", "アラビア語", "Ukrainian", "ルーマニア語" ]
    Arabic     -> [ "اليابانية", "البولندية", "الكرواتية", "السويدية", "الالمانية", "الاصبانية", "البرتغالية", "الفرنسية", "الروسية", "الايطالية", "السيبيرية", "النرويجية", "الاندونيسية", "الصينية", "الاسبرانتو", "الهولندية", "التركية", "Arabic", "Ukrainian", "الرومانية" ]
    Turkish    -> [ "Japonca", "Lehçe", "Hırvatça", "İsveççe", "Almanca", "İspanyolca", "Portekizce", "Fransızca", "Rusça", "İtalyanca", "Sırpça", "Norveççe", "Endonezce", "Çince", "Esperanto", "Hollandaca", "Türkçe", "Arabic", "Ukrainian", "Rumence" ]
    Polish     -> [ "Japanese", "polski", "chorwacki", "szwedzki", "niemiecki", "hiszpański", "portugalski", "francuski", "rosyjski", "włoski", "serbski", "norweski", "indonezyjski", "chiński", "esperanto", "niderlandzki", "Turkish", "Arabic", "Ukrainian", "rumuński" ]
    Croatian   -> [ "Japanese", "poljski", "hrvatski", "švedski", "njemački", "španjolski", "portugalski", "francuski", "ruski", "talijanski", "srpski", "norveški", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "rumunjski" ]
    Swedish    -> [ "Japanese", "Polska", "Kroatiska", "Svenska", "Tyska", "Spanska", "Portugisiska", "Franska", "Ryska", "Italienska", "Serbiska", "Norska", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Rumänska" ]
    German     -> [ "Japanisch", "Polnisch", "Kroatisch", "Schwedisch", "Deutsch", "Spanisch", "Portugiesisch", "Französisch", "Russisch", "Italienisch", "Serbisch", "Norwegisch", "Indonesisch", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Romanisch" ]
    Spanish    -> [ "Japanese", "Polaco", "Croata", "Sueco", "Alemán", "Español", "Portugués", "Francés", "Ruso", "Italiano", "Serbio", "Noruego", "Indonesio", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Rumano" ]
    Portuguese -> [ "Japonês", "Polonês", "Croata", "Sueco", "Alemão", "Espanhol", "Português", "Francês", "Russo", "Italiano", "Sérvio", "Norueguês", "Indonésio", "Chinês", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Romena" ]
    French     -> [ "Japanese", "Polonais", "Croate", "Suédois", "Allemand", "Espagnol", "Portugais", "Français", "Russe", "Italien", "Serbe", "Norvégien", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Roumain" ]
    Russian    -> [ "Японский", "Польский", "Хорватский", "Шведский", "Немецкий", "Испанский", "Португальский", "Французский", "Русский", "Итальянский", "Сербский", "Норвежский", "Индонезийский", "Китайский", "Эсперанто", "Датский", "Турецкий", "Арабский", "Украинский", "Румынский" ]
    Italian    -> [ "Giapponese", "Polacco", "Croato", "Svedese", "Tedesco", "Spagnolo", "Portoghese", "Francese", "Russo", "Italiano", "Serbo", "Norvegese", "Indonesiano", "Cinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Rumeno" ]
    Serbian    -> [ "Japanese", "Пољски", "Хрватски", "Шведски", "Немачки", "Шпански", "Португалски", "Француски", "Руски", "Италијански", "Српски", "", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Румунски" ]
    Norwegian  -> [ "Japanese", "Polsk", "Kroatisk", "Svensk", "Tysk", "Spansk", "Portugisisk", "Fransk", "Russisk", "Italiensk", "Serbisk", "Norsk", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Rumensk" ]
    Indonesia  -> [ "Japanese", "Polandia", "Kroasia", "Swedia", "Jerman", "Spanyol", "Portugis", "Prancis", "Rusia", "Italia", "Serbia", "Norwegia", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Rumania" ]
    Chinese    -> [ "日语", "波兰语", "克罗地亚语", "瑞典语", "德语", "西班牙语", "葡萄牙语", "法语", "俄语", "意大利语", "塞尔维亚语", "挪威语", "印度尼西亚语", "中文", "世界语", "Dutch", "Turkish", "Arabic", "Ukrainian", "罗马尼亚语" ]
    Esperanto  -> [ "La japana", "La pola", "La kroata", "La sevda", "La germana", "La hispana", "La portugala", "La franca", "La rusa", "La itala", "La serba", "La norvega", "La indonezia", "La ĉina", "Esperanto", "La nederlanda", "Turkish", "Arabic", "Ukrainian", "La rumana" ]
    Dutch      -> [ "Japans", "Pools", "Kroatisch", "Zweeds", "Duits", "Spaans", "Portugees", "Frans", "Russisch", "Italiaans", "Servisch", "Noors", "Indonesisch", "Chinees", "Esperanto", "Nederlands", "Turkish", "Arabic", "Ukrainian", "Roemeens" ]
    Ukrainian  -> [ "Японська", "Польська", "Хорватська", "Швецька", "Німецька", "Іспанська", "Португальська", "Французька", "Російська", "Італьянська", "Сербська", "Норвезька", "Індонезійська", "Китайська", "Есперанто", "Датська", "Турецька", "Арабська", "Українська", "Румунська" ]
    Romanian   -> [ "Japoneză", "Poloneză", "Croată", "Suedeză", "Germană", "Spaniolă", "Portugheză", "Franceză", "Rusă", "Italiană", "Sârbă", "Norvegiană", "Indoneziană", "Chineză", "Esperanto", "Olandeză", "Turcă", "Arabă", "Ucraineană", "Română" ]
    Vietnamese -> [ "Nhật", "Ba Lan", "Croatia", "Thụy Điển", "Đức", "Tây Ban Nha", "Bồ Đào Nha", "Pháp", "Nga", "Ý", "Serbia", "Na Uy", "Indonesia", "Trung Quốc", "Quốc tế ngữ", "Hà Lan", "Thổ Nhỹ Kỳ", "Ả rập", "Ukraina", "Romani", "Tiếng Việt" ]
    _          -> [ "Japanese", "Polish", "Croatian", "Swedish", "German", "Spanish", "Portuguese", "French", "Russian", "Italian", "Serbian", "Norwegian", "Indonesian", "Chinese", "Esperanto", "Dutch", "Turkish", "Arabic", "Ukrainian", "Romanian" ]

translatorMsgTitle :: Language -> Text
translatorMsgTitle = \case
    Japanese   -> "Auraの翻訳者："
    Polish     -> "Tłumacze Aury:"
    Arabic     -> "Aura مترجم"
    Turkish    -> "Aura Çevirmeni:"
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
    Esperanto  -> "Tradukistoj de Aura:"
    Dutch      -> "Aura Vertalers:"
    Ukrainian  -> "Перекладачі Aura:"
    Romanian   -> "Traducători Aura:"
    Vietnamese -> "Dịch giả của Aura:"
    _          -> "Aura Translators:"

translatorMsg :: Language -> [Text]
translatorMsg lang = title : names
  where title = translatorMsgTitle lang
        names = fmap snd . M.toList $
            M.mapWithKey (\l t -> formatLang (assocLang l t)) translators
        assocLang lang' translator = (translator, langNames M.! lang')
        formatLang (translator, lang') = " (" <> lang' <> ") " <> translator
        langNames = languageNames lang

-- | Make some `Text` cyan. Previous wrapped things in backticks.
bt :: Pretty a => a -> Doc AnsiStyle
bt = cyan . pretty

whitespace :: Language -> Char
whitespace Japanese = '　'  -- \12288
whitespace _        = ' '   -- \32

langFromLocale :: Text -> Maybe Language
langFromLocale = T.take 2 >>> \case
  "ja" -> Just Japanese
  "ar" -> Just Arabic
  "tr" -> Just Turkish
  "pl" -> Just Polish
  "hr" -> Just Croatian
  "sv" -> Just Swedish
  "de" -> Just German
  "es" -> Just Spanish
  "pt" -> Just Portuguese
  "fr" -> Just French
  "ru" -> Just Russian
  "it" -> Just Italian
  "sr" -> Just Serbian
  "nb" -> Just Norwegian
  "id" -> Just Indonesia
  "zh" -> Just Chinese
  "eo" -> Just Esperanto
  "nl" -> Just Dutch
  "en" -> Just English
  "uk" -> Just Ukrainian
  "ro" -> Just Romanian
  "vi" -> Just Vietnamese
  _    -> Nothing

----------------------
-- Aura/Core functions
----------------------
-- NEEDS TRANSLATION
checkDBLock_1 :: Language -> Doc AnsiStyle
checkDBLock_1 = \case
    Japanese   -> "パッケージデータベースが閉鎖されている状態です。開放したらキーを押して続行してください。"
    Polish     -> "Baza pakietów jest zablokowana. Kiedy zostanie odblokowana naciśnij enter aby kontynuować"
    Arabic     -> ".قاعدة البيانات مقفلة. اضغط ادخال عندما تفتح قاعدة البيانات للاستمرار"
    Turkish    -> "Paket veritabanı kilitlendi. Devam etmek için kilidi açıldığında enter tuşuna basın."
    Croatian   -> "Baza paketa je zaključana. Kad se otključa, pritisnite enter da biste nastavili."
    German     -> "Die Paketdatenbank ist gesperrt. Drücken Sie Enter wenn sie entsperrt ist um fortzufahren."
    Spanish    -> "La base de datos de paquetes está bloqueada. Presiona enter cuando esté desbloqueada para continuar."
    Norwegian  -> "Pakkedatabasen er låst. Trykk enter når den er åpnet for å fortsette."
    French     -> "La base de données des paquets est bloquée. Appuyez sur enter pour continuer."
    Portuguese -> "Banco de dados de pacote travado. Aperte 'enter' quando estiver destravado para poder continuar."
    Russian    -> "База данных пакетов заблокирована. Нажмите \"Ввод\", когда она разблокируется, чтобы продолжить."
    Italian    -> "Non è stato possibile accedere alla banca dati dei pacchetti. Per continuare premere invio quando sarà di nuovo disponibile."
    Chinese    -> "包数据库已锁定。请在解锁后按下回车以继续。"
    Swedish    -> "Paketdatabasen är låst. Klicka på enter när den är upplåst."
    Esperanto  -> "La datumbazo de pakaĵoj estas ŝlosita. Premu enen-klavo kiam la datumbazo estas malŝlosita por daŭrigi"
    Dutch      -> "De pakket databank is vergrendelt. Druk op enter wanneer het ontgrendelt is."
    Ukrainian  -> "База даних пакетів заблокована. Натисніть Enter, коли вона розблокується, щоб продовжити."
    Romanian   -> "Baza de date de pachete este blocată. Apăsați Enter după ce s-a deblocat pentru a continua."
    Vietnamese -> "Cơ sở dữ liệu của gói đã bị khóa. Nhấn Enter sau khi nó được mở khóa để tiếp tục."
    _          -> "The package database is locked. Press enter when it's unlocked to continue."

trueRoot_3 :: Language -> Doc AnsiStyle
trueRoot_3 = \case
    Japanese   -> "「root」としてパッケージを作成するのは「makepkg v4.2」で不可能になりました。"
    Arabic     -> ".makepkg v4.2 البناء كمشرف لم يعد ممكنا في"
    Polish     -> "Od makepkg v4.2, budowanie jako root nie jest dozwolone."
    Turkish    -> "Makepkg v4.2'den itibaren, kök olarak oluşturmak artık mümkün değildir."
    German     -> "Seit makepkg v4.2 ist es nicht mehr möglich als root zu bauen."
    Spanish    -> "Desde makepkg v4.2 no es posible compilar paquetes como root."
    Portuguese -> "A partir da versão v4.2 de makepkg, não é mais possível compilar como root."
    Russian    -> "С версии makepkg v4.2 сборка от имени root более невозможна."
    Italian    -> "A partire dalla versione 4.2 di makepkg non è più possibile compilare come root."
    Chinese    -> "自从 makepkg v4.2 以后，就不能以根用户身份构建软件了。"
    Swedish    -> "I makepkg v4.2 och uppåt är det inte tillåtet att bygga som root."
    Esperanto  -> "Depost makepkg v4.2, konstruanto ĉefuzante ne eblas."
    Dutch      -> "Vanaf makepkg v4.2 is het niet langer mogelijk om als root te bouwen."
    Ukrainian  -> "З версії makepkg v4.2 збірка від імені root неможлива."
    Romanian   -> "De la versiunea makepkg v4.2 încolo, compilarea ca root nu mai este posibilă."
    Vietnamese -> "Kể từ makepkg v4.2, build bằng quyền root không còn khả dụng."
    _          -> "As of makepkg v4.2, building as root is no longer possible."

mustBeRoot_1 :: Language -> Doc AnsiStyle
mustBeRoot_1 = let sudo = bt @Text "sudo" in \case
    Japanese   -> sudo <> "を使わないとそれができない！"
    Arabic     -> "." <> sudo <> " ﻻ يمكن اجراء هذه العملية بدون استخدام"
    Polish     -> "Musisz użyć " <> sudo <> ", żeby to zrobić."
    Turkish    -> sudo <> "kullanmadan bu işlemi gerçekleştiremezsiniz."
    Croatian   -> "Morate koristiti" <> sudo <> "za ovu radnju."
    Swedish    -> "Du måste använda " <> sudo <> " för det."
    German     -> "Sie müssen dafür " <> sudo <> " benutzen."
    Spanish    -> "Tienes que utilizar " <> sudo <> " para eso."
    Portuguese -> "Utilize " <> sudo <> " para isso."
    French     -> "Vous devez utiliser " <> sudo <> " pour ça."
    Russian    -> "Необходимо использовать " <> sudo <> " для этого."
    Italian    -> "Per eseguire questa operazione è necessario utilizzare " <> sudo <> "."
    Serbian    -> "Морате да користите " <> sudo <> " за ову радњу."
    Norwegian  -> "Du må bruke " <> sudo <> " for det."
    Indonesia  -> "Anda harus menggunakan " <> sudo <> " untuk melakukannya."
    Chinese    -> "除非是根用户，否则不能执行此操作。"
    Esperanto  -> "Vi ne povas fari ĉi tiun operacion, sen " <> sudo <> "."
    Dutch      -> "U kunt deze operatie niet uitvoeren zonder " <> sudo <> " te gebruiken."
    Ukrainian  -> "Для цієї дії, потрібно використати " <> sudo <> "."
    Romanian   -> "Nu se poate folosi această operație asta fără " <> sudo <> "."
    Vietnamese -> "Bạn không thể thực hiện hành động này nếu không dùng " <> sudo <> "."
    _          -> "You cannot perform this operation without using sudo."

-----------------------
-- Aura/Build functions
-----------------------
buildPackages_1 :: PkgName -> Language -> Doc AnsiStyle
buildPackages_1 (bt . pnName -> p) = \case
    Japanese   -> p <> "を作成中・・・"
    Arabic     -> "..." <> p <> " بناء"
    Turkish    -> "İnşa ediliyor " <> p <> "..."
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
    Esperanto  -> "Muntanta " <> p <> "..."
    Dutch      -> "Pakket " <> p <> " aan het bouwen..."
    Ukrainian  -> "Збираємо " <> p <> "..."
    Romanian   -> "Se compilează " <> p <> "..."
    Vietnamese -> "Đang build " <> p <> "..."
    _          -> "Building " <> p <> "..."

buildPackages_2 :: Language -> Doc AnsiStyle
buildPackages_2 = \case
    Arabic     -> ".لن يتم بناء اي رزمة .'--allsource' كشف"
    Polish     -> "'--allsource' wykryte. Nie zostaną zbudowane żadne pakiety."
    Turkish    -> "'--allsource' bulundu. Yüklenebilir gerçek paketler oluşturulmayacaktır."
    Spanish    -> "'--allsource' detectado. No se construirán paquetes instalables reales."
    Romanian   -> "'--allsource' detectat. Nu se va compila oricare pachet instalabil."
    Vietnamese -> "'--allsource' được sử dụng. Không có gói nào sẽ được build."
    _          -> "'--allsource' detected. No actual installable packages will be built."

buildPackages_3 :: FilePath -> Language -> Doc AnsiStyle
buildPackages_3 fp = \case
    Arabic     -> pretty fp <> ":تم بناؤها ونسخها إلى .src.tar.gz كل ملفات"
    Polish     -> "Wszystkie pliki .src.tar.gz zostały zbudowane i przekopiowane do: " <> pretty fp
    Turkish    -> "Tüm .src.tar.gz dosyaları oluşturuldu ve şuraya kopyalandı: " <> pretty fp
    Spanish    -> "Todos los archivos .src.tar.gz fueron construidos y copiados a: " <> pretty fp
    Ukrainian  -> "Всі архіви .src.tar.gz були зібрані та скопійовані до: " <> pretty fp
    Romanian   -> "Toate fișierele .src.tar.gz au fost construite și copiate către: " <> pretty fp
    Vietnamese -> "Tất cả các tệp .src.tar.gz đã được build và sao chép tới: " <> pretty fp
    _          -> "All .src.tar.gz files were built and copied to: " <> pretty fp

buildPackages_4 :: Language -> Doc AnsiStyle
buildPackages_4 = \case
    Romanian -> bt @Text "--hotedit" <+> "detectat, dar acestea au date în cache și vor fi omise din editare:"
    Vietnamese -> bt @Text "--hotedit" <+> "được sử dụng, những gói sau có trong cache và sẽ được bỏ qua để chỉnh sửa:"
    _        -> bt @Text "--hotedit" <+> "detected, but the following have cache entries and will be skipped for editing:"

buildPackages_5 :: Language -> Doc AnsiStyle
buildPackages_5 = \case
    Romanian -> "Se poate folosi" <+> bt @Text "--force" <+> "pentru a trece peste acest comportament."
    Vietnamese -> "Bạn có thể dùng" <+> bt @Text "--force" <+> "để ghi đè hành động này."
    _        -> "You can use" <+> bt @Text "--force" <+> "to override this behaviour."

buildFail_5 :: Language -> Doc AnsiStyle
buildFail_5 = \case
    Japanese   -> "パッケージ作成に失敗しました。"
    Arabic     -> ".لقد فشل البناء"
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
    Esperanto  -> "Muntado paneis"
    Dutch      -> "Bouwen is mislukt."
    Ukrainian  -> "Збірка не вдалась."
    Romanian   -> "Compilare nereușită."
    Vietnamese -> "Build thất bại."
    _          -> "Building failed."

-- NEEDS TRANSLATION
buildFail_6 :: Language -> Doc AnsiStyle
buildFail_6 = \case
    Japanese   -> "それでも続行しますか？"
    Arabic     -> "هل ترغب في الاستمرار على أي حال؟"
    Polish     -> "Czy mimo to chcesz kontynuować?"
    Croatian   -> "Želite li svejedno nastaviti?"
    German     -> "Möchten Sie trotzdem fortfahren?"
    Spanish    -> "¿Deseas continuar de todas formas?"
    Norwegian  -> "Vil du fortsette likevel?"
    Italian    -> "Procedere comunque?"
    Portuguese -> "Gostaria de continuar mesmo assim?"
    French     -> "Voulez-vous tout de même continuer ?"
    Russian    -> "Продолжить, несмотря ни на что?"
    Indonesia  -> "Apakah anda tetap ingin melanjutkan?"
    Chinese    -> "你仍然希望继续吗？"
    Swedish    -> "Vill du fortsätta ändå?"
    Esperanto  -> "Ĉu vi volas daŭrigi?"
    Dutch      -> "Wilt U toch doorgaan?"
    Ukrainian  -> "Ви все одно бажаєте продовжити?"
    Romanian   -> "Doriți oricum să continuați?"
    Vietnamese -> "Bạn có muốn tiếp tục không?"
    _          -> "Would you like to continue anyway?"

-- NEEDS TRANSLATION
buildFail_7 :: PkgName -> Language -> Doc AnsiStyle
buildFail_7 (bt . pnName -> p) = \case
    Japanese   -> p <> "のビルドスクリプトを収得できませんでした。"
    Arabic     -> "." <> p <> " فشل في الحصول على نصوص البناء لأجل"
    Polish     -> "Nie udało się pozyskać skryptów budowania dla " <> p <> "."
    German     -> "Herunterladen der Build-Skripte für " <> p <> " fehlgeschlagen."
    Spanish    -> "No se han podido obtener los scripts de compilación de " <> p <> "."
    Portuguese -> "Falha ao obter scripts de compilação para " <> p <> "."
    Indonesia  -> "Gagal mendapatkan skrip untuk " <> p <> "."
    Russian    -> "Не удалось получить сценарии сборки для " <> p <> "."
    Italian    -> "Non è stato possibile ottenere gli script di compilazione per " <> p <> "."
    Chinese    -> "无法获得 " <> p <> " 的构建脚本。"
    Swedish    -> "Kunde inte hämta byggskript för " <> p <> "."
    Esperanto  -> "Paneis akiri muntaj skriptoj de " <> p <> "."
    Dutch      -> "Verkrijgen van bouw scripten mislukt voor " <> p <> "."
    Ukrainian  -> "Не вдалось отримати сценарії збірки для " <> p <> "."
    Romanian   -> "Nu s-au putut obține scripturi de compilare pentru " <> p <> "."
    Vietnamese -> "Không thể lấy tập lệnh build cho " <> p <> "."
    _          -> "Failed to obtain build scripts for " <> p <> "."

buildFail_8 :: Language -> Doc AnsiStyle
buildFail_8 = \case
    Japanese   -> "makepkgは失敗しました。"
    Arabic     -> ".makepkgهناك فشل في اﻟ"
    Polish     -> "Wystąpił problem z makepkg."
    Spanish    -> "Ocurrió un error al ejecutar makepkg"
    Portuguese -> "Ocorreu um erro ao executar makepkg"
    Russian    -> "Произошла ошибка makepkg."
    Italian    -> "C'è stato un errore nell'esecuzione di makepkg."
    Esperanto  -> "Paneo de makepkg okazis."
    Dutch      -> "Er is een fout opgetreden in makepkg."
    Ukrainian  -> "Сталась помилка makepkg."
    Romanian   -> "A fost o problemă cu makepkg."
    Vietnamese -> "Có lỗi khi makepkg."
    _          -> "There was a makepkg failure."

buildFail_9 :: Language -> Doc AnsiStyle
buildFail_9 = \case
  Polish    -> "Nie udało się zlokalizować żadnych zbudowanych pakietów (*.pkg.tar.xz)."
  Arabic    -> ".(*.pkg.tar.xz) فشل في اكتشاف اي ملف من ملفات البناء"
  Spanish   -> "Error al detectar todos los archivo de paquete (*.pkg.tar.xz)."
  Italian   -> "Non è stato possibile trovare nessun archivio risultante dalla compilazione del pacchetto (*.pkg.tar.xz)."
  Esperanto -> "Paneis detekti ĉiujn dosierojn de pakaĵoj (*.pkg.tar.xz)."
  Dutch     -> "Detecteren van built package files mislukt (*.pkg.tar.xz)."
  Ukrainian -> "Не вдалось знайти жодного файлу пакунку (*.pkg.tar.xz)."
  Romanian  -> "Nu s-a detectat nici un pachet construit (*.pkg.tar.xz)."
  Vietnamese -> "Không thể phát hiện các tệp đã được build (*.pkg.tar.xz)."
  _         -> "Failed to detect any built package files (*.pkg.tar.xz)."

buildFail_10 :: Language -> Doc AnsiStyle
buildFail_10 = \case
  Polish    -> "Nie udało się zbudować żadnego pakietu."
  Arabic    -> ".فشل بناء كل الرزم"
  Spanish   -> "Los paquetes no se pudieron construir."
  Italian   -> "Non è stato possibile compilare i pacchetti."
  Esperanto -> "Ĉiuj pakaĵoj paneis munti."
  Dutch     -> "Het bouwen van alle pakketten is mislukt."
  Ukrainian -> "Жоден пакунок не вдалося зібрати."
  Romanian  -> "Nu s-a putut compila nici un pachet."
  Vietnamese -> "Tất cả các gói build thất bại."
  _         -> "Every package failed to build."

buildFail_11 :: Language -> Doc AnsiStyle
buildFail_11 = \case
  Japanese  -> "作成は失敗しました。エラーを見ますか？"
  Arabic    -> "فشل البناء. هل ترغب في رؤية الخطأ؟"
  Polish    -> "Budowa zakończona niepowodzeniem. Czy chcesz zobaczyć błąd?"
  Spanish   -> "Construcción fallida. ¿Te gustaría ver el error?"
  Italian   -> "La compilazione è fallita. Visionare l'errore?"
  Esperanto -> "Muntado paneis. Ĉu vi volas vidi la eraron?"
  Dutch     -> "Bouwen mislukt. Wilt U de fouten zien?"
  Ukrainian -> "Збірка не вдалась. Бажаєте побачити помилку?"
  Romanian  -> "Compilare nereușită. Doriți să vedeți eroarea?"
  Vietnamese -> "Build thất bại. Bạn có muốn xem lịch sử lỗi?"
  _         -> "Building failed. Would you like to see the error?"

buildFail_12 :: Language -> Doc AnsiStyle
buildFail_12 = \case
    Polish     -> "Błąd podczas pobierania najnowszych aktualizacji poprzez 'git pull'."
    Arabic     -> ".على اخر تحديث 'git pull' فشل حصول"
    Spanish    -> "Error al 'git pull' las últimas actualizaciones."
    Ukrainian  -> "Не вдалося використати 'git pull' для отримання останніх оновлень."
    Romanian   -> "Nu a reușit 'git pull' să descarce cele mai recente actualizări."
    Vietnamese -> "Thất bại trong việc 'git pull' để cập nhật."
    _          -> "Failed to 'git pull' the latest updates."

------------------------------
-- Aura/Dependencies functions
------------------------------
-- NEEDS UPDATE TO MATCH NEW ENGLISH
getRealPkgConflicts_1 :: PkgName -> PkgName -> Text -> Text -> Language -> Doc AnsiStyle
getRealPkgConflicts_1 (bt . pnName -> prnt) (bt . pnName -> p) (bt -> r) (bt -> d) = \case
    Japanese   -> "パッケージ" <> p <> "はバージョン" <> d <> "を要するが" <> "一番最新のバージョンは" <> r <> "。"
    Arabic     -> prnt <> " الرزمة " <> d <> " تعتمد على النسخة " <> p <> " من " <> r <> " لكن أحدث نسخه هي"
    Polish     -> "Zależność " <> p <> " powinna być w wersji " <> d <> ", ale najnowsza wersja to " <> r <> "."
    Croatian   -> "Zavisnost " <> p <> " zahtjeva verziju " <> d <> ", a najnovija dostupna verzija je " <> r <> "."
    Swedish    -> "Beroendepaketet " <> p <> " kräver version " <> d <> " men den senaste versionen är " <> r <> "."
    German     -> "Die Abhängigkeit " <> p <> " verlangt Version " <> d <> ", aber die neuste Version ist " <> r <> "."
    Spanish    -> "La dependencia " <> p <> " requiere la versión " <> d <> " pero la versión más reciente es " <> r <> "."
    Portuguese -> "A dependência " <> p <> " exige a versão " <> d <> " mas a versão mais recente é " <> r <> "."
    French     -> p <> " est une dépendance nécessitant la version " <> d <> ", mais la plus récente est la version " <> r <> "."
    Russian    -> "Зависимость " <> p <> " требует версию " <> d <> ", однако самой последней версией является " <> r <> "."
    Italian    -> p <> "è una dipendenza che necessita della versione " <> d <> ", ma la più recente è la " <> r <> "."
    Serbian    -> "Зависност " <> p <> " захтева верзију " <> d <> ", али најновија верзија је " <> r <> "."
    Norwegian  -> "Avhengigheten " <> p <> " krever versjon " <> d <>", men den nyeste versjonen er " <> r <> "."
    Indonesia  -> "Dependensi " <> p <> " meminta versi " <> d <> " namun versi paling baru adalah " <> r <> "."
    Chinese    -> "依赖 " <> p <> " 需要版本 " <> d <> "，但是最新的版本是 " <> r <> "。"
    Esperanto  -> "La pakaĵo, " <> prnt <> ", dependas de versio " <> d <> " de " <> p <> ", sed la plej nova versio estas " <> r <> "."
    Dutch      -> "Het pakket " <> prnt <> ", hangt af van versie " <> d <> " van " <> p <> ", maar de meest recente versie is " <> r <> "."
    Ukrainian  -> "Залежність " <> p <> " потребує версію " <> d <> ", проте останньою версією є " <> r <> "."
    Romanian   -> "Pachetul " <> prnt <> " depinde de versiunea " <> d <> " al pachetului " <> p <> ", dar cea mai recentă versiune este " <> r <> "."
    Vietnamese -> "Gói " <> prnt <> " phụ thuộc vào bản " <> d <> " của " <> p <> ", nhưng bản mới nhất là " <> r <> "."
    _          -> "The package " <> prnt <> " depends on version " <> d <> " of " <> p <> ", but the most recent version is " <> r <> "."

getRealPkgConflicts_2 :: PkgName -> Language -> Doc AnsiStyle
getRealPkgConflicts_2 (bt . pnName -> p) = \case
  Japanese   -> p <> "は無視されるパッケージ！`pacman.conf`を参考に。"
  Arabic     -> ".الخاص بك `pacman.conf`انظر الى اﻟ !" <> p <> " ملف مجهول"
  Polish     -> p <> " jest ignorowany! Sprawdź plik `pacman.conf`."
  Croatian   -> p <> " je ignoriran paket! Provjerite svoj `pacman.conf`."
  Swedish    -> p <> " är ett ignorerat paket! Kolla din `pacman.conf`-fil."
  German     -> p <> " ist ein ignoriertes Paket! Siehe /etc/pacman.conf."
  Spanish    -> "¡" <> p <> " es un paquete ignorado! Revisa tu fichero `pacman.conf`."
  Portuguese -> p <> " é um pacote ignorado conforme configuração em `pacman.conf`!"
  French     -> "Le paquet " <> p <> " est ignoré. Vous devriez jeter un œil à votre `pacman.conf`."
  Russian    -> "Пакет " <> p <> " игнорируется! Проверьте ваш файл `pacman.conf`."
  Italian    -> p <> " è marcato come pacchetto ignorato all'interno del file `pacman.conf`."
  Serbian    -> "Пакет " <> p <> " је игнорисан! Видите ваш фајл „pacman.conf“."
  Norwegian  -> p <> " er en ignorert pakke! Sjekk din `pacman.conf`-fil."
  Indonesia  -> p <> " merupakan paket yang diabaikan! Lihat `pacman.conf` anda."
  Chinese    -> p <> " 是一个被忽略的包！请查看你的 `pacman.conf` 文件。"
  Esperanto  -> p <> " estas malatenta pakaĵo! Vidu vian `pacman.conf` dosieron."
  Dutch      -> p <> " is een genegeerd pakket! Bekijk uw `pacman.conf` file."
  Ukrainian  -> "Пакунок " <> p <> " буде проігноровано! Перевірте ваш файл `pacman.conf`."
  Romanian   -> "Pachetul " <> p <> " este ignorat! Verificați fișierul `pacman.conf`."
  Vietnamese -> "Gói " <> p <> "đã bị bỏ qua! Hãy xem trong `pacman.conf` của bạn."
  _          -> p <> " is an ignored package! See your `pacman.conf` file."

missingPkg_2 :: [DepError] -> Language -> Doc AnsiStyle
missingPkg_2 ps l = vsep $ map (depError l) ps

depError :: Language -> DepError -> Doc AnsiStyle
depError _ (VerConflict s) = s
depError _ (Ignored s)     = s
depError l (NonExistant (PkgName s) (PkgName par)) = case l of
  Polish     -> "Zależność " <> bt s <> "nie została znaleziona."
  Arabic     -> ".غير موجود " <> bt par <> " من " <> bt s <> " الاعتماد"
  Spanish    -> "La dependencia " <> bt s <> " no pudo ser encontrada."
  Portuguese -> "A dependência " <> bt s <> " não foi encontrada."
  Russian    -> "Зависимость " <> bt s <> " не найдена."
  Italian    -> "Non è stato possibile trovare la dipendenza " <> bt s <> "."
  Esperanto  -> "La dependeco " <> bt s <> " de " <> bt par <> " ne povis troviĝi."
  Dutch      -> "Het afhankelijkheid " <> bt s <> "kan niet worden gevonden."
  Ukrainian  -> "Залежність " <> bt s <> " не було знайдено."
  Vietnamese -> "Không thể tìm thấy các gói phụ thuộc của " <> bt s <> "."
  _          -> "The dependency " <> bt s <> " of " <> bt par <> " couldn't be found."
depError l (BrokenProvides (PkgName pkg) (Provides (PkgName pro)) (PkgName n)) = case l of
  Arabic     -> "." <> bt pro <> " اللتي تقدم ," <> bt n <> " تحتاج" <> bt pkg <> " الرزمة"
  Spanish    -> "El paquete " <> bt pkg <> " necesita " <> bt n <> " que proporciona " <> bt pro <> "."
  Russian    -> "Пакету " <> bt pkg <> " требуется " <> bt n <> ", предоставляющий " <> bt pro <> "."
  Esperanto  -> "La pakaĵo, " <> bt pkg <> " bezonas " <> bt n <> ", kiu donas " <> bt pro <> "."
  Italian    -> "Il pacchetto " <> bt pkg <> " ha bisogno di " <> bt n <> ", che rende disponibile " <> bt pro <> "."
  Dutch      -> "Het pakket" <> bt pkg <> " heeft " <> bt n <> " nodig, die " <> bt pro <> " biedt."
  Ukrainian  -> "Пакунку " <> bt pkg <> " потрібен " <> bt n <> ", який забезпечує " <> bt pro <> "."
  Romanian   -> "Pachetul " <> bt pkg <> " are nevoie de " <> bt n <> ", care provizionează " <> bt pro <> "."
  Vietnamese -> "Gói " <> bt pkg <> " cần " <> bt n <> ", để cung cấp " <> bt pro <> "."
  _          -> "The package " <> bt pkg <> " needs " <> bt n <> ", which provides " <> bt pro <> "."

missingPkg_3 :: Language -> Doc AnsiStyle
missingPkg_3 = \case
  Polish     -> "Wystąpił problem podczas reorganizowania grafu zależności. Jeśli widzisz tą wiadomość, coś poszło bardzo nie tak."
  Arabic     -> ".حدث خطا في اعادة تنظيم الرسم البياني التبعي. اذا رايت هذه الرسالة، فهناك مشكلة كبيرة"
  Spanish    -> "Se produjo un error al reorganizar el gráfico de dependencia. Si ves esto, algo está muy mal."
  Esperanto  -> "Eraro okazis kiam reorganizi la grafeo de dependeco. Io estas erarega."
  Italian    -> "C'è stato un errore nella riorganizzazione della gerarchia delle dipendenze. Se vedi questo messaggio, qualcosa è andato davvero storto."
  Dutch      -> "Er is een fout opgetreden bij het reorganizeren van de afhankelijkheidsgrafiek. Als U dit ziet, is er iets heel erg mis."
  Romanian   -> "A fost o problemă reorganizând graful de dependențe. Dacă vedeți asta, e foarte rău."
  Vietnamese -> "Có lỗi trong quá trình xây dựng biểu đồ gói phụ thuộc. Nếu bạn thấy điều này, có gì đó không đúng."
  _          -> "There was an error reorganizing the dependency graph. If you see this, something is very wrong."

missingPkg_4 :: [NonEmpty PkgName] -> Language -> Doc AnsiStyle
missingPkg_4 pns = \case
  Polish     -> vsep $ "Następujące cykle zależności zostały wykryte:" : pns'
  Arabic     -> vsep $ pns' <> [":تم اكتشاف دورات التبعية التالية"]
  Spanish    -> vsep $ "Se detectaron los siguientes ciclos de dependencia:" : pns'
  Italian    -> vsep $ "Sono stati individuati i seguenti cicli di dipendenza:" : pns'
  Dutch      -> vsep $ "The volgende afhankelijkheidscycli zijn gedetecteerd:" : pns'
  Ukrainian  -> vsep $ "Було помічено цикл залежностей:" : pns'
  Romanian   -> vsep $ "Aceste cicluri de dependență a fost detectate:" : pns'
  Vietnamese -> vsep $ "Phát hiện chu kỳ gói phụ thuộc: " : pns'
  _ -> vsep $ "The following dependency cycles were detected:" : pns'
  where
    pns' :: [Doc ann]
    pns' = map (hsep . map pretty . L.intersperse "=>" . map pnName . toList) pns

missingPkg_5 :: PkgName -> Language -> Doc AnsiStyle
missingPkg_5 (PkgName p) = \case
  Polish    -> bt p <> " nie istnieje."
  Arabic    -> ".ليس موجود " <> bt p
  Spanish   -> bt p <> " no existe."
  Italian   -> bt p <> " non esiste."
  Dutch     -> bt p <> " bestaat niet."
  Ukrainian -> "Пакунок " <> bt p <> " не існує."
  Romanian  -> "Pachetul " <> bt p <> " nu există."
  Vietnamese -> bt p <> " không tồn tại."
  _         -> bt p <> " does not exist."

-----------------
-- aura functions
-----------------
displayOutputLanguages_1 :: Language -> Doc AnsiStyle
displayOutputLanguages_1 = \case
    Japanese   -> "aura は下記の言語に対応しています："
    Arabic     -> ":اللغات التالية موجودة"
    Polish     -> "Następujące języki są dostępne:"
    Croatian   -> "Dostupni su sljedeći jezici:"
    Swedish    -> "Följande språk är tillängliga:"
    German     -> "Die folgenden Sprachen sind verfügbar:"
    Spanish    -> "Los siguientes idiomas están disponibles:"
    Portuguese -> "Os seguintes idiomas estão disponíveis:"
    French     -> "Les langues suivantes sont disponibles :"
    Russian    -> "Доступны следующие языки:"
    Italian    -> "Sono disponibili le seguenti lingue:"
    Serbian    -> "Доступни су следећи језици:"
    Norwegian  -> "Følgende språk er tilgjengelig:"
    Indonesia  -> "Berikut ini adalah bahasa yang tersedia:"
    Chinese    -> "以下语言是可用的："
    Esperanto  -> "La sekvaj lingvo estas disponebla:"
    Dutch      -> "De volgende talen zijn beschikbaar:"
    Ukrainian  -> "Доступні наступні мови:"
    Romanian   -> "Aceste pacheturi sunt disponibile:"
    Vietnamese -> "Ngôn ngữ khả dụng:"
    _          -> "The following languages are available:"

----------------------------
-- Aura/Commands/A functions
----------------------------
-- NEEDS TRANSLATION
auraCheck_1 :: Language -> Doc AnsiStyle
auraCheck_1 = \case
    Japanese   -> "Aura が更新されています。Auraだけ先に更新しますか？"
    Arabic     -> "موجود. هل تريد ان تحدث اولا؟ Aura تحديث الى"
    Polish     -> "Dostępna jest nowa wersja Aura. Czy chcesz ją najpierw aktualizować?"
    Croatian   -> "Dostupna je nova verzija Aura. Želite li prvo ažurirati?"
    German     -> "Ein Update für aura ist verfügbar. Dies zuerst aktualisieren?"
    Spanish    -> "Hay una actualización de aura disponible. ¿Deseas actualizar aura primero?"
    Norwegian  -> "En Aura-oppdatering er tilgjengelig. Oppdater den først?"
    Portuguese -> "Uma atualização para Aura está disponível. Deseja atualizar antes?"
    French     -> "Une mise à jour d'Aura est disponible. Voulez-vous la mettre à jour en premier ?"
    Russian    -> "Доступно обновление Aura. Обновить сперва её?"
    Italian    -> "È disponibile un nuovo aggiornamento per Aura. Eseguirlo subito?"
    Indonesia  -> "Pemutakhiran aura tersedia. Mutakhirkan aura dulu?"
    Chinese    -> "Aura 可以升级。先升级 aura？"
    Swedish    -> "Det finns en uppdatering tillgänglig till Aura. Vill du uppdatera Aura först?"
    Esperanto  -> "Ĝisdatigo de Aura estas disponebla. Ĉu ĝisdatigas ĝin?"
    Dutch      -> "Aura update beschikbaar. Eerst updaten?"
    Ukrainian  -> "Доступно оновлення для Aura. Бажаєте оновити її першою?"
    Romanian   -> "O versiune nouă de Aura este disponibilă. Să se actualizeze înainte de toate?"
    Vietnamese -> "Đã có cập nhật cho Aura. Cập nhật?"
    _          -> "Aura update available. Update it first?"

install_2 :: Language -> Doc AnsiStyle
install_2 = \case
    Japanese   -> "適切なパッケージを入力してください。"
    Arabic     -> ".لم يتم تحديد حزم صالحة"
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
    Esperanto  -> "Ne validajn pakaĵojn specifis"
    Dutch      -> "Geen geldige pakketen gespecificeerd."
    Ukrainian  -> "Валідні пакунки не вказані."
    Romanian   -> "Nu s-a specificat nici un pachet valabil."
    Vietnamese -> "Tên của gói được yêu cầu không đúng."
    _          -> "No valid packages specified."

install_3 :: Language -> Doc AnsiStyle
install_3 = \case
    Japanese   -> "続行しますか？"
    Arabic     -> "هل تريد ان تكمل؟"
    Turkish    -> "Devam edilsin mi?"
    Polish     -> "Kontynuować?"
    Croatian   -> "Nastaviti?"
    Swedish    -> "Fortsätta?"
    German     -> "Fortsetzen?"
    Spanish    -> "¿Continuar?"
    Portuguese -> "Continuar?"
    French     -> "Continuer ?"
    Russian    -> "Продолжить?"
    Italian    -> "Continuare?"
    Serbian    -> "Наставити?"
    Norwegian  -> "Fortsett?"
    Indonesia  -> "Lanjut?"
    Chinese    -> "继续？"
    Esperanto  -> "Ĉu daŭrigi?"
    Dutch      -> "Doorgaan?"
    Ukrainian  -> "Продовжити?"
    Romanian   -> "Continuați?"
    Vietnamese -> "Tiếp tục?"
    _          -> "Continue?"

install_4 :: Language -> Doc AnsiStyle
install_4 = \case
    Japanese   -> "続行は意図的に阻止されました。"
    Arabic     -> ".تم الغاء التثبيت يدويا"
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
    Esperanto  -> "Instalon ĉesigi permane"
    Dutch      -> "Installatie handmatig afgebroken."
    Ukrainian  -> "Встановлення скасовано користувачем."
    Romanian   -> "Instalarea anulată manual."
    Vietnamese -> "Quá trình cài đặt được hủy."
    _          -> "Installation manually aborted."

install_5 :: Language -> Doc AnsiStyle
install_5 = \case
    Japanese   -> "従属パッケージを確認中・・・"
    Arabic     -> ".تحديد التبعيات"
    Polish     -> "Ustalanie zależności..."
    Croatian   -> "Određivanje zavisnosti..."
    Swedish    -> "Avgör beroenden..."
    German     -> "Bestimme Abhängigkeiten..."
    Spanish    -> "Determinando dependencias..."
    Portuguese -> "Determinando as dependências..."
    French     -> "Détermination des dépendances en cours…"
    Russian    -> "Определение зависимостей..."
    Italian    -> "Determinazione delle dipendenze..."
    Serbian    -> "Утврђивање зависности..."
    Norwegian  -> "Bestemmer avhengigheter..."
    Indonesia  -> "Menentukan dependensi..."
    Chinese    -> "确定依赖中..."
    Esperanto  -> "Difinas dependecojn..."
    Dutch      -> "Afhankelijkheden aan het bepalen..."
    Ukrainian  -> "Визначення залежностей..."
    Romanian   -> "Se determin dependențele..."
    Vietnamese -> "Xác định các gói phụ thuộc..."
    _          -> "Determining dependencies..."

-- 2014 December  7 @ 14:45 - NEEDS TRANSLATIONS
confirmIgnored_1 :: PkgName -> Language -> Doc AnsiStyle
confirmIgnored_1 (bt . pnName -> p) = \case
    Japanese   -> p <> "は無視されるはずのパッケージです。それでも続行しますか？"
    Arabic     -> "تم تحديده كمجهول. هل تريد ان تحمل على أي حال؟ " <> p
    Polish     -> p <> " jest oznaczony jako ignorowany. Zainstalować mimo tego?"
    Spanish    -> p <> " está marcado como ignorado. ¿Deseas instalarlo de todas formas?"
    Portuguese -> p <> " está marcado como Ignorado. Deseja instalar mesmo assim?"
    Russian    -> p <> " отмечен как игнорируемый. Всё равно установить?"
    Italian    -> p <> " è marcato come pacchetto ignorato. Installarlo comunque?"
    Chinese    -> p <> " 已被标记为忽略。仍然安装？"
    Swedish    -> p <> " är markerad som ignorerad. Vill du installera ändå?"
    Esperanto  -> p <> " estas markita kiel malatenta. Ĉu instali?"
    Dutch      -> p <> " is gemarkeerd als genegeerd. Toch installeren?"
    Romanian   -> p <> " e marcat ca ignorat. Să se instaleze oricum?"
    Vietnamese -> p <> " được đánh dấu là Bỏ qua. Vẫn cài đặt nó?"
    _          -> p <> " is marked as Ignored. Install anyway?"

-- NEEDS UPDATE TO REFLECT CHANGED ENGLISH
reportNonPackages_1 :: Language -> Doc AnsiStyle
reportNonPackages_1 = \case
    Japanese   -> "下記はAURパッケージではありません："
    Arabic     -> ".AURالرزمة التالية ليست من اﻟ"
    Polish     -> "To nie są pakiety AUR:"
    Croatian   -> "Ovo nisu AUR paketi:"
    Swedish    -> "Följande är inte paket:"
    German     -> "Folgende sind keine AUR-Pakete:"
    Spanish    -> "Los siguientes paquetes no son de AUR:"
    Portuguese -> "Os seguintes não são pacotes AUR:"
    French     -> "Les éléments suivants ne sont pas des paquets AUR :"
    Russian    -> "Ниже указано то, что не является пакетами AUR:"
    Italian    -> "I seguenti pacchetti non fanno parte dell'AUR:"
    Serbian    -> "Ово нису пакети:"
    Norwegian  -> "Det følgende er ikke AUR-pakker:"
    Indonesia  -> "Paket berikut ini bukan merupakan paket AUR:"
    Chinese    -> "以下软件不是 AUR 包："
    Esperanto  -> "La sekvaj ne estas pakaĵoj de la AUR:"
    Dutch      -> "De volgende pakketten zijn geen AUR pakketten:"
    Ukrainian  -> "Нижче вказано те, що не є пакунком AUR:"
    Romanian   -> "Aceste pachete nu se află pe AUR:"
    Vietnamese -> "Các gói sau không thuộc AUR:"
    _          -> "The following are not AUR packages:"

-- NEEDS TRANSLATION
reportUnneededPackages_1 :: Language -> Doc AnsiStyle
reportUnneededPackages_1 = \case
    Japanese   -> "下記のパッケージは既にインストールされています："
    Arabic     -> ".الرزمة التالية مثبتة"
    Polish     -> "Następujące pakiety zostały już zainstalowane:"
    Portuguese -> "Os seguintes pacotes já estão instalados:"
    Russian    -> "Следующие пакеты уже установлены:"
    Italian    -> "I seguenti pacchetti sono già stati installati:"
    German     -> "Die folgenden Pakete sind bereits installiert:"
    Spanish    -> "Los siguientes paquetes ya están instalados:"
    Chinese    -> "以下包已被安装："
    Swedish    -> "Följande paket är redan installerade:"
    Esperanto  -> "La sekvaj pakaĵoj jam instaliĝas:"
    Dutch      -> "The volgende pakketten zijn al geinstalleerd:"
    Ukrainian  -> "Наступні пакунки вже встановлені:"
    Romanian   -> "Aceste pachete sunt deja instalate:"
    Vietnamese -> "Các gói sau đã sẵn sàng cài đặt:"
    _          -> "The following packages are already installed:"

reportPkgsToInstall_1 :: Language -> Doc AnsiStyle
reportPkgsToInstall_1 = \case
    Japanese   -> "Pacmanの従属パッケージ："
    Arabic     -> ":تبعيات المستودع"
    Polish     -> "Zależności z repozytoriów:"
    Croatian   -> "Zavisnosti iz repozitorija:"
    Swedish    -> "Beroenden ifrån lager:"
    German     -> "Abhängigkeiten in den Paketquellen:"
    Spanish    -> "Dependencias del repositorio:"
    Portuguese -> "Dependências no repositório:"
    French     -> "Dépendances du dépôt :"
    Russian    -> "Зависимости из репозитория:"
    Italian    -> "Dipendenze del repository:"
    Serbian    -> "Зависности из ризница:"
    Norwegian  -> "Avhengigheter fra depotet:"
    Indonesia  -> "Dependensi dari repositori:"
    Chinese    -> "仓库依赖："
    Esperanto  -> "Dependecoj de deponejo:"
    Dutch      -> "Repository afhankelijkheden:"
    Ukrainian  -> "Залежності репозиторія:"
    Romanian   -> "Dependențe din repertorii:"
    Vietnamese -> "Các repo phụ thuộc:"
    _          -> "Repository dependencies:"

-- NEEDS AN UPDATE
reportPkgsToInstall_2 :: Language -> Doc AnsiStyle
reportPkgsToInstall_2 = \case
    Japanese   -> "AURのパッケージ:"
    Arabic     -> ":AURرزمةاﻟ"
    Polish     -> "Pakiety AUR:"
    Turkish    -> "AUR Paketleri:"
    Croatian   -> "AUR Paketi:"
    German     -> "AUR Pakete:"
    Spanish    -> "AUR Paquetes:"
    Norwegian  -> "AUR Pakker:"
    Italian    -> "AUR Pacchetti:"
    Portuguese -> "AUR Pacotes:"
    French     -> "AUR Paquets :"
    Russian    -> "AUR Пакеты:"
    Indonesia  -> "AUR Paket:"
    Chinese    -> "AUR 包："
    Swedish    -> "AUR Paket:"
    Esperanto  -> "Pakaĵoj de AUR:"
    Dutch      -> "AUR Pakketten:"
    Ukrainian  -> "Пакунки AUR:"
    Romanian   -> "Pachete din AUR:"
    Vietnamese -> "Gói AUR:"
    _          -> "AUR Packages:"

reportPkgsToInstall_3 :: Language -> Doc AnsiStyle
reportPkgsToInstall_3 = \case
    Japanese   -> "AURの従属パッケージ："
    Arabic     -> ":AURتبعيات اﻟ"
    Polish     -> "Zależności z AUR:"
    Croatian   -> "Zavisnosti iz AUR-a:"
    Swedish    -> "Beroenden ifrån AUR:"
    German     -> "Abhängigkeiten im AUR:"
    Spanish    -> "Dependencias en AUR:"
    Portuguese -> "Dependências no AUR:"
    French     -> "Dépendances AUR\xa0:"
    Russian    -> "Зависимости из AUR:"
    Italian    -> "Dipendenze nell'AUR:"
    Serbian    -> "Зависности из AUR-а:"
    Norwegian  -> "Avhengigheter fra AUR:"
    Esperanto  -> "Dependencoj de AUR:"
    Dutch      -> "AUR-afhankelijkheden:"
    Ukrainian  -> "Залежності в AUR:"
    Romanian   -> "Dependențe din AUR:"
    Vietnamese -> "Gói phụ thuộc của AUR:"
    _          -> "AUR dependencies:"

-- NEEDS TRANSLATION
reportPkgbuildDiffs_1 :: PkgName -> Language -> Doc AnsiStyle
reportPkgbuildDiffs_1 (bt . pnName -> p) = \case
    Japanese   -> p <> "のPKGBUILDはまだ保存されていません。"
    Arabic     -> ".مخزن الان PKGBUILD لا يوجد" <> p
    Polish     -> p <> " nie ma jeszcze przechowywanego pliku PKGBUILD."
    Croatian   -> p <> " još nema pohranjen PKGBUILD."
    German     -> p <> " hat noch keinen gespeicherten PKGBUILD."
    Spanish    -> p <> " no tiene un PKGBUILD almacenado aún."
    Portuguese -> p <> " não possui PKGBUILD."
    French     -> p <> " n'a pas encore de PKGBUILD enregistré."
    Russian    -> "У " <> p <> " ещё нет сохраненного PKGBUILD."
    Italian    -> p <> " non ci sono ancora PKGBUILD salvati"
    Serbian    -> p <> " још нема похрањен PKGBUILD."
    Norwegian  -> p <> " har ingen PKGBUILD ennå."
    Indonesia  -> p <> " tidak mempunyai PKGBUILD yang tersimpan untuk saat ini."
    Chinese    -> p <> " 还没有保存的 PKGBUILD。"
    Swedish    -> p <> " har ännu ingen PKGBUILD."
    Esperanto  -> p <> " ne havas PKGBUILD jam."
    Dutch      -> p <> " heeft nog geen opgeslagen PKGBUILD."
    Ukrainian  -> "В " <> p <> " ще не зберігається PKGBUILD."
    Romanian   -> p <> " încă nu are un PKGBUILD descărcat."
    Vietnamese -> p <> " không có sẵn PKGBUILD."
    _          -> p <> " has no stored PKGBUILD yet."

-- NEEDS TRANSLATION
reportPkgbuildDiffs_3 :: PkgName -> Language -> Doc AnsiStyle
reportPkgbuildDiffs_3 (bt . pnName -> p) = \case
    Japanese   -> p <> "のPKGBUILD変更報告："
    Arabic     -> "قد تغير PKGBUILD " <> p
    Polish     -> "Zmiany w PKGBUILD dla " <> p <> ":"
    Croatian   -> "Promjene u PKGBUILD-u za " <> p <> ":"
    German     -> "PKGBUILD-Änderungen von " <> p <> ":"
    Spanish    -> "Cambios en el PKGBUILD de " <> p <> ":"
    Portuguese -> "Mudanças no PKGBUILD de " <> p <> ":"
    Russian    -> "Изменения, вносимые " <> p <> " PKGBUILD:"
    French     -> "Changements du PKGBUILD de " <> p <> " :"
    Italian    -> "Cambiamenti nel PKGBUILD del pacchetto " <> p <>":"
    Serbian    -> "Промене PKGBUILD-a за " <> p <> ":"
    Norwegian  -> p <> "'s endringer i PKGBUILD:"
    Indonesia  -> "Perubahan PKGBUILD " <> p <> ":"
    Chinese    -> p <> " 的 PKGBUILD 变化："
    Swedish    -> "Förändringar i PKGBUILD för " <> p <> ":"
    Esperanto  -> p <> " PKGBUILD ŝanĝoj:"
    Dutch      -> p <> " PKGBUILD aanpassingen:"
    Ukrainian  -> "Зміни PKGBUILD в " <> p <> ":"
    Romanian   -> "Schimbări in PKGBUILD pentru " <> p <> ":"
    Vietnamese -> "Thay đổi trong PKGBUILD của " <> p <> ":"
    _          -> p <> " PKGBUILD changes:"

-- NEEDS TRANSLATION
reportPkgsToUpgrade_1 :: Language -> Doc AnsiStyle
reportPkgsToUpgrade_1 = \case
    Japanese   -> "アップグレードするAURパッケージ："
    Arabic     -> "للتحديث AURرزمة اﻟ"
    Polish     -> "Pakiety z AUR do zaktualizowania:"
    Croatian   -> "AUR paketi za nadogradnju:"
    Swedish    -> "AUR-paket att uppgradera:"
    German     -> "Zu aktualisierendes AUR-Paket:"
    Spanish    -> "Paquetes de AUR a actualizar:"
    Portuguese -> "Pacotes do AUR para atualizar:"
    French     -> "Paquets AUR à mettre à jour :"
    Russian    -> "Пакеты AUR, готовые для обновления:"
    Italian    -> "Pacchetti dell'AUR da aggiornare:"
    Serbian    -> "Пакети из AUR-а за надоградњу:"
    Norwegian  -> "AUR-pakker å oppgradere:"
    Indonesia  -> "Paket AUR yang akan ditingkatkan:"
    Chinese    -> "要升级的 AUR 包："
    Esperanto  -> "Pakaĵoj de AUR ĝisdatigi:"
    Dutch      -> "AUR-Pakketten om te upgraden:"
    Ukrainian  -> "Пакунки AUR, готові для оновлення:"
    Romanian   -> "Pachete din AUR de actualizat:"
    Vietnamese -> "Cập nhật các gói AUR:"
    _          -> "AUR Packages to upgrade:"

-- NEEDS UPDATING
reportBadDowngradePkgs_1 :: Language -> Doc AnsiStyle
reportBadDowngradePkgs_1 = \case
    Japanese   -> "このパッケージはキャッシュには入っていないので、ダウングレードできません。"
    Arabic     -> "ما يلي ليس له إصدارات في ذاكرة التخزين المؤقت ، وبالتالي لا يمكن الرجوع إلى الإصدار السابق."
    Polish     -> "Poniższe pakiety nie są zainstalowane i nie mogą być zainstalowane w starszej wersji:"
    Croatian   -> "Sljedeći paketi nisu instalirani te se stoga ne mogu vratiti na stare verzije:"
    Swedish    -> "Följande paket är inte installerade, och kan därför inte bli nergraderade:"
    German     -> "Folgende Pakete sind in keiner Version im Cache und können daher nicht gedowngradet werden:"
    Spanish    -> "Los siguientes paquetes no tienen versiones en la caché, por lo que no se pueden bajar a versiones anteriores:"
    Portuguese -> "Os seguintes pacotes não possuem versões no cache, logo não podem retornar a uma versão anterior:"
    French     -> "Aucune version des paquets suivants n'est présente dans le cache ; ils ne peuvent pas être mis à niveau à une version antérieure :"
    Russian    -> "Следующих пакетов нет в кэше. Следовательно, они не могут быть откачены к старой версии:"
    Italian    -> "Nessuna versione dei seguenti pacchetti è disponibile nella cache, perciò non è possibile riportarli ad una versione precedente:"
    Serbian    -> "Следећи пакети нису ни инсталирани, те се не могу вратити на старију верзију:"
    Norwegian  -> "Følgende pakker har ingen versjoner i cache, og kan derfor ikke bli nedgradert:"
    Indonesia  -> "Berikut ini tidak mempunyai versi pada cache, sehingga tidak akan diturunkan:"
    Chinese    -> "以下包在缓存中没有版本，所以无法被降级："
    Esperanto  -> "La sekvaj pakaĵoj havas ne kaŝmemorigitajn versiojn, do ĝi ne povas malpromociigi:"
    Dutch      -> "De volgende pakketten hebben geen versie in de cache, en kunnen dus niet gedowngrade worden."
    Ukrainian  -> "Наступних пакунків немає в кеші. Отже, вони не можуть відкотитися до старої версії:"
    Romanian   -> "Aceste pachete nu au nici o versiune disponibilă în cache, așa că nu pot fi retrogradate:"
    Vietnamese -> "Những gói sau không có bản nào trong cache, vì vậy không thể hạ cấp:"
    _          -> "The following have no versions in the cache, and thus can’t be downgraded:"

reportBadDowngradePkgs_2 :: PkgName -> Language -> Doc AnsiStyle
reportBadDowngradePkgs_2 (PkgName p) = \case
  Spanish   -> pretty p <+> "no tiene una versión en la caché."
  Arabic    -> ".ليس  لديه اصدار في الذاكرة التخزين الموقت" <+> pretty p
  Italian   -> pretty p <+> "non ha alcuna versione nella cache."
  Dutch     -> pretty p <+> "heeft geen versie in de cache."
  Ukrainian -> pretty p <+> "не має версії в кеші."
  Romanian  -> pretty p <+> "nu are nici o versiune în cache."
  Vietnamese -> pretty p <+> "không có bản nào trong cache."
  _         -> pretty p <+> "has no version in the cache."

upgradeAURPkgs_1 :: Language -> Doc AnsiStyle
upgradeAURPkgs_1 = \case
    Japanese   -> "パッケージ情報をダウンロード中・・・"
    Arabic     -> "...احضار معلومات الحزمة"
    Polish     -> "Pobieranie informacji o pakietach..."
    Croatian   -> "Preuzimanje podataka o paketima..."
    Swedish    -> "Hämtar paketinformation..."
    German     -> "Rufe Paketinformationen ab..."
    Spanish    -> "Obteniendo información de los paquetes..."
    Portuguese -> "Obtendo informação dos pacotes..."
    French     -> "Obtention des informations des paquets en cours…"
    Russian    -> "Сборка информации о пакетах..."
    Italian    -> "Ottenimento di informazioni sui pacchetti..."
    Serbian    -> "Преузимање информација о пакетима..."
    Norwegian  -> "Henter pakkeinformasjon..."
    Indonesia  -> "Mengambil informasi paket..."
    Chinese    -> "正在获取包信息..."
    Esperanto  -> "Venigas informacion de pakaĵoj..."
    Dutch      -> "Pakket informatie aan het ophalen..."
    Ukrainian  -> "Збираємо інформацію про пакунок..."
    Romanian   -> "Se obțin informații despre pachete..."
    Vietnamese -> "Cập nhật thông tin của gói..."
    _          -> "Fetching package information..."

upgradeAURPkgs_2 :: Language -> Doc AnsiStyle
upgradeAURPkgs_2 = \case
    Japanese   -> "バージョンを比較中・・・"
    Arabic     -> "...مقارنة اصدارات الحزمة"
    Polish     -> "Porównywanie wersji pakietów..."
    Croatian   -> "Uspoređivanje verzija paketa..."
    Swedish    -> "Jämför paket-versioner..."
    German     -> "Vergleiche Paketversionen..."
    Spanish    -> "Comparando versiones de los paquetes..."
    Portuguese -> "Comparando versões dos pacotes..."
    French     -> "Comparaison des versions des paquets en cours…"
    Russian    -> "Сравнение версий пакетов..."
    Italian    -> "Esecuzione di un confronto fra le versioni dei pacchetti..."
    Serbian    -> "Упоређивање верзија пакета..."
    Norwegian  -> "Sammenligner pakkeversjoner..."
    Indonesia  -> "Membandingkan versi paket..."
    Chinese    -> "正在比较包的版本..."
    Esperanto  -> "Komparas versiojn de pakaĵoj..."
    Dutch      -> "Pakket versies aan het vergelijken..."
    Ukrainian  -> "Порівнюємо версії пакунків..."
    Romanian   -> "Se compar versiunile pacheturilor..."
    Vietnamese -> "So sánh phiên bản của gói..."
    _          -> "Comparing package versions..."

upgradeAURPkgs_3 :: Language -> Doc AnsiStyle
upgradeAURPkgs_3 = \case
    Japanese   -> "アップグレードは必要ありません。"
    Arabic     -> ".AURلا يلزم تحديث حزمة اﻟ"
    Polish     -> "Nie jest wymagana aktualizacja pakietów z AUR."
    Croatian   -> "Svi AUR paketi su ažurirani."
    Swedish    -> "Inga AUR-paketsuppgraderingar behövs."
    German     -> "Keine Aktualisierungen für AUR-Paket notwendig."
    Spanish    -> "No es necesario actualizar paquetes de AUR."
    Portuguese -> "Nenhum pacote do AUR precisa de atualização."
    French     -> "Aucune mise à jour de paquet AUR n'est nécessaire."
    Russian    -> "Обновление пакетов из AUR не требуется."
    Italian    -> "Nessun pacchetto dell'AUR necessita di aggiornamenti."
    Serbian    -> "Ажурирање пакета из AUR-а није потребно."
    Norwegian  -> "Ingen pakkeoppgradering fra AUR nødvendig."
    Indonesia  -> "Tidak ada peningkatan AUR yang dibutuhkan."
    Chinese    -> "没有需要升级的 AUR 包。"
    Esperanto  -> "Ne ĝisdatigoj de pakaĵoj de AUR necesas."
    Dutch      -> "Geen AUR-pakket upgrades vereist."
    Ukrainian  -> "Пакунки AUR не потребують оновлення."
    Romanian   -> "Nu e nevoie să se actualizeze nici un pachet din AUR."
    Vietnamese -> "Không có cập nhật cho các gói AUR."
    _          -> "No AUR package upgrades necessary."

removeMakeDepsAfter_1 :: Language -> Doc AnsiStyle
removeMakeDepsAfter_1 = \case
    Japanese   -> "あと片付け。必要ないパッケージを削除："
    Arabic     -> "...التي لا لزوم لها makeازالة التبعيات اﻟ"
    Polish     -> "Usuwanie niepotrzebnych zależności potrzebnych do budowy..."
    Croatian   -> "Uklanjanje nepotrebnih zavisnosti vezanih uz izgradnju..."
    Swedish    -> "Tar bort obehövda beroenden för `make`..."
    German     -> "Entferne nicht benötigte make-Abhängigkeiten..."
    Spanish    -> "Removiendo dependencias `make` innecesarias..."
    Portuguese -> "Removendo dependências `make` desnecessárias..."
    French     -> "Suppression des dépendances inutiles…"
    Russian    -> "Удаление ненужных зависимостей make..."
    Italian    -> "Rimozione delle dipendenze utilizzate per la compilazione..."
    Serbian    -> "Уклањање непотребних зависности за изградњу..."
    Norwegian  -> "Fjerner unødvendige make-avhengigheter..."
    Indonesia  -> "Menghapus dependensi `make` yang tidak dibutuhkan..."
    Chinese    -> "移除不需要的 make 依赖..."
    Esperanto  -> "Forigas nenecesajn dependecojn de make..."
    Dutch      -> "Onnodige make afhankelijkheden aan het verwijderen..."
    Ukrainian  -> "Видаляємо непотрібні залежності make..."
    Romanian   -> "Se șterg dependențele de compilare inutile..."
    Vietnamese -> "Loại bỏ các gói phụ thuộc khi make không cần thiết..."
    _          -> "Removing unneeded make dependencies..."

----------------------------
-- Aura/Commands/B functions
----------------------------
-- NEEDS TRANSLATION
cleanStates_2 :: Int -> Language -> Doc AnsiStyle
cleanStates_2 n@(bt . tshow -> s) = \case
    Japanese   -> s <> "個のパッケージ状態記録だけが残される。その他削除？"
    Arabic     -> "سيتم الاحتفاظ بحالات الحزمة.هل تريد ازالة الباقي؟ " <> s
    Polish     -> s <> " stan pakietów zostanie zachowany. Usunąć resztę?"
    Croatian   -> s <> " stanja paketa će biti zadržano. Ukloniti ostatak?"
    German     -> s <> " Paketzustände werden behalten. Den Rest entfernen?"
    Spanish    -> "El estado del paquete" <> s <> " se mantendrá. ¿Deseas eliminar el resto?"
    Serbian    -> s <> " стања пакета ће бити сачувано. Уклонити остатак?"
    Norwegian  -> s <> " pakketilstander vil bli beholdt. Vil du fjerne resten?"
    Italian    -> "Lo stato del pacchetto" <> s <> " sarà mantenuto. Rimuovere il resto?"
    Portuguese -> s <> " estados de pacotes serão mantidos. Remover o resto?"
    French     -> s <> " états des paquets vont être conservés. Supprimer le reste ?"
    Russian    -> s <> pluralRussian " состояние пакетов будет оставлено." " состояния пакетов будут оставлены." " состояний пакетов будет оставлено." n <> " Удалить оставшиеся?"
    Indonesia  -> s <> " paket akan tetap sama. Hapus yang lainnya?"
    Chinese    -> s <> " 个包的状态将会保留。删除其它的？"
    Swedish    -> s <> " paket kommer att bevaras. Ta bort resten?"
    Esperanto  -> s <> " statoj de pakaĵoj teniĝas. Ĉu forigi la ceteron?"
    Dutch      -> s <> " pakketstatussen worden behouden. De rest verwijderen?"
    Ukrainian  -> s <> " стан пакунків будуть залишені. Видалити решту?"
    Romanian   -> "Stările pachetului " <> s <> " vor fi păstrate. Să se șteargă restul?"
    Vietnamese -> "Trạng thái của gói " <> s <> " sẽ được lưu lại. Loại bỏ phần còn lại?"
    _          -> s <> " package states will be kept. Remove the rest?"

-- NEEDS TRANSLATION
cleanStates_3 :: Language -> Doc AnsiStyle
cleanStates_3 = \case
    Japanese   -> "何も削除しないで終了します。"
    Arabic     -> ".لم يتم ازالة اي حالة حزمة"
    Polish     -> "Żaden stan pakietu nie został usunięty."
    Croatian   -> "Nijedno stanje paketa nije uklonjeno."
    German     -> "Keine Paketzustände wurden entfernt."
    Spanish    -> "No se han eliminado estados de los paquetes."
    Serbian    -> "Ниједно стање пакета није уклоњено."
    Norwegian  -> "Ingen pakketilstander ble fjernet."
    Italian    -> "Nessuno stato dei pacchetti è stato rimosso."
    Portuguese -> "Nenhum estado de pacote será removido."
    French     -> "Aucun état des paquets n'a été supprimé."
    Russian    -> "Состояния пакетов отались нетронутыми."
    Indonesia  -> "Tidak ada paket yang dihapus."
    Chinese    -> "没有删除任何包。"
    Swedish    -> "Inga paket togs bort."
    Esperanto  -> "Ne statojn de pakaĵoj forigis."
    Dutch      -> "Geen pakketstatussen verwijderd."
    Ukrainian  -> "Стани пакунків залишились недоторкані."
    Romanian   -> "Nici o stare de pachet a fost ștearsă."
    Vietnamese -> "Không có trạng thái gói nào được lưu."
    _          -> "No package states were removed."

cleanStates_4 :: Int -> Language -> Doc AnsiStyle
cleanStates_4 n = \case
  Japanese  -> "現在のパッケージ状態記録：" <+> pretty n <+> "個。"
  Arabic    -> ".محفوظة " <+> pretty n <+> " لديك حاليا حالات حزمة"
  Polish    -> "Chwilowo posiadasz" <+> pretty n <+> "zapisanych stanów pakietów."
  Spanish   -> "Actualmente tiene " <+> pretty n <+> "estados de paquetes guardados."
  Russian   -> "У вас сейчас " <+> pretty n <+> pluralRussian " сохраненное состояние пакета" " сохраненных состояний пакета" " сохраненных состояний пакетов." n
  Italian   -> "Al momento ci sono" <+> pretty n <+> "stati di pacchetti salvati."
  Esperanto -> "Vi havas " <+> pretty n <+> " konservajn statojn de pakaĵoj."
  Dutch     -> "U heeft momenteel" <+> pretty n <+> "opgeslagen pakketstatussen."
  Ukrainian -> "Зараз ви маєте " <+> pretty n <+> " збережених станів пакунків."
  Romanian  -> "Momentan aveți " <+> pretty n <+> " stări de pachet salvate."
  Vietnamese -> "Bạn hiện đã lưu " <+> pretty n <+> " trạng thái gói."
  _         -> "You currently have " <+> pretty n <+> " saved package states."

cleanStates_5 :: Text -> Language -> Doc AnsiStyle
cleanStates_5 t = \case
  Japanese  -> "一番最近に保存されたのは：" <+> pretty t
  Arabic    -> pretty t <+> ":احدث ما تم حفظه"
  Polish    -> "Ostatnio zapisane:" <+> pretty t
  Spanish   -> "Guardado recientemente:" <+> pretty t
  Russian   -> "Последнее сохраненное:" <+> pretty t
  Italian   -> "Salvato più recentemente:" <+> pretty t
  Esperanto -> "Lastaj konservaj:" <+> pretty t
  Dutch     -> "Meest recent opgeslagen:" <+> pretty t
  Ukrainian -> "Останні збереженні:" <+> pretty t
  Romanian  -> "Cel mai recent salvat:" <+> pretty t
  Vietnamese -> "Lần lưu gần nhất:" <+> pretty t
  _         -> "Most recently saved:" <+> pretty t

cleanStates_6 :: Int -> Language -> Doc AnsiStyle
cleanStates_6 n = \case
  Polish    -> pretty n <+> "jest przypiętych i nie zostanie usuniętych."
  Arabic    -> ".اذا كانو مثبتين ولا يمكن ازالتهم " <+> pretty n
  Spanish   -> pretty n <+> "de estos están anclados y no se eliminarán."
  Italian   -> pretty n <+> "di questi sono stati fissati, perciò non saranno rimossi."
  Dutch     -> pretty n <+> "hiervan zijn vastgezet, en worden niet verwijderd."
  Ukrainian -> pretty n <+> "були закріплені та залишуться недоторканими."
  Romanian  -> pretty n <+> "dintre astea sunt fixate, și nu vor fi șterse."
  Vietnamese -> pretty n <+> "trong số chúng đã được ghim, và sẽ không bị loại bỏ."
  _         -> pretty n <+> "of these are pinned, and won't be removed."

readState_1 :: Language -> Doc AnsiStyle
readState_1 = \case
    Polish     -> "Ten plik stanu nie mógł zostać odczytany. Czy jest to prawidłowy plik JSON?"
    Arabic     -> "صحيح؟ JSON فشل في تحليل ملف الحالة. هل"
    Spanish    -> "Ese archivo de estado no se pudo analizar. ¿Es un archivo JSON válido?"
    Portuguese -> "O arquivo de estado não pôde ser interpretado. É um arquivo JSON válido?"
    Russian    -> "Это состояние не распознано. Это корректный JSON?"
    Italian    -> "Non è stato possibile analizzare il file di stato. E' correttamente formattato in JSON?"
    Esperanto  -> "Tiu statdosiero paneis sintake analizi. Ĉu ĝi estas valida JSON?"
    Dutch      -> "Dat statusbestand kon niet worden geparseerd. Is het legale JSON?"
    Ukrainian  -> "Стан не був розпізнаний правильно. Це точно коректний JSON?"
    Romanian   -> "Acel fișier de stare nu se putea analiza. Este un fișier JSON valabil?"
    Vietnamese -> "Thất bại trong việc lấy dữ liệu từ tệp. Đó có đúng là tệp JSON?"
    _          -> "That state file failed to parse. Is it legal JSON?"

----------------------------
-- Aura/Commands/C functions
----------------------------
getDowngradeChoice_1 :: PkgName -> Language -> Doc AnsiStyle
getDowngradeChoice_1 (bt . pnName -> p) = \case
    Japanese   -> p <> "はどのバージョンにしますか？"
    Arabic     -> "الذي تريده؟ . " <> p <> " ما هو اصدار"
    Polish     -> "Którą wersję pakietu " <> p <> " zainstalować?"
    Croatian   -> "Koju verziju paketa " <> p <> " želite?"
    Swedish    -> "Vilken version av " <> p <> " vill du ha?"
    German     -> "Welche Version von " <> p <> " möchten Sie haben?"
    Spanish    -> "¿Qué versión de " <> p <> " deseas?"
    Portuguese -> "Qual versão de " <> p <> " deseja?"
    French     -> "Quelle version de " <> p <> " voulez-vous ?"
    Russian    -> "Какую версию " <> p <> " вы хотите?"
    Italian    -> "Quale versione di " <> p <> " preferisci?"
    Serbian    -> "Коју верзију " <> p <> "-а желите?"
    Norwegian  -> "Hvilken versjon av " <> p <> " vil du ha?"
    Indonesia  -> "Versi dari paket " <> p <> " mana yang anda inginkan?"
    Chinese    -> "你希望安装 " <> p <> " 的哪个版本？"
    Esperanto  -> "Kiu versio de " <> p <> " vi volas?"
    Dutch      -> "Welke versie van " <> p <> " wil je?"
    Ukrainian  -> "Яку версію пакунку " <> p <> " ви бажаєте?"
    Romanian   -> "Care versiune al pachetului " <> p <> " o doriți?"
    Vietnamese -> "Bạn muốn sử dụng phiên bản nào của " <> p <> "?"
    _          -> "What version of " <> p <> " do you want?"

backupCache_3 :: Language -> Doc AnsiStyle
backupCache_3 = \case
    Japanese   -> "バックアップ先は存在しません。"
    Arabic     -> ".عدم وجود موقع النسخ الاحتياطي"
    Polish     -> "Lokalizacja kopii zapasowych nie istnieje."
    Croatian   -> "Lokacija sigurnosne kopije ne postoji."
    Swedish    -> "Specifierad backup-plats finns inte."
    German     -> "Der Sicherungsort existiert nicht."
    Spanish    -> "La localización para copia de seguridad no existe."
    Portuguese -> "Localização do backup não existe."
    French     -> "Le chemin des copies de sauvegarde spécifié n'existe pas."
    Russian    -> "Путь к бэкапу не существует."
    Italian    -> "La locazione di backup non esiste."
    Serbian    -> "Путања ка бекапу не постоји."
    Norwegian  -> "Spesifisert backup-plass finnes ikke."
    Indonesia  -> "Lokasi `backup` tidak ada."
    Chinese    -> "备份位置不存在。"
    Esperanto  -> "La savkopia loko ne ekzistas."
    Dutch      -> "De back-up lokatie bestaat niet."
    Ukrainian  -> "Шлях до резервної копії не існує."
    Romanian   -> "Locul de reservă nu există."
    Vietnamese -> "Đường dẫn sao lưu không tồn tại."
    _          -> "The backup location does not exist."

backupCache_4 :: FilePath -> Language -> Doc AnsiStyle
backupCache_4 (bt . T.pack -> dir) = \case
    Japanese   -> "キャッシュのバックアップ先：" <> dir
    Arabic     -> dir <> " التنسيخ الاحتياطي الى"
    Polish     -> "Tworzenie kopii zapasowej pamięci podręcznej w " <> dir
    Croatian   -> "Stvaram sigurnosnu kopiju u " <> dir
    Swedish    -> "Tar backup på cache-filer till " <> dir
    German     -> "Sichere Cache in " <> dir
    Spanish    -> "Haciendo una copia de seguridad de la caché en " <> dir
    Portuguese -> "Backup do cache sendo feito em " <> dir
    French     -> "Copie de sauvegarde dans " <> dir <> "."
    Russian    -> "Бэкап создается в директории " <> dir
    Italian    -> "Eseguo un backup della cache in " <> dir
    Serbian    -> "Бекапујем кеш у " <> dir
    Norwegian  -> "Tar backup på cache til " <> dir
    Indonesia  -> "Melakukan `backup` pada direktori " <> dir
    Chinese    -> "正在将缓存备份到 " <> dir
    Esperanto  -> "Enarkivigas la kaŝdosieron al " <> dir
    Dutch      -> "Back-up van cache aan het maken naar " <> dir
    Ukrainian  -> "Зберігаємо резервну копію до " <> dir
    Romanian   -> "Se copiază cache-ul de rezervă către " <> dir
    Vietnamese -> "Sao lưu cache vào " <> dir
    _          -> "Backing up cache to " <> dir

backupCache_5 :: Int -> Language -> Doc AnsiStyle
backupCache_5 (bt . tshow -> n) = \case
    Japanese   -> "パッケージのファイル数：" <> n
    Arabic     -> n <> " :حزمة الملفات الى النسخ الاحتياطي"
    Polish     -> "Pliki będące częścią\xa0kopii zapasowej: " <> n
    Croatian   -> "Datoteke koje su dio sigurnosne kopije: " <> n
    Swedish    -> "Paket-filer att ta backup på: " <> n
    German     -> "Zu sichernde Paketdateien: " <> n
    Spanish    -> "Ficheros de paquetes de los que se hará copia de seguridad: " <> n
    Portuguese -> "Arquivos de pacotes para backup: " <> n
    French     -> "Copie de sauvegarde des fichiers de paquets suivants : " <> n
    Russian    -> "Файлы пакета для бэкапа: " <> n
    Italian    -> "Archivi dei pacchetti per cui sarà eseguito un backup: " <> n
    Serbian    -> "Датотеке за бекап: " <> n
    Norwegian  -> "Pakker som blir tatt backup på: " <> n
    Indonesia  -> "Jumlah paket yang di-`backup`: " <> n
    Chinese    -> "将要备份的包文件：" <> n
    Esperanto  -> "La dosierojn de la pakaĵoj enarkivigi: " <> n
    Dutch      -> "Pakketbestanden om te back-uppen: " <> n
    Ukrainian  -> "Файли пакунку для резервної копії: " <> n
    Romanian   -> "Fișiere de pachet pentru copiare de rezervă: " <> n
    Vietnamese -> "Các tệp của gói sẽ được sao lưu: " <> n
    _          -> "Package files to backup: " <> n

backupCache_6 :: Language -> Doc AnsiStyle
backupCache_6 = \case
    Japanese   -> "バックアップを実行しますか？"
    Arabic     -> "هل تريد ان تكمل النسخ الاحتياطي؟"
    Polish     -> "Kontynuować tworzenie kopii zapasowej?"
    Croatian   -> "Nastavi sa stvaranjem sigurnosne kopije?"
    Swedish    -> "Fortsätt med backup?"
    German     -> "Sicherung fortsetzen?"
    Spanish    -> "¿Proceder con la copia de seguridad?"
    Portuguese -> "Proceder com o backup?"
    French     -> "Procéder à la copie de sauvegarde ?"
    Russian    -> "Продолжить создание бэкапа?"
    Italian    -> "Procedere con il backup?"
    Serbian    -> "Наставити бекаповање?"
    Norwegian  -> "Fortsett med backup?"
    Indonesia  -> "Lanjutkan dengan `backup`?"
    Chinese    -> "开始备份？"
    Esperanto  -> "Ĉu daŭrigu enarkivigi?"
    Dutch      -> "Doorgaan met back-up?"
    Ukrainian  -> "Продовжити створення резервної копії?"
    Romanian   -> "Continuați cu copiile de rezervă?"
    Vietnamese -> "Tiến hành sao lưu?"
    _          -> "Proceed with backup?"

backupCache_7 :: Language -> Doc AnsiStyle
backupCache_7 = \case
    Japanese   -> "バックアップは意図的に阻止されました。"
    Arabic     -> ".اقاف النسخ الاحتياطي يدويا"
    Polish     -> "Tworzenie kopii zapasowej zostało przerwane przez użytkownika."
    Croatian   -> "Stvaranje sigurnosne kopije prekinuto od strane korisnika."
    Swedish    -> "Backup avbröts manuellt."
    German     -> "Backup durch Benutzer abgebrochen."
    Spanish    -> "Copia de seguridad abortada manualmente."
    Portuguese -> "Backup cancelado manualmente."
    French     -> "Copie de sauvegarde manuelle annulée."
    Russian    -> "Создание бэкапа прервано пользователем."
    Italian    -> "Backup interrotto manualmente."
    Serbian    -> "Бекаповање је ручно прекинуто."
    Norwegian  -> "Backup ble avbrutt manuelt."
    Indonesia  -> "Proses `backup` dibatalkan secara paksa."
    Chinese    -> "手动备份已中止。"
    Esperanto  -> "Enarkivigadon ĉesigis permane."
    Dutch      -> "Back-up handmatig afgebroken."
    Ukrainian  -> "Створення резервної копії перервано користувачем."
    Romanian   -> "Copiarea de rezervă anulată manual."
    Vietnamese -> "Quá trình sao lưu được hủy."
    _          -> "Backup manually aborted."

backupCache_8 :: Language -> Doc AnsiStyle
backupCache_8 = \case
    Japanese   -> "バックアップ中。数分かかるかもしれません。"
    Arabic     -> "...النسخ الاحتياطي. هذه العملية يمكن ان تاخذ وقت"
    Polish     -> "Tworzenie kopii zapasowej. To może potrwać kilka minut..."
    Croatian   -> "Stvaranje sigurnosne kopije. Ovo može potrajati nekoliko minuta..."
    Swedish    -> "Tar backup. Det här kan ta ett tag..."
    German     -> "Sichere. Dies kann einige Minuten dauern..."
    Spanish    -> "Haciendo copia de seguridad. Esto puede tardar unos minutos..."
    Portuguese -> "Efetuando backup. Isso pode levar alguns minutos..."
    French     -> "Copie de sauvegarde en cours. Ceci peut prendre quelques minutes…"
    Russian    -> "Создается бэкап. Это может занять пару минут..."
    Italian    -> "Esecuzione del backup in corso. Potrebbe volerci qualche minuto..."
    Serbian    -> "Бекапујем. Ово може да потраје пар минута..."
    Norwegian  -> "Tar backup. Dette kan ta en stund..."
    Indonesia  -> "Melakukan `backup`. Proses ini akan berjalan untuk beberapa menit..."
    Chinese    -> "正在备份中。可能需要几分钟的时间..."
    Esperanto  -> "Enarkiviganta. Ĉi tiu eble daŭros dum kelkaj tagoj..."
    Dutch      -> "Aan het back-uppen. Dit kan een aantal minuten duren..."
    Ukrainian  -> "Створюємо резервну копію. Це може зайняти декілька хвилин..."
    Romanian   -> "Se fac copii de rezervă. Ar putea să dureze câteva minute..."
    Vietnamese -> "Đang sao lưu. Có thể sẽ mất vài phút..."
    _          -> "Backing up. This may take a few minutes..."

copyAndNotify_1 :: Int -> Language -> Doc AnsiStyle
copyAndNotify_1 (cyan . pretty -> n) = \case
    Japanese   -> "#[" <> n <> "]をコピー中・・・"
    Arabic     -> "["<> n <> "]# نسخ"
    Polish     -> "Kopiowanie #[" <> n <> "]"
    Turkish    -> "Kopyalanıyor #[" <> n <> "]"
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
    Esperanto  -> "Kopianta #[" <> n <> "]"
    Dutch      -> "Kopiëren #[" <> n <> "]"
    Ukrainian  -> "Копіюємо #[" <> n <> "]"
    Romanian   -> "Se copiază #[" <> n <> "]"
    Vietnamese -> "Sao chép #[" <> n <> "]"
    _          -> "Copying #[" <> n <> "]"

cleanCache_2 :: Language -> Doc AnsiStyle
cleanCache_2 = \case
    Japanese   -> "パッケージ・キャッシュは完全に削除されます。"
    Arabic     -> ".هذا سوف يحذف ذاكرة التخزين الموقت للحزمة بالكامل"
    Polish     -> "To usunie WSZYSTKIE pakiety z pamięci podręcznej."
    Croatian   -> "Ovo će izbrisati CIJELI cache paketa."
    Swedish    -> "Detta kommer ta bort HELA paket-cachen."
    German     -> "Dies wird den GESAMTEN Paketcache leeren."
    Spanish    -> "Esto eliminará POR COMPLETO la caché de paquetes."
    Portuguese -> "Isso removerá TODOS OS PACOTES do cache."
    French     -> "Ceci va supprimer la TOTALITÉ du cache des paquets."
    Russian    -> "Это действие ВСЕЦЕЛО уничтожит кэш пакетов."
    Italian    -> "Questa operazione cancellerà l'INTERA cache dei pacchetti."
    Serbian    -> "Ово ће избрисати ЦЕО кеш пакета."
    Norwegian  -> "Dette vil slette HELE pakke-cachen."
    Indonesia  -> "Akan menghapus SEMUA `cache` paket"
    Chinese    -> "这将会删除全部的包缓存。"
    Esperanto  -> "Ĉi tiu forigos la TUTAN kaŝmemoron de pakaĵoj."
    Dutch      -> "Hiermee wordt de GEHELE pakketcache verwijderd."
    Ukrainian  -> "Ця операція ПОВНІСТЮ видалить кеш пакунків."
    Romanian   -> "Asta va șterge COMPLET cache-ul de pachete."
    Vietnamese -> "Điều này sẽ xóa TOÀN BỘ cache của gói."
    _          -> "This will delete the ENTIRE package cache."

cleanCache_3 :: Word -> Language -> Doc AnsiStyle
cleanCache_3 n@(bt . tshow -> s) = \case
    Japanese   -> "パッケージ・ファイルは" <> s <> "個保存されます。"
    Arabic     -> ".من كل ملف حزمة سيتم الاحتفاظ بها " <> s
    Polish     -> s <> " wersji każdego pakietu zostanie zachowane."
    Croatian   -> s <> " zadnjih verzija svakog paketa će biti zadržano."
    Swedish    -> s <> " av varje paketfil kommer att sparas."
    German     -> s <> " jeder Paketdatei wird behalten."
    Spanish    -> "Se mantendrán " <> s <> " ficheros de cada paquete."
    Portuguese -> s <> " arquivos de cada pacote serão mantidos."
    French     -> s <> " fichiers de chaque paquet sera conservé."
    Russian    -> s <> pluralRussian " версия каждого пакета будет нетронута." " версии каждого пакета будут нетронуты." " версий каждого пакета будут нетронуты." n
    Italian    -> "Saranno mantenuti " <> s <> " file di ciascun pacchetto."
    Serbian    -> s <> " верзије сваког од пакета ће бити сачуване."
    Norwegian  -> s <> " av hver pakkefil blir beholdt."
    Indonesia  -> s <> " berkas dari tiap paket akan disimpan."
    Chinese    -> "每个包文件将会保存 " <> s <> " 个版本。"
    Esperanto  -> s <> " de ĉiu dosiero de pakaĵo teniĝos."
    Dutch      -> s <> " van elk pakketbestand wordt bewaard."
    Ukrainian  -> s <> " версія кожного пакунку залишиться недоторканою."
    Romanian   -> s <> " din fiecare fișier de pachet vor fi păstrate."
    Vietnamese -> "Sẽ giữ lại " <> s <> " tệp của gói."
    _          -> s <> " of each package file will be kept."

cleanCache_4 :: Language -> Doc AnsiStyle
cleanCache_4 = \case
    Japanese   -> "残りは全部削除されます。承知していますか？"
    Arabic     -> "سيتم حذف الباقي. هل تريد أن تكمل؟"
    Polish     -> "Wszystko inne zostanie usunięte. Na pewno?"
    Croatian   -> "Ostali paketi će biti izbrisani. Jeste li sigurni?"
    Swedish    -> "Resten kommer att tas bort. Är det OK?"
    German     -> "Der Rest wird gelöscht. Ist das OK?"
    Spanish    -> "El resto se eliminarán. ¿De acuerdo?"
    Portuguese -> "O resto será removido. OK?"
    French     -> "Le reste sera supprimé. Êtes-vous d'accord ?"
    Russian    -> "Всё остальное будет удалено. Годится?"
    Italian    -> "Il resto sarà rimosso. Continuare?"
    Serbian    -> "Остатак ће бити избрисан. Да ли је то у реду?"
    Norwegian  -> "Resten vil bli slettet. Er det OK?"
    Indonesia  -> "Selainnya akan dihapus. Ikhlas kan?"
    Chinese    -> "其余的将会被删除。确定？"
    Esperanto  -> "La cetero foriĝos. Ĉu bone?"
    Dutch      -> "De rest wordt verwijderd. OK?"
    Ukrainian  -> "Все інше буде видалено. Гаразд?"
    Romanian   -> "Restul va fi șters. De acord?"
    Vietnamese -> "Xóa bỏ phần còn lại. Ok?"
    _          -> "The rest will be deleted. Okay?"

cleanCache_5 :: Language -> Doc AnsiStyle
cleanCache_5 = \case
    Japanese   -> "削除の続行は意図的に阻止されました。"
    Arabic     -> ".تم ايقاف تنظيف ذاكرة الموقت يدويا"
    Polish     -> "Czyszczenie pamięci podręcznej zostało przerwane przez użytkownika."
    Croatian   -> "Čišćenje cache-a paketa prekinuto od strane korisnika."
    Swedish    -> "Cache-rensning avbröts manuellt."
    German     -> "Leeren des Caches durch Benutzer abgebrochen."
    Spanish    -> "Limpieza de la caché abortada manualmente."
    Portuguese -> "Limpeza do cache cancelada manualmente."
    French     -> "Le nettoyage du cache a été arrêté manuellement."
    Russian    -> "Очистка кэша прервана пользователем."
    Italian    -> "La pulizia della cache è stata interrotta manualmente."
    Serbian    -> "Чишћење кеша је ручно прекинуто."
    Norwegian  -> "Cache-rensing ble avbrutt manuelt."
    Indonesia  -> "Pembersihan `cache` dibatalkan secara paksa."
    Chinese    -> "手动清理缓存已中止。"
    Esperanto  -> "Puriganta Kaŝmemoro ĉesis permane."
    Dutch      -> "Cachereiniging handmatig afgebroken."
    Ukrainian  -> "Очищення кешу було перервано користувачем."
    Romanian   -> "Curățenia cache-ului anulată manual."
    Vietnamese -> "Đã hủy xóa cache."
    _          -> "Cache cleaning manually aborted."

cleanCache_6 :: Language -> Doc AnsiStyle
cleanCache_6 = \case
    Japanese   -> "パッケージ・キャッシュを掃除中・・・"
    Arabic     -> "...تنظيف رزمة الذاكرة التخزين الموقت"
    Polish     -> "Czyszczenie pamięci podręcznej..."
    Croatian   -> "Čišćenje cache-a paketa..."
    Swedish    -> "Rensar paket-cache..."
    German     -> "Leere Paketcache..."
    Spanish    -> "Limpiando la caché de paquetes..."
    Portuguese -> "Limpando cache de pacotes..."
    French     -> "Nettoyage du cache des paquets…"
    Russian    -> "Очистка кэша пакета..."
    Italian    -> "Pulizia della cache dei pacchetti..."
    Serbian    -> "Чишћење кеша..."
    Norwegian  -> "Renser pakke-cache..."
    Indonesia  -> "Membersihkan `cache` paket..."
    Chinese    -> "正在清理包缓存..."
    Esperanto  -> "Purigas Kaŝmemoron de pakaĵoj..."
    Dutch      -> "Pakketcache aan het reinigen..."
    Ukrainian  -> "Очищуємо кеш пакунків..."
    Romanian   -> "Se curăță cache-ul de pachete..."
    Vietnamese -> "Xóa cache..."
    _          -> "Cleaning package cache..."

cleanCache_7 :: Word -> Word -> Language -> Doc AnsiStyle
cleanCache_7 (bt . tshow -> ps) (bt . tshow -> bytes) = \case
    Arabic     -> ".ميغابايت " <> bytes <> " الذي تاخذ " <> ps <> " الذاكرة التخزين الموقت فيه الرزمة"
    Polish     -> "Pamięć podręczna posiada " <> ps <> " pakietów, zajmujących " <> bytes <> " megabajtów."
    Spanish    -> "La caché contiene " <> ps <> " paquetes, consumiendo " <> bytes <> " megabytes."
    Ukrainian  -> "Кеш містить " <> ps <> " пакунків, які використовують " <> bytes <> " МБ місця."
    Romanian   -> "Cache-ul conține " <> ps <> " pachete, consumând " <> bytes <> " MB."
    Vietnamese -> "Có " <> ps <> " gói trong cache, chiếm " <> bytes <> " megabytes."
    _          -> "The cache contains " <> ps <> " packages, consuming " <> bytes <> " megabytes."

cleanCache_8 :: Word -> Language -> Doc AnsiStyle
cleanCache_8 (bt . tshow -> bytes) = \case
    Arabic    -> ".ميغابايت محررة " <> bytes
    Polish    -> bytes <> " megabajtów zwolnionych."
    Spanish   -> bytes <> " megabytes liberados."
    Ukrainian -> bytes <> " МБ звільнилось."
    Romanian  -> bytes <> " MB eliberat."
    Vietnamese -> "Giải phóng " <> bytes <> "megabytes."
    _         -> bytes <> " megabytes freed."

cleanCache_9 :: Word -> Language -> Doc AnsiStyle
cleanCache_9 (bt . tshow -> w) = \case
    Romanian -> w <> " versiuni din fiecare pachet instalat vor fi păstrate."
    Vietnamese -> "Sẽ giữ lại " <> w <> " phiên bản của các gói đã cài đặt."
    _        -> w <> " versions of each installed package will be kept."

-- NEEDS TRANSLATION
cleanNotSaved_1 :: Language -> Doc AnsiStyle
cleanNotSaved_1 = \case
    Japanese   -> "不要パッケージファイルを確認・・・"
    Arabic     -> "...تحديد ملفات الحزم غير الضرورية"
    Polish     -> "Określanie niepotrzebnych plików pakietów..."
    Croatian   -> "Pronalazim nepotrebne datoteke paketa..."
    German     -> "Bestimme nicht benötigte Paketdateien..."
    Spanish    -> "Determinando ficheros de paquetes innecesarios..."
    Norwegian  -> "Finner unødige pakkefiler..."
    Italian    -> "Inviduazione dei pacchetti non più necessari..."
    Portuguese -> "Determinando pacotes não necessários..."
    French     -> "Détermination des fichiers de paquet inutiles…"
    Russian    -> "Вычисляются ненужные файлы пакетов..."
    Indonesia  -> "Menentukan berkas paket yang tidak dibutuhkan..."
    Chinese    -> "正在确定不需要的包文件..."
    Swedish    -> "Beräknar onödiga paketfiler..."
    Esperanto  -> "Decidas nebezonajn dosierojn de pakaĵoj..."
    Dutch      -> "Overbodige pakketbestanden aan het vaststellen..."
    Ukrainian  -> "Визначачення непотрібних пакунків..."
    Romanian   -> "Se determin fișiere de pachet inutile..."
    Vietnamese -> "Xác định các tệp của gói không cần thiết..."
    _          -> "Determining unneeded package files..."

-- NEEDS TRANSLATION
cleanNotSaved_2 :: Int -> Language -> Doc AnsiStyle
cleanNotSaved_2 n@(cyan . pretty -> s) = \case
    Japanese   -> "「" <> s <> "」の不要パッケージファイルがあります。削除しますか？"
    Arabic     -> "تم العثور على ملفات الحزمة غير الضرورية.هل تريد حذفه؟ " <> s
    Polish     -> s <> " niepotrzebnych plików zostało znalezionych. Usunąć?"
    Croatian   -> s <> " nepotrebnih datoteka pronađeno. Obrisati?"
    German     -> s <> " nicht benötigte Paketdateien gefunden. Löschen?"
    Spanish    -> s <> " ficheros innecesarios de paquetes encontrados. ¿Deseas eliminarlos?"
    Norwegian  -> s <> " unødige pakkefiler funnet. Vil du slette?"
    Italian    -> "Sono stati trovati " <> s <> " file non necessari per i pacchetti. Cancellarli?"
    Portuguese -> s <> " pacotes não necessários encontrados. Removê-los?"
    French     -> s <> " paquets inutiles trouvés. Les supprimer ?"
    Russian    -> pluralRussian ("Обнаружен " <> s <> " ненужный файл пакета.") ("Обнаружены " <> s <> " ненужных файла пакетов.") ("Обнаружено " <> s <> " ненужных файлов пакетов.") n <> " Удалить?"
    Indonesia  -> s <> " berkas paket yang tidak dibutuhkan ditemukan. Hapus?"
    Chinese    -> "发现了 " <> s <> " 个不需要的包文件。是否删除？"
    Swedish    -> s <> " oanvända paket hittades. Ta bort?"
    Esperanto  -> s <> " nebezonajn dosierojn de pakaĵoj trovis. Ĉu forigi"
    Dutch      -> s <> " overbodige pakketbestanden gevonden. Verwijderen?"
    Ukrainian  -> "Знайдено " <> s <> " непотрібних пакунків. Видалити?"
    Romanian   -> "S-au găsit " <> s <> " fișiere de pachet inutile. Ștergeți?"
    Vietnamese -> "Tìm thấy " <> s <> " gói không cần thiết. Xóa bỏ?"
    _          -> s <> " unneeded package files found. Delete?"

----------------------------
-- Aura/Commands/L functions
----------------------------
logLookUpFields :: Language -> [Text]
logLookUpFields = sequence [ Fields.package
                           , Fields.firstInstall
                           , Fields.upgrades
                           , Fields.recentActions ]

reportNotInLog_1 :: Language -> Doc AnsiStyle
reportNotInLog_1 = \case
    Japanese   -> "logファイルには出ていない："
    Arabic     -> ":لم تظهر هذه في ملف السجل"
    Polish     -> "Tych pakietów nie ma w dzienniku:"
    Croatian   -> "Ovih paketa nema u dnevniku:"
    Swedish    -> "Dessa har inte framkommit i loggfiler:"
    German     -> "Diese sind nicht in der Logdatei aufgetaucht:"
    Spanish    -> "Estos no aparecen en el fichero log:"
    Portuguese -> "Os seguintes não apareceram no arquivo de log:"
    French     -> "Ceci n'apparaît pas des les journaux (log) :"
    Russian    -> "Следующих пакетов нет в лог-файле:"
    Italian    -> "Questi non sono apparsi nel file di log:"
    Serbian    -> "Ови пакети се не спомињу у дневнику:"
    Norwegian  -> "Følgende har ikke vist seg i loggen:"
    Indonesia  -> "Tidak terlihat pada berkas log:"
    Chinese    -> "这些没有在日志文件中出现："
    Esperanto  -> "Ĉi tiuj ne enestis la protokolajn dosierojn:"
    Dutch      -> "Deze zijn niet verschenen in het logbestand:"
    Ukrainian  -> "Наступних пакунків немає в лог файлі:"
    Romanian   -> "Acestea nu au apărut în log:"
    Vietnamese -> "Nội dung sau không có trong tệp log:"
    _          -> "These have not appeared in the log file:"

-------------------------------
-- Aura/AUR functions
-------------------------------

packageNotFound_1 :: Language -> Doc AnsiStyle
packageNotFound_1 = \case
  Romanian -> "Nu s-a găsit nici un pachet."
  Vietnamese -> "Không tím thấy gói."
  _        -> "No packages found."

-- https://github.com/fosskers/aura/issues/498
connectFailure_1 :: Language -> Doc AnsiStyle
connectFailure_1 = \case
  Polish    -> "Nie udało się nawiązać połączenia z AUR. Czy jesteś połączony z internetem?"
  Arabic    -> "هل انت متصل بالانترنت؟ .AURفشل الاتصال بـ"
  Spanish   -> "No se pudo contactar con el AUR. ¿Tienes conexión a internet?"
  Italian   -> "Non è stato possibile contattare l'AUR. Il computer è connesso ad internet?"
  Dutch     -> "Contact opnemen met de AUR mislukt. Heeft U een internet connectie?"
  Ukrainian -> "Не вдалося зв'язатись з AUR. У вас є підключення до інтернету?"
  Romanian  -> "Nu s-a putut contacta AUR. Sunteți conectat pe Internet?"
  Vietnamese -> "Mất kết nối tới AUR. Bạn có kết nối mạng không?"
  _         -> "Failed to contact the AUR. Do you have an internet connection?"

dependencyLookup_1 :: Text -> Language -> Doc AnsiStyle
dependencyLookup_1 t = \case
  Romanian  -> vsep ["A fost o problemă cu analiza recursivă de dependențe:", pretty t]
  Vietnamese -> vsep ["Có lỗi trong quá trình tìm kiếm gói phụ thuộc đệ quy:", pretty t]
  _         -> vsep ["There was an issue during recursive dependency lookup:", pretty t]

miscAURFailure_1 :: Language -> Doc AnsiStyle
miscAURFailure_1 = \case
  Polish    -> "Wystąpił nieznany błąd podczas próby łączenia z AUR."
  Arabic    -> ".بطريقة غير معروفة AURفشل الاتصال بـ"
  Spanish   -> "El contacto con el AUR falló de alguna manera desconocida."
  Italian   -> "C'è stato un errore sconosciuto nel contattare l'AUR."
  Dutch     -> "Contact opnemen met de AUR is op een onbekende manier mislukt."
  Ukrainian -> "Зв'язок з AUR було обірвано невідомим чином."
  Romanian  -> "Nu s-a putut contacta AUR dintr-un motiv necunoscut."
  Vietnamese -> "Bất ngờ không thể kết nối tới AUR."
  _         -> "Contacting the AUR failed in some unknown way."

miscAURFailure_3 :: Language -> Doc AnsiStyle
miscAURFailure_3 = \case
  Polish    -> "Plik JSON zwrócony z AUR nie mógł zostać rozszyfrowany."
  Arabic    -> ".AURالذي تم ارجاعه من اﻟ JSONفشل فك شفرة اﻟ"
  Spanish   -> "El JSON devuelto por el servidor AUR no se pudo decodificar."
  Ukrainian -> "JSON, який повернувся з сервера AUR, неможливо розшифрувати."
  Romanian  -> "JSON-ul întors de server-ul AUR nu putea fi decodat."
  Vietnamese -> "Không thể giải mã tệp JSON lấy từ máy chủ AUR."
  _         -> "The JSON returned from the AUR server could not be decoded."

infoFields :: Language -> [Text]
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
    Arabic     -> "!انتهت صلاحيته"
    Polish     -> "Nieaktualny!"
    Croatian   -> "Zastarjelo!"
    Swedish    -> "Utdaterad!"
    German     -> "Veraltet!"
    Spanish    -> "¡Desactualizado!"
    Portuguese -> "Desatualizado!"
    French     -> "Périmé !"
    Russian    -> "Устарел!"
    Italian    -> "Non aggiornato all'ultima versione!"
    Serbian    -> "Застарео!"
    Norwegian  -> "Utdatert!"
    Indonesia  -> "Ketinggalan Zaman!"
    Chinese    -> "过期！"
    Esperanto  -> "Neĝisdata!"
    Dutch      -> "Verouderd!"
    Ukrainian  -> "Застарів!"
    Romanian   -> "Neactualizat!"
    Vietnamese -> "Đã cũ!"
    _          -> "Out of Date!"
outOfDateMsg Nothing = green . \case
    Japanese   -> "最新"
    Arabic     -> "حتى الوقت الرهن"
    Polish     -> "Aktualny"
    Croatian   -> "Ažurirano"
    Swedish    -> "Aktuell"
    German     -> "Aktuell"
    Spanish    -> "Actualizado"
    Portuguese -> "Atualizado"
    French     -> "À jour"
    Russian    -> "Новейший"
    Italian    -> "Aggiornato all'ultima versione"
    Serbian    -> "Ажуран"
    Norwegian  -> "Oppdatert"
    Indonesia  -> "Mutakhir"
    Chinese    -> "最新"
    Esperanto  -> "Ĝisdata"
    Dutch      -> "Up-to-date"
    Ukrainian  -> "Найновіший"
    Romanian   -> "Actializat"
    Vietnamese -> "Mới nhất"
    _          -> "Up to Date"

-- NEEDS TRANSLATION
orphanedMsg :: Maybe Text -> Language -> Doc AnsiStyle
orphanedMsg (Just m) = const (pretty m)
orphanedMsg Nothing = red . \case
    Japanese   -> "孤児です!"
    Arabic     -> "!حزمة يتيمة"
    Polish     -> "Osierocony!"
    Croatian   -> "Nema roditelja!"
    German     -> "Verwaist!"
    Spanish    -> "¡Huérfano!"
    Norwegian  -> "Foreldreløs!"
    Portuguese -> "Órfão!"
    French     -> "Abandonné !"
    Russian    -> "Осиротевший!"
    Italian    -> "Orfano!"
    Indonesia  -> "Tak dipelihara!"
    Chinese    -> "孤包！"
    Swedish    -> "Föräldralös!"
    Esperanto  -> "Orfita!"
    Dutch      -> "Verweest!"
    Ukrainian  -> "Осиротів!"
    Romanian   -> "Orfan!"
    Vietnamese -> "Gói lẻ!"
    _          -> "Orphaned!"

-----------------------
-- Aura/State functions
-----------------------
-- NEEDS TRANSLATION
saveState_1 :: Language -> Doc AnsiStyle
saveState_1 = \case
    Japanese   -> "パッケージ状態の保存完了。"
    Arabic     -> ". حالة الحزمة محفوظة"
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
    Esperanto  -> "Konservita stato de pakaĵo."
    Dutch      -> "Pakketstatus opgeslagen."
    Ukrainian  -> "Стан пакунків збережено."
    Romanian   -> "Stare de pachete salvată."
    Vietnamese -> "Đã lưu trạng thái gói."
    _          -> "Saved package state."

-- NEEDS TRANSLATION
restoreState_1 :: Language -> Doc AnsiStyle
restoreState_1 = \case
    Japanese   -> "対象バージョンがないパッケージ："
    Arabic     -> ":اصدارات الرجوع المطلوبة غير متوفرة للحزمة التالية"
    Polish     -> "Starsze wersje nie są dostępne dla:"
    Croatian   -> "Tražene stare verzije nisu dostupne za:"
    German     -> "Gewünschte Downgrade-Versionen nicht verfügbar für:"
    Spanish    -> "Versiones anteriores no disponibles para:"
    Serbian    -> "Захтеване старе верзије нису доступне за:"
    Norwegian  -> "De spesifiserte nedgraderingsversjonene er ikke tilgjengelig for:"
    Italian    -> "Non sono disponibili versioni precedenti a cui tornare per:"
    Portuguese -> "Versões anteriores requisitadas não disponívels para:"
    French     -> "Version antérieure requise non disponible pour :"
    Russian    -> "Запрошенные версии для отката не доступны для:"
    Indonesia  -> "Versi yang diturunkan tidak tersedia untuk: "
    Chinese    -> "请求的降级版本对以下包不可用："
    Swedish    -> "Den begärda nedgraderingen finns inte tillgänglig för:"
    Esperanto  -> "Petitajn malpromociajn versiojn ne estas disponebla de:"
    Dutch      -> "Verzochtte downgrade versies niet beschikbaar voor:"
    Ukrainian  -> "Запитані версії для відкату не доступні для:"
    Romanian   -> "Versiunea solicitată pentru retrogradare nu este disponibilă pentru:"
    Vietnamese -> "Không thể hạ cấp cho:"
    _          -> "Requested downgrade versions not available for:"

restoreState_2 :: Language -> Doc AnsiStyle
restoreState_2 = \case
    Japanese   -> "保存されたパッケージ状態がない。作るには「-B」を。"
    Arabic     -> "(لحفظ الحالة الحالية -B استخدم) .عدم وجود حالة محفوظة للرجوع إليها"
    Polish     -> "Brak zapisanych stanów do przywrócenia. (Użyj -B by zapisać aktualny stan)"
    Spanish    -> "No hay estados guardados para ser restaurados. (Utilice -B para guardar el estado actual)"
    Portuguese -> "Nenhum estado disponível para ser recuperado. (Utilize -B para salvar o estado atual)"
    Russian    -> "Нет сохраненных состояний для восстановления. (Используйте -B для сохранения текущего состояния)"
    Italian    -> "Nessuno stato precedente a cui tornare. (Usa -B per salvare lo stato attuale)"
    Chinese    -> "没有要恢复的已保存状态。（使用 -B 保存当前状态）"
    Swedish    -> "Inga sparade tillstånd att återhämta. (Använd -B för att spara det nuvarande tillståndet)"
    Esperanto  -> "Ne konservitaj statoj restaŭros. (Uzu -B konservi la aktualan staton)"
    Dutch      -> "Er zijn geen opgeslagen statussen om te herstellen. (Gebruik -B om de huidige staat op te slaan)"
    Ukrainian  -> "Немає збережених станів для відновлення. (Викоривуйте -B для збереження теперішнього стану)"
    Romanian   -> "Nu există vreo stare de recuperat. (Folosiți -B pentru a salva starea actuală)"
    Vietnamese -> "Không có trạng thái nào có thể lưu. (Dùng -B để lưu trạng thái hiện tại)"
    _          -> "No saved states to be restored. (Use -B to save the current state)"

-- NEEDS TRANSLATION
reinstallAndRemove_1 :: Language -> Doc AnsiStyle
reinstallAndRemove_1 = \case
    Japanese   -> "パッケージを変更する必要はありません。"
    Arabic     -> ".لا يوجد حزمة تحتاج التغير"
    Polish     -> "Żaden pakiet nie wymaga zmian"
    Croatian   -> "Nema paketa kojima su potrebne izmjene."
    German     -> "Keine Pakete brauchen Änderungen."
    Spanish    -> "Ningún paquete necesita cambios."
    Serbian    -> "Ниједан пакет не захтева измене."
    Norwegian  -> "Ingen pakker trenger forandring."
    Italian    -> "Nessun pacchetto necessita di cambiamenti."
    Portuguese -> "Nenhum pacote requer alteração."
    French     -> "Aucun paquet n'a besoin de changement."
    Russian    -> "Пакеты не нуждаются в изменениях."
    Indonesia  -> "Tidak ada paket yang diubah."
    Chinese    -> "没有包需要改变。"
    Swedish    -> "Inga paket behöver ändras."
    Esperanto  -> "Ne pakaĵoj devas ŝanĝiĝi."
    Dutch      -> "Er zijn geen pakketten die wijzigingen nodig hebben."
    Ukrainian  -> "Пакунки не потребують оновлення."
    Romanian   -> "Nu trebuie schimbat nici un pachet."
    Vietnamese -> "Không có gói nào cần thay đổi."
    _          -> "No packages need changing."

--------------------------------------
-- Aura/Settings/BadPackages functions
--------------------------------------
whoIsBuildUser_1 :: Language -> Doc AnsiStyle
whoIsBuildUser_1 = \case
    Polish     -> "Nie można określić z którego konta użytkownika chcesz budować."
    Arabic     -> ".لا يمكن تحديد حساب المستخدم الذي سيتم البناء به"
    Spanish    -> "No se puede determinar el usuario que ejecutará la compilación."
    Portuguese -> "Não foi possível determinal o usuário que executará a compilação."
    Russian    -> "Не удается определить, от имени какого пользователя производить сборку."
    Italian    -> "Non è stato possibile determinare l'utente che eseguirà la compilazione."
    Esperanto  -> "Ne povas decidi, per kiu konto de uzanto munti."
    Dutch      -> "Kan niet bepalen met welk gebruikers account te bouwen."
    Ukrainian  -> "Не вдається визначити користувача, від імені якого буде проводитись збірка."
    Romanian   -> "Nu se poate determina cu care cont de utilizator să se compileze."
    Vietnamese -> "Không thể xác định tài khoản người dùng nào để build."
    _          -> "Can't determine which user account to build with."

------------------------
-- Aura/Pacman functions
------------------------
confParsing_1 :: Language -> Doc AnsiStyle
confParsing_1 = \case
    Polish     -> "Nie udało się odczytać twojego pliku pacman.conf"
    Arabic     -> ".الخاص بك pacman.confفشل تحليل ملف اﻟ"
    Spanish    -> "No fue posible analizar su archivo pacman.conf."
    Portuguese -> "Não foi possível interpretar o arquivo pacman.conf ."
    Russian    -> "Не удается распознать формат вашего файла pacman.conf."
    Italian    -> "Non è stato possibile analizzare il file pacman.conf."
    Esperanto  -> "Ne kapablas sintaske analizi vian dosieron, pacman.conf."
    Dutch      -> "Niet in staat om uw pacman.conf bestand te parseren."
    Ukrainian  -> "Не вдалось зрозуміти вміст файлу pacman.conf."
    Romanian   -> "Nu se poate analiza fișierul pacman.conf."
    Vietnamese -> "Không thể lấy dữ liệu từ tệp pacman.conf của bạn."
    _          -> "Unable to parse your pacman.conf file."

provides_1 :: PkgName -> Language -> Doc AnsiStyle
provides_1 (bt . pnName -> pro) = \case
    Polish     -> pro <+> "jest wymagany/a jako zależność, dostarczana przez wiele pakietów. Proszę wybrać jeden:"
    Arabic     -> ":مطلوب باعتباره تبعية ، والتي يتم توفيرها بواسطة حزم متعددة. رجاءا اختر واحدة" <+> pro
    Spanish    -> pro <+> "se requiere como una dependencia, que es proporcionada por múltiples paquetes. Por favor, seleccione uno:"
    Italian    -> pro <+> "è richiesto come dipendenza; si trova in molteplici pacchetti. Selezionarne uno:"
    Dutch      -> pro <+> "is vereist als afhankelijkheid, die wordt geleverd door meerdere pakketten. Selecteer er alstublieft een:"
    Ukrainian  -> pro <+> "потрібен як залежність, яка надається декількома пакунками. Оберіть один з них:"
    Romanian   -> pro <+> "este necesar ca dependență, care e provizionat de mai multe pachete. Selectați unul dintre ele:"
    Vietnamese -> pro <+> "là gói phụ thuộc, được cung cấp từ nhiều gói khác. Hãy chọn một:"
    _          -> pro <+> "is required as a dependency, which is provided by multiple packages. Please select one:"

----------------------------------
-- Aura/Pkgbuild/Editing functions
----------------------------------
hotEdit_1 :: PkgName -> Language -> Doc AnsiStyle
hotEdit_1 (bt . pnName -> p) = \case
    Japanese   -> p <> "のPKGBUILDを編成しますか？"
    Polish     -> "Czy chcesz edytować PKGBUILD " <> p <> "?"
    Arabic     -> "؟" <> p <> "التابع ﻟ PKGBUILDهل تريد ان تعدل اﻟ"
    Croatian   -> "Želite li izmjeniti PKGBUILD " <> p <> "?"
    Swedish    -> "Vill du ändra PKGBUILD-filen ifrån " <> p <> "?"
    German     -> "Möchten Sie die PKGBUILD-Datei für " <> p <> " bearbeiten?"
    Spanish    -> "¿Deseas editar el PKGBUILD de " <> p <> "?"
    Portuguese -> "Deseja editar o PKGBUILD de " <> p <> "?"
    French     -> "Voulez-vous éditer le PKGBUILD de " <> p <> " ?"
    Russian    -> "Отредактировать PKGBUILD пакета " <> p <> "?"
    Italian    -> "Modificare il PKGBUILD di " <> p <> "?"
    Serbian    -> "Желите ли да измените PKGBUILD за " <> p <> "?"
    Norwegian  -> "Vil du endre PKGBUILD for " <> p <> "?"
    Indonesia  -> "Apakah anda ingin menyunting PKGBUILD untuk paket " <> p <> "?"
    Chinese    -> "你希望编辑 " <> p <> " 的 PKGBUILD 文件吗？"
    Esperanto  -> "Ĉu vi volas redakti la PKGBUILD de " <> p <> "?"
    Dutch      -> "Wilt u het PKGBUILD-bestand van " <> p <> " bewerken?"
    Ukrainian  -> "Бажаєте відредагувати PKGBUILD для пакунку " <> p <> "?"
    Romanian   -> "Doriți să modificați PKGBUILD-ul pachetului " <> p <> "?"
    Vietnamese -> "Bạn có muốn chỉnh sửa PKGBUILD của " <> p <> "?"
    _          -> "Would you like to edit the PKGBUILD of " <> p <> "?"

hotEdit_2 :: Language -> Doc AnsiStyle
hotEdit_2 = \case
  Polish    -> "Czy chcesz edytować plik .install?"
  Arabic    -> "؟.installهل تريد تعديل ملف اﻟ"
  Spanish   -> "¿Desea editar el archivo .install?"
  Ukrainian -> "Бажаєте відредагувати файл .intall?"
  Romanian  -> "Doriți să modificați fișierul .install?"
  Vietnamese -> "Bạn có muốn chỉnh sửa tệp .install?"
  _         -> "Would you like to edit the .install file?"

hotEdit_3 :: FilePath -> Language -> Doc AnsiStyle
hotEdit_3 fp = \case
  Polish    -> "Czy chcesz edytować " <> pretty fp <> "?"
  Arabic    -> "؟" <> pretty fp <> " هل تريد التعديل"
  Spanish   -> "¿Desea editar " <> pretty fp <> "?"
  Ukrainian -> "Бажаєте відредагувати " <> pretty fp <> "?"
  Romanian  -> "Doriți să modificați " <> pretty fp <> "?"
  Vietnamese -> "Bạn có muốn chỉnh sửa " <> pretty fp <> "?"
  _         -> "Would you like to edit " <> pretty fp <> "?"

------------------------------
-- Pkgbuild Security functions
------------------------------
security_1 :: PkgName -> Language -> Doc AnsiStyle
security_1 (PkgName p) = \case
  Polish    -> "PKGBUILD dla" <+> bt p <+> "był zbyt zawiły do odczytania - może zawierać złośliwy kod."
  Arabic    -> ".كان معقدا جدا للتحليل - يمكن أن يكون تعتيم مشوش للشفرة" <+> bt p <+> "تبع PKGBUILDاﻟ"
  Spanish   -> "El PKGBUILD de" <+> bt p <+> "era demasiado complejo de analizar - puede estar ofuscando código malicioso."
  Italian   -> "Il PKGBUILD di" <+> bt p <+> "è troppo complesso per essere analizzato - è possibile che stia offuscando codice malevolo."
  Dutch     -> "Het PKGBUILD-bestand van" <+> bt p <+> " was te complex om te parseren - het kan schadelijke code versluieren."
  Ukrainian -> "PKGBUILD пакунку" <+> bt p <+> "був надто складним для аналізу - він може містити замаскований шкідливий код."
  Romanian  -> "PKGBUILD-ul pachetului" <+> bt p <+> "este prea complicat de analizat - ar putea sa acopere cod rău intenționat."
  Vietnamese -> "PKGBUILD của" <+> bt p <+> "quá khó để đọc - nó có thể chứa đoạn mã nguy hiểm."
  _ -> "The PKGBUILD of" <+> bt p <+> "was too complex to parse - it may be obfuscating malicious code."

security_2 :: Text -> Language -> Doc AnsiStyle
security_2 (bt -> t) = \case
  Polish    -> t <+> "może zostać użyty do pobrania arbitralnych skryptów, które nie są śledzone przez ten PKGBUILD."
  Arabic    -> ".هذه PKGBUILDيمكن ان يحمل ملفات عشروتىية ليست مسجلة باﻟ" <+> t
  Spanish   -> t <+> "se puede usar para descargar scripts arbitrarios que este PKGBUILD no rastrea."
  Italian   -> t <+> "può essere usato per scaricare script arbitrari non tracciati da questo PKGBUILD."
  Dutch     -> t <+> "kan gebruikt worden om willekeurige scripten te downloaden die niet worden bijgehouden door dit PKGBUILD-bestand."
  Ukrainian -> t <+> "може завантажувати довільні скріпти, які не відстежуються цим PKGBUILD."
  Romanian  -> t <+> "se poate folosi pentru a descărca scripturi neurmărite de acest PKGBUILD."
  Vietnamese -> t <+> "có thể dùng để tải xuống các tập lệnh sẽ không được kiểm soát bởi PKGBUILD."
  _ -> t <+> "can be used to download arbitrary scripts that aren't tracked by this PKGBUILD."

security_3 :: Text -> Language -> Doc AnsiStyle
security_3 (bt -> t) = \case
  Polish    -> t <+> "może zostać użyty do wykonywania arbitralnego kodu, który nie jest śledzony przez ten PKGBUILD."
  Arabic    -> ".هذه PKGBUILDيمكن ان يستعمل ملفات عشروتىية ليست مسجلة باﻟ" <+> t
  Spanish   -> t <+> "se puede usar para ejecutar código arbitrario que este PKGBUILD no rastrea."
  Italian   -> t <+> "può essere usato per eseguire codice arbitrario non tracciato da questo PKGBUILD."
  Dutch     -> t <+> "kan gebruikt worden om willekeurige code uit te voeren die niet worden bijgehouden door dit PKGBUILD-bestand."
  Ukrainian -> t <+> "може виконувати довільний код, який не відстежуються цим PKGBUILD."
  Romanian  -> t <+> "se poate folosi pentru a executa cod arbitrar neurmărit de acest PKGBUILD."
  Vietnamese -> t <+> "có thể dùng để chạy các đoạn mã không được kiểm soát bởi PKGBUILD. "
  _ -> t <+> "can be used to execute arbitrary code not tracked by this PKGBUILD."

security_4 :: Text -> Language -> Doc AnsiStyle
security_4 (bt -> t) = \case
  Polish    -> t <+> "wskazuje na to, że ktoś może próbować uzyskać dostęp root'a do twojej maszyny."
  Arabic    -> ".تشير ان شخصا ما يحاول الوصول الى قوت المسؤول على جهازك" <+> t
  Spanish   -> t <+> "indica que alguien puede estar intentando obtener acceso de root a su máquina."
  Italian   -> t <+> "indica che forse qualcuno sta cercando di ottenere accesso alla tua macchina come root."
  Dutch     -> t <+> "geeft aan dat iemand mogelijk root-toegang to uw machine probeert te krijgen."
  Ukrainian -> t <+> "вказує на те, що хтось може спробувати отримати доступ root до вашої машини."
  Romanian  -> t <+> "indică că cineva are putea încerca să obțină acces root asupra mașinăria dumneavoastră."
  Vietnamese -> t <+> "được xác định là có người đang có giành quyền truy cập vào root trên thiết bị của bạn."
  _ -> t <+> "indicates that someone may be trying to gain root access to your machine."

security_5 :: PkgName -> Language -> Doc AnsiStyle
security_5 (PkgName p) = \case
  Polish    -> "UWAGA: PKGBUILD dla " <+> bt p <+> "zawiera wyrażenia bash znajdujące się na czarnej liście."
  Arabic    -> ".في القائمة السودء bash في تعبيرات" <+> bt p <+> "باجل PKGBUILDتحذير: اﻟ"
  Spanish   -> "ADVERTENCIA: El PKGBUILD de" <+> bt p <+> "contiene expresiones bash en la lista negra."
  Italian   -> "ATTENZIONE: Il PKGBUILD di" <+> bt p <+> "contiene espressioni bash presenti nella lista nera."
  Dutch     -> "WAARSCHUWING: De PKGBUILD van" <+> bt p <+> "bevat bash uitdrukkingen die op de zwarte lijst staan."
  Ukrainian -> "УВАГА! PKGBUILD пакунку" <+> bt p <+> "містить вирази bash, які занесені в чорний список."
  Romanian  -> "ATENȚIE! PKGBUILD-ul pachetului" <+> bt p <+> "conține expresii de bash pe lista neagră."
  Vietnamese -> "CẢNH BÁO: PKGBUILD của" <+> bt p <+> "chứa những câu lệnh bash nguy hiểm."
  _ -> "WARNING: The PKGBUILD of" <+> bt p <+> "contains blacklisted bash expressions."

security_6 :: Language -> Doc AnsiStyle
security_6 = \case
  Polish    -> "Czy chcesz zakończyć proces budowania?"
  Arabic    -> "هل تريد اقاف البناء؟"
  Spanish   -> "¿Desea salir del proceso de compilación?"
  Italian   -> "Terminare la compilazione?"
  Dutch     -> "Wilt u het bouw process stoppen?"
  Ukrainian -> "Бажаєте скасувати процес збірки?"
  Romanian  -> "Doriți anula procesul de compilare?"
  Vietnamese -> "Bạn có muốn dừng quá trình build?"
  _         -> "Do you wish to quit the build process?"

security_7 :: Language -> Doc AnsiStyle
security_7 = \case
  Polish    -> "Anulowano dalsze przetwarzanie by uniknąć egzekucji potencjalnie złośliwego kodu bash"
  Arabic    -> ".الذي يحتمل أن يكون ضارا bash تم الغاء المعالجة الاضافيه لتجنب صدور"
  Spanish   -> "Se canceló el procesamiento posterior para evitar el código bash potencialmente malicioso."
  Italian   -> "Non saranno eseguite altre operazioni al fine di evitare l'esecuzione di codice bash potenzialmente malevolo."
  Dutch     -> "Verdere verwerking geannuleerd om het uitvoeren van potentieel schadelijke bash-code te voorkomen."
  Ukrainian -> "Подальша установка скасована, щоб уникнути потенційно шкідливого коду bash."
  Romanian  -> "S-a cancelat procesarea ulterioară pentru a evita cod de bash potențial rău intenționat."
  Vietnamese -> "Hãy dừng những quá trình tiếp theo để ngắn đoạn mã bash nguy hiểm."
  _ -> "Cancelled further processing to avoid potentially malicious bash code."

security_8 :: Text -> Language -> Doc AnsiStyle
security_8 (bt -> t) = \case
  Polish    -> t <+> "jest komendą bash zawartą w polach tablicy twojego PKGBUILD."
  Arabic    -> ".الخاص بك PKGBUILDمضمن بالحقول المصفوفة باﻟ bash امر" <+> t
  Spanish   -> t <+> "es un comando bash integrado en los campos de la matriz del PKGBUILD."
  Italian   -> t <+> "è un comando bash presente all'interno degli array del tuo PKGBUILD."
  Dutch     -> t <+> "is een bash-opdracht die is opgenomen in uw PKGBUILD-arrayvelden."
  Ukrainian -> t <+> "- це команда bash, що вбудована в ваші поля масиву PKGBUILD"
  Romanian  -> t <+> "este o comandă bash integrată în matricele din PKGBUILD."
  Vietnamese -> t <+> "là lệnh bash được lồng trong mảng của PKGBUILD."
  _ -> t <+> "is a bash command inlined in your PKGBUILD array fields."

security_9 :: Text -> Language -> Doc AnsiStyle
security_9 (bt -> t) = \case
  Polish    -> t <+> "jest dziwną rzeczą w polach tablicy. Czy to bezpieczne?"
  Arabic    -> "شيء غريب ان يكون لديك في الحقول المصفوفة, هل هيا امن؟" <+> t
  Spanish   -> t <+> "es algo extraño para tener en sus campos de matriz. ¿Es seguro?"
  Italian   -> t <+> "è una cosa strana da trovare all'interno degli array. E' sicura?"
  Dutch     -> t <+> "is een vreemd ding om in uw arrayvelden te hebben. Is het veilig?"
  Ukrainian -> t <+> "- дивна річ в полях масиву. Це безпечно?"
  Romanian  -> t <+> "e ciudat să se afle în matricele dumneavoastră. Asta este sigur?"
  Vietnamese -> t <+> "là đoạn mã lạ trong mảng. Nó có an toàn không?"
  _ -> t <+> "is a strange thing to have in your array fields. Is it safe?"

security_10 :: Text -> Language -> Doc AnsiStyle
security_10 (bt -> t) = \case
  Polish    -> t <+> "sugeruje, że ktoś próbował być sprytny używając zmiennych do ukrycia złośliwych komend."
  Arabic    -> ".يعني ان شخصا ما كان يحاول ان يكون ذكيا مع المتغيرات لاخفاء الاوامر الخبيثة" <+> t
  Spanish   -> t <+> "implica que alguien estaba tratando de ser astuto con las variables para ocultar comandos maliciosos."
  Italian   -> t <+> "implica che qualcuno stava trafficando con le variabili per nascondere comandi malevoli."
  Dutch     -> t <+> "impliceert dat iemand slim probeerde om te gaan met variabelen om schadelijke opdrachten te verbergen."
  Ukrainian -> t <+> "означає, що хтось намагається обдурити змінними, щоб сховати небеспечні команди."
  Romanian  -> t <+> "implică că cineva încearcă să fie șmecher cu variabile pentru a ascunde comenzi rele intenționate."
  Vietnamese -> t <+> "được xác định là có ai đó đang cố ẩn những câu lệnh nguy hiểm trong các biến."
  _ -> t <+> "implies that someone was trying to be clever with variables to hide malicious commands."

security_11 :: Language -> Doc AnsiStyle
security_11 = \case
  Polish    -> "Ten PKGBUILD jest zbyt zawiły do odczytania - może ukrywać w sobie złośliwy kod."
  Arabic    -> ".كان معقدا جدا للتحليل - يمكن ان يخفي برنامج ضار PKGBUILDذلك اﻟ"
  Spanish   -> "Éste PKGBUILD es demasiado complejo para analizar, puede estar ofuscando código malicioso."
  Ukrainian -> "Цей PKGBUILD був надто складним для аналізу - він може містити шкідливий код."
  Romanian  -> "Acel PKGBUILD este prea complicat de analizat - are putea ascunde cod rău intenționat."
  Vietnamese -> "Không thể đọc PKGBUILD - nó có thể chứa đoạn mã nguy hiểm."
  _         -> "That PKGBUILD is too complex to parse - it may be obfuscating malicious code."

security_12 :: Language -> Doc AnsiStyle
security_12 = \case
  Polish    -> "Potencjalne luki w bezpieczeństwie wykryte w PKGBUILD"
  Arabic    -> ".PKGBUILDاحتمال وجود ثغرات امنية في اﻟ"
  Spanish   -> "Posibles vulnerabilidades de PKGBUILD detectadas."
  Ukrainian -> "Потенційні вразливості знайдено в PKGBUILD."
  Romanian  -> "Vulnerabilități potențiale detectate în PKGBUILD."
  Vietnamese -> "Phát hiện lỗ hổng trong PKGBUILD."
  _         -> "Potential PKGBUILD vulnerabilities detected."

security_13 :: Word -> Language -> Doc AnsiStyle
security_13 (bt -> w) = \case
  Polish    -> "Sprawdzanie PKGBUILD" <+> w <+> "w poszukiwaniu luk w bezpieczeństwie..."
  Arabic    -> "...بحثا عن نقاط ضعف" <+> w <+> "تبع PKGBUILDتحقق اﻟ"
  Spanish   -> "Comprobando" <+> w <+> "PKGBUILDs por vulnerabilidades..."
  Ukrainian -> "Перевіряємо" <+> w <+> "PKGBUILD-ів на вразливості..."
  Romanian  -> "Se verifică PKGBUILD-uri" <+> w <+> "pentru vulnerabilități..."
  Vietnamese -> "Tìm kiếm" <+> w <+> "lỗ hổng trong PKGBUILD..."
  _         -> "Checking" <+> w <+> "PKGBUILDs for vulnerabilities..."

security_14 :: Language -> Doc AnsiStyle
security_14 = \case
  Polish    -> "Nie wykryto żadnych luk w bezpieczeństwie."
  Arabic    -> ".لا تم العثور على نقاط ضعف"
  Spanish   -> "No se detectaron vulnerabilidades."
  Ukrainian -> "Ніяких вразливостей не було знайдено."
  Romanian  -> "Nu s-a găsit nici o vulnerabilitate."
  Vietnamese -> "Không tìm thấy lỗ hổng."
  _         -> "No vulnerabilities detected."

-----------------------
-- Aura/Utils functions
-----------------------
yesNoMessage :: Language -> Doc ann
yesNoMessage = \case
    Polish     -> "[T/n]"
    Arabic     -> "[ن/لا]"
    Turkish    -> "[e/h]"
    Croatian   -> "[D/n]"
    German     -> "[J/n]"
    Spanish    -> "[S/n]"
    Norwegian  -> "[J/n]"
    Italian    -> "[S/n]"
    Portuguese -> "[S/n]"
    French     -> "[O/n]"
    Russian    -> "[Д/н]"
    Esperanto  -> "[J/n]"
    Dutch      -> "[J/n]"
    Ukrainian  -> "[Т/н]"
    Romanian   -> "[D/n]"
    _          -> "[Y/n]"

yesPattern :: Language -> [T.Text]
yesPattern lang = map T.toCaseFold $ case lang of
    Polish     -> ["t", "tak"]
    Arabic     -> ["ن", "نعم"]
    Turkish    -> ["e", "evet"]
    Croatian   -> ["d", "da"]
    German     -> ["j", "ja"]
    Spanish    -> ["s", "si"]
    Norwegian  -> ["j", "ja"]
    Italian    -> ["s", "si"]
    Portuguese -> ["s", "sim"]
    French     -> ["o", "oui"]
    Russian    -> ["д", "да"]
    Esperanto  -> ["j", "jes"]
    Dutch      -> ["j", "ja"]
    Ukrainian  -> ["т", "так"]
    Romanian   -> ["d", "da"]
    _          -> ["y", "yes"]

----------------------
-- Pluralization rules
----------------------
pluralRussian :: Integral n => a -> a -> a -> n -> a
pluralRussian singular plural1 plural2 n | n % 10 == 1 && n % 100 /= 11 = singular
                                         | n % 10 `elem` [2, 3, 4] = plural1
                                         | otherwise = plural2
