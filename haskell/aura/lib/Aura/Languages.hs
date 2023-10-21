{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
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
    , (Chinese,    "Kai Zhang / Alex3236")
    , (Japanese,   "Onoue Takuro / Colin Woodbury")
    , (Esperanto,  "Zachary Matthews")
    , (Dutch,      "Joris Blanken / Heimen Stoffels")
    , (Turkish,    "Cihan Alkan")
    , (Arabic,     "\"Array in a Matrix\"")
    , (Ukrainian,  "Andriy Cherniy")
    , (Romanian,   "90 / benone")
    , (Vietnamese, "\"Kritiqual\"")
    , (Czech,      "Daniel Rosel")
    , (Korean,     "\"Nioden\"")
    , (Hindi,      "@yozachar")
    ]

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
    Dutch      -> "Aura-vertalers:"
    Ukrainian  -> "Перекладачі Aura:"
    Romanian   -> "Traducători Aura:"
    Vietnamese -> "Dịch giả của Aura:"
    Czech      -> "Překladači Aury:"
    Korean     -> "Aura 번역자:"
    Hindi      -> "Aura अनुवादकों:"
    _          -> "Aura Translators:"

translatorMsg :: Language -> [Text]
translatorMsg lang = title : names
  where
    title :: Text
    title = translatorMsgTitle lang

    names :: [Text]
    names = mapMaybe (\l -> formatLang . (,l) <$> M.lookup l translators) [English ..]

    formatLang :: (Text, Language) -> Text
    formatLang (translator, lang') = " (" <> T.pack (show lang') <> ") " <> translator

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
  "cs" -> Just Czech
  "ko" -> Just Korean
  "hi" -> Just Hindi
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
    Dutch      -> "De pakketdatabank is vergrendeld. Druk op enter zodra de databank ontgrendeld is."
    Ukrainian  -> "База даних пакетів заблокована. Натисніть Enter, коли вона розблокується, щоб продовжити."
    Romanian   -> "Baza de date de pachete este blocată. Apăsați Enter după ce s-a deblocat pentru a continua."
    Vietnamese -> "Cơ sở dữ liệu của gói đã bị khóa. Nhấn Enter sau khi nó được mở khóa để tiếp tục."
    Czech      -> "Databáze balíčků je uzamčena. Až bude odemčena, stiskněte Enter pro pokračování."
    Korean     -> "패키지 데이터베이스가 잠겨있습니다. 계속하려면 Enter 키를 누르시오."
    Hindi      -> "पैकेज डेटाबेस लॉक है. जारी रखने के लिए अनलॉक होने पर एंटर दबाएँ।"
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
    Dutch      -> "Sinds makepkg v4.2 is het niet meer mogelijk om als root te bouwen."
    Ukrainian  -> "З версії makepkg v4.2 збірка від імені root неможлива."
    Romanian   -> "De la versiunea makepkg v4.2 încolo, compilarea ca root nu mai este posibilă."
    Vietnamese -> "Kể từ makepkg v4.2, build bằng quyền root không còn khả dụng."
    Czech      -> "Od makepkg v4.2 již není sestavení jako root možné."
    Korean     -> "makepkg v4.2부터는 루트 권한으로 빌드 할 수 없습니다."
    Hindi      -> "मेकपीकेजी v4.2 के अनुसार, रूट के रूप में निर्माण अब संभव नहीं है।"
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
    Chinese    -> "需要" <> sudo <> "以执行此操作。"
    Esperanto  -> "Vi ne povas fari ĉi tiun operacion, sen " <> sudo <> "."
    Dutch      -> "U kunt deze actie niet uitvoeren zonder " <> sudo <> " te gebruiken."
    Ukrainian  -> "Для цієї дії, потрібно використати " <> sudo <> "."
    Romanian   -> "Nu se poate folosi această operație asta fără " <> sudo <> "."
    Vietnamese -> "Bạn không thể thực hiện hành động này nếu không dùng " <> sudo <> "."
    Czech      -> "Tuto operaci nelze provést bez použití sudo."
    Korean     -> "루트 권한으로 실행해야 합니다."
    Hindi      -> "आप सूडो का उपयोग किए बिना यह ऑपरेशन नहीं कर सकते।"
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
    Dutch      -> "Bezig met bouwen van " <> p <> "…"
    Ukrainian  -> "Збираємо " <> p <> "..."
    Romanian   -> "Se compilează " <> p <> "..."
    Vietnamese -> "Đang build " <> p <> "..."
    Czech      -> "Kompilace " <> p <> "..."
    Korean     -> p <> "빌드 중..."
    Hindi      -> "बिल्डिंग " <> p <> "..."
    _          -> "Building " <> p <> "..."

buildPackages_2 :: Language -> Doc AnsiStyle
buildPackages_2 = \case
    Arabic     -> ".لن يتم بناء اي رزمة .'--allsource' كشف"
    Polish     -> "'--allsource' wykryte. Nie zostaną zbudowane żadne pakiety."
    Turkish    -> "'--allsource' bulundu. Yüklenebilir gerçek paketler oluşturulmayacaktır."
    Spanish    -> "'--allsource' detectado. No se construirán paquetes instalables reales."
    Romanian   -> "'--allsource' detectat. Nu se va compila oricare pachet instalabil."
    Vietnamese -> "'--allsource' được sử dụng. Không có gói nào sẽ được build."
    Czech      -> "Bylo nalezeno '-allsource'. Žádné skutečné instalovatelné balíčky nebudou sestaveny."
    Korean     -> "'--allsource' 감지되었습니다. 실제 설치 가능한 패키지는 빌드되지 않습니다."
    Dutch      -> "--allsource gedetecteerd. Er worden geen installeerbare pakketten gebouwd."
    Hindi      -> "'--allsource' का पता चला। कोई वास्तविक इंस्टाल करने योग्य पैकेज नहीं बनाया जाएगा।"
    Chinese    -> "检测到 '--allsource'。不会构建实际的可安装软件包。"
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
    Czech      -> "Všechny soubory .src.tar.gz byly vytvořeny a zkopírovány do: " <> pretty fp
    Dutch      -> "Alle .src.tar.gz-bestanden zijn gebouwd en gekopieerd naar " <> pretty fp
    Korean     -> "모든 .src.tar.gz 파일은 빌드되고 복사됩니다:" <> pretty fp
    Hindi      -> "सभी .src.tar.gz फ़ाइलें बनाई गईं और यहां कॉपी की गईं: " <> pretty fp
    Chinese    -> "所有 .src.tar.gz 文件已经构建并复制到：" <> pretty fp
    _          -> "All .src.tar.gz files were built and copied to: " <> pretty fp

buildPackages_4 :: Language -> Doc AnsiStyle
buildPackages_4 = \case
    Romanian   -> bt @Text "--hotedit" <+> "detectat, dar acestea au date în cache și vor fi omise din editare:"
    Vietnamese -> bt @Text "--hotedit" <+> "được sử dụng, những gói sau có trong cache và sẽ được bỏ qua để chỉnh sửa:"
    Czech      -> bt @Text "--hotedit" <+> "zjištěno, ale následující položky mají položky mezipaměti a budou přeskočeny pro úpravy:"
    Korean     -> bt @Text "--hotedit" <+> "감지되었지만 캐시 목록이 있으므로 편집을 위해 건너뜁니다."
    Dutch      -> "--hotedit gedetecteerd. De volgende pakketten zijn gecachet en zullen niet worden bewerkt:"
    Hindi      -> bt @Text "--hotedit" <+> "पता चला, लेकिन निम्नलिखित में कैश प्रविष्टियाँ हैं और इन्हें संपादन के लिए छोड़ दिया जाएगा:"
    Chinese    -> "检测到" <+> bt @Text "--hotedit" <+> "，但以下条目已缓存，将跳过编辑："
    _          -> bt @Text "--hotedit" <+> "detected, but the following have cache entries and will be skipped for editing:"

buildPackages_5 :: Language -> Doc AnsiStyle
buildPackages_5 = \case
    Romanian   -> "Se poate folosi" <+> bt @Text "--force" <+> "pentru a trece peste acest comportament."
    Vietnamese -> "Bạn có thể dùng" <+> bt @Text "--force" <+> "để ghi đè hành động này."
    Czech      -> "Můžete použít" <+> bt @Text "--force" <+> "k potlačení tohoto chování."
    Korean     -> bt @Text "--force" <+> "를 사용하여 이 동작을 무시할 수 있습니다."
    Dutch      -> "U kunt" <+> bt @Text "--force" <+> "toekennen om dit gedrag te omzeilen."
    Hindi      -> "आप इस व्यवहार को ओवरराइड करने के लिए" <+> bt @Text "--force" <+> "का उपयोग कर सकते हैं।"
    Chinese    -> "使用" <+> bt @Text "--force" <+> "可忽略此行为。"
    _          -> "You can use" <+> bt @Text "--force" <+> "to override this behaviour."

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
    Dutch      -> "Het bouwen is mislukt."
    Ukrainian  -> "Збірка не вдалась."
    Romanian   -> "Compilare nereușită."
    Vietnamese -> "Build thất bại."
    Czech      -> "Budování se nezdařilo."
    Korean     -> "빌드 실패"
    Hindi      -> "बिल्डिंग फेल हो गई।"
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
    Chinese    -> "仍要继续吗？"
    Swedish    -> "Vill du fortsätta ändå?"
    Esperanto  -> "Ĉu vi volas daŭrigi?"
    Dutch      -> "Wilt u toch doorgaan?"
    Ukrainian  -> "Ви все одно бажаєте продовжити?"
    Romanian   -> "Doriți oricum să continuați?"
    Vietnamese -> "Bạn có muốn tiếp tục không?"
    Czech      -> "Chcete přesto pokračovat?"
    Korean     -> "계속하시겠습니까?"
    Hindi      -> "क्या आप फिर भी जारी रखना चाहेंगे?"
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
    Chinese    -> "无法获取 " <> p <> " 的构建脚本。"
    Swedish    -> "Kunde inte hämta byggskript för " <> p <> "."
    Esperanto  -> "Paneis akiri muntaj skriptoj de " <> p <> "."
    Dutch      -> "De bouwscripts van " <> p <> " kunnen niet worden opgehaald."
    Ukrainian  -> "Не вдалось отримати сценарії збірки для " <> p <> "."
    Romanian   -> "Nu s-au putut obține scripturi de compilare pentru " <> p <> "."
    Vietnamese -> "Không thể lấy tập lệnh build cho " <> p <> "."
    Czech      -> "Nepodařilo se získat sestavení skriptů pro " <> p <> "."
    Korean     -> p <> "의 빌드 스크립트를 가져올 수 없습니다."
    Hindi      -> "" <> p <> "के लिए बिल्ड स्क्रिप्ट प्राप्त करने में विफल।"
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
    Czech      -> "Došlo k chybě makepkg."
    Korean     -> "makepkg를 실패했습니다."
    Hindi      -> "मेकपेकेजी विफलता थी।"
    Chinese    -> "makepkg 失败。"
    _          -> "There was a makepkg failure."

buildFail_9 :: Language -> Doc AnsiStyle
buildFail_9 = \case
  Polish     -> "Nie udało się zlokalizować żadnych zbudowanych pakietów (*.pkg.tar.xz)."
  Arabic     -> ".(*.pkg.tar.xz) فشل في اكتشاف اي ملف من ملفات البناء"
  Spanish    -> "Error al detectar todos los archivo de paquete (*.pkg.tar.xz)."
  Italian    -> "Non è stato possibile trovare nessun archivio risultante dalla compilazione del pacchetto (*.pkg.tar.xz)."
  Esperanto  -> "Paneis detekti ĉiujn dosierojn de pakaĵoj (*.pkg.tar.xz)."
  Dutch      -> "Er zijn geen gebouwde pakketbestanden aangetroffen (*.pkg.tar.xz)."
  Ukrainian  -> "Не вдалось знайти жодного файлу пакунку (*.pkg.tar.xz)."
  Romanian   -> "Nu s-a detectat nici un pachet construit (*.pkg.tar.xz)."
  Vietnamese -> "Không thể phát hiện các tệp đã được build (*.pkg.tar.xz)."
  Czech      -> "Nepodařilo se detekovat žádné soubory zabudovaného balíčku (*.pkg.tar.xz)."
  Korean     -> "빌드된 패키지 파일 (*.pkg.tar.xz)을 검색하지 못했습니다."
  Hindi      -> "किसी भी निर्मित पैकेज फ़ाइल (*.pkg.tar.xz) का पता लगाने में विफल।"
  Chinese    -> "未能发现任何已构建的包 (*.pkg.tar.xz)。"
  _          -> "Failed to detect any built package files (*.pkg.tar.xz)."

buildFail_10 :: Language -> Doc AnsiStyle
buildFail_10 = \case
  Polish     -> "Nie udało się zbudować żadnego pakietu."
  Arabic     -> ".فشل بناء كل الرزم"
  Spanish    -> "Los paquetes no se pudieron construir."
  Italian    -> "Non è stato possibile compilare i pacchetti."
  Esperanto  -> "Ĉiuj pakaĵoj paneis munti."
  Dutch      -> "Het bouwen van alle pakketten is mislukt."
  Ukrainian  -> "Жоден пакунок не вдалося зібрати."
  Romanian   -> "Nu s-a putut compila nici un pachet."
  Vietnamese -> "Tất cả các gói build thất bại."
  Czech      -> "Sestavení každého balíčku se nezdařilo."
  Korean     -> "모든 패키지를 빌드하지 못했습니다."
  Hindi      -> "हर पैकेज बनाने में असफल रहा।"
  Chinese    -> "所有包都未能构建。"
  _          -> "Every package failed to build."

buildFail_11 :: Language -> Doc AnsiStyle
buildFail_11 = \case
  Japanese   -> "作成は失敗しました。エラーを見ますか？"
  Arabic     -> "فشل البناء. هل ترغب في رؤية الخطأ؟"
  Polish     -> "Budowa zakończona niepowodzeniem. Czy chcesz zobaczyć błąd?"
  Spanish    -> "Construcción fallida. ¿Te gustaría ver el error?"
  Italian    -> "La compilazione è fallita. Visionare l'errore?"
  Esperanto  -> "Muntado paneis. Ĉu vi volas vidi la eraron?"
  Dutch      -> "Het bouwen is mislukt. Wilt u de foutmeldingen bekijken?"
  Ukrainian  -> "Збірка не вдалась. Бажаєте побачити помилку?"
  Romanian   -> "Compilare nereușită. Doriți să vedeți eroarea?"
  Vietnamese -> "Build thất bại. Bạn có muốn xem lịch sử lỗi?"
  Czech      -> "Budování se nezdařilo. Chcete vidět chybu?"
  Korean     -> "빌드를 실패했습니다. 오류를 확인하시겠습니까?"
  Hindi      -> "बिल्डिंग विफल हो गई। क्या आप त्रुटि देखना चाहेंगे?"
  Chinese    -> "构建失败。要检查错误吗？"
  _          -> "Building failed. Would you like to see the error?"

buildFail_12 :: Language -> Doc AnsiStyle
buildFail_12 = \case
    Polish     -> "Błąd podczas pobierania najnowszych aktualizacji poprzez 'git pull'."
    Arabic     -> ".على اخر تحديث 'git pull' فشل حصول"
    Spanish    -> "Error al 'git pull' las últimas actualizaciones."
    Ukrainian  -> "Не вдалося використати 'git pull' для отримання останніх оновлень."
    Romanian   -> "Nu a reușit 'git pull' să descarce cele mai recente actualizări."
    Vietnamese -> "Thất bại trong việc 'git pull' để cập nhật."
    Czech      -> "Nepodařilo se 'git stáhnout' nejnovější aktualizace."
    Korean     -> "최신 버전 'git pull'을 실패했습니다."
    Dutch      -> "Het uitvoeren van ‘git pull’ om de nieuwste bestanden op te halen is mislukt."
    Hindi      -> "नवीनतम अपडेट को 'गिट पुल' करने में विफल।"
    Chinese    -> "未能从 'git pull' 获取最新更新。"
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
    Chinese    ->  p <> " 依赖于版本 " <> d <> "，但最新版本是 " <> r <> "。"
    Esperanto  -> "La pakaĵo, " <> prnt <> ", dependas de versio " <> d <> " de " <> p <> ", sed la plej nova versio estas " <> r <> "."
    Dutch      -> prnt <> " is afhankelijk van versie " <> d <> " van " <> p <> ", maar de nieuwste versie is " <> r <> "."
    Ukrainian  -> "Залежність " <> p <> " потребує версію " <> d <> ", проте останньою версією є " <> r <> "."
    Romanian   -> "Pachetul " <> prnt <> " depinde de versiunea " <> d <> " al pachetului " <> p <> ", dar cea mai recentă versiune este " <> r <> "."
    Vietnamese -> "Gói " <> prnt <> " phụ thuộc vào bản " <> d <> " của " <> p <> ", nhưng bản mới nhất là " <> r <> "."
    Czech      -> "Balík " <> prnt <> " závisí na verzi " <> d <> " z " <> p <> ", ale nejnovější verze je " <> r <> "."
    Korean     -> prnt <> " 패키지는 " <> p <> "의 버전 " <> d <> "에 의존하지만 가장 최신 버전은 " <> r <> "입니다."
    Hindi      -> "पैकेज " <> prnt <> " " <> p <> " के संस्करण " <> d <> " पर निर्भर करता है, लेकिन सबसे हालिया संस्करण " <> r <> " है।"
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
  Chinese    -> p <> " 是一个被忽略的包！请检查 `pacman.conf` 文件。"
  Esperanto  -> p <> " estas malatenta pakaĵo! Vidu vian `pacman.conf` dosieron."
  Dutch      -> p <> " wordt genegeerd! Bekijk uw `pacman.conf`-bestand."
  Ukrainian  -> "Пакунок " <> p <> " буде проігноровано! Перевірте ваш файл `pacman.conf`."
  Romanian   -> "Pachetul " <> p <> " este ignorat! Verificați fișierul `pacman.conf`."
  Vietnamese -> "Gói " <> p <> "đã bị bỏ qua! Hãy xem trong `pacman.conf` của bạn."
  Czech      -> p <> " je ignorovaný balíček! Podívejte se na svůj soubor `pacman.conf`."
  Korean     -> p <> "은(는) 무시된 패키지입니다! `pacman.conf`를 확인하시오."
  Hindi      -> p <> " एक उपेक्षित पैकेज है! अपनी `pacman.conf` फ़ाइल देखें।"
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
  Dutch      -> "De afhankelijkheid " <> bt s <> " van " <> bt par <> " kan niet worden gevonden."
  Ukrainian  -> "Залежність " <> bt s <> " не було знайдено."
  Vietnamese -> "Không thể tìm thấy các gói phụ thuộc của " <> bt s <> "."
  Czech      -> "Nebyla nalezena závislost " <> bt s <> " z " <> bt par <> "."
  Korean     -> bt par <> "의 종속성 " <> bt s <> "을(를) 찾을 수 없습니다."
  Hindi      -> "निर्भरता " <> bt s <> " की " <> bt par <> " नहीं मिल सकी।"
  Chinese    -> "未能找到 " <> bt par <> " 的依赖 " <> bt s <> "。"
  _          -> "The dependency " <> bt s <> " of " <> bt par <> " couldn't be found."

depError l (BrokenProvides (PkgName pkg) (Provides (PkgName pro)) (PkgName n)) = case l of
  Arabic     -> "." <> bt pro <> " اللتي تقدم ," <> bt n <> " تحتاج" <> bt pkg <> " الرزمة"
  Spanish    -> "El paquete " <> bt pkg <> " necesita " <> bt n <> " que proporciona " <> bt pro <> "."
  Russian    -> "Пакету " <> bt pkg <> " требуется " <> bt n <> ", предоставляющий " <> bt pro <> "."
  Esperanto  -> "La pakaĵo, " <> bt pkg <> " bezonas " <> bt n <> ", kiu donas " <> bt pro <> "."
  Italian    -> "Il pacchetto " <> bt pkg <> " ha bisogno di " <> bt n <> ", che rende disponibile " <> bt pro <> "."
  Dutch      -> bt pkg <> " is afhankelijk van " <> bt n <> ", wat " <> bt pro <> " bevat."
  Ukrainian  -> "Пакунку " <> bt pkg <> " потрібен " <> bt n <> ", який забезпечує " <> bt pro <> "."
  Romanian   -> "Pachetul " <> bt pkg <> " are nevoie de " <> bt n <> ", care provizionează " <> bt pro <> "."
  Vietnamese -> "Gói " <> bt pkg <> " cần " <> bt n <> ", để cung cấp " <> bt pro <> "."
  Czech      -> "Balík " <> bt pkg <> " potřebuje " <> bt n <> ", který poskytuje " <> bt pro <> "."
  Korean     -> bt pkg <> " 패키지는 " <> bt pro <> "를 제공하는 " <> bt n <> "이(가) 필요합니다."
  Hindi      -> "पैकेज " <> bt pkg <> " को " <> bt n <> " की आवश्यकता है, जो " <> bt pro <> " प्रदान करता है।"
  Chinese    -> bt pkg <> " 需要 " <> bt n <> " 以提供 " <> bt pro <> "。"
  _          -> "The package " <> bt pkg <> " needs " <> bt n <> ", which provides " <> bt pro <> "."

missingPkg_3 :: Language -> Doc AnsiStyle
missingPkg_3 = \case
  Polish     -> "Wystąpił problem podczas reorganizowania grafu zależności. Jeśli widzisz tą wiadomość, coś poszło bardzo nie tak."
  Arabic     -> ".حدث خطا في اعادة تنظيم الرسم البياني التبعي. اذا رايت هذه الرسالة، فهناك مشكلة كبيرة"
  Spanish    -> "Se produjo un error al reorganizar el gráfico de dependencia. Si ves esto, algo está muy mal."
  Esperanto  -> "Eraro okazis kiam reorganizi la grafeo de dependeco. Io estas erarega."
  Italian    -> "C'è stato un errore nella riorganizzazione della gerarchia delle dipendenze. Se vedi questo messaggio, qualcosa è andato davvero storto."
  Dutch      -> "Er is een fout opgetreden tijdens het vernieuwen van de afhankelijkheidsgrafiek. Als u dit ziet, dan is er iets grondig mis."
  Romanian   -> "A fost o problemă reorganizând graful de dependențe. Dacă vedeți asta, e foarte rău."
  Vietnamese -> "Có lỗi trong quá trình xây dựng biểu đồ gói phụ thuộc. Nếu bạn thấy điều này, có gì đó không đúng."
  Czech      -> "Při reorganizaci grafu závislostí došlo k chybě. Pokud vidíte toto, něco je velmi špatně."
  Korean     -> "종속성 그래프를 재구성하는 동안 오류가 발생했습니다."
  Hindi      -> "निर्भरता ग्राफ़ को पुनर्व्यवस्थित करने में एक त्रुटि हुई। यदि आप इसे देखते हैं, तो कुछ बहुत गलत है।"
  Chinese    -> "重组依赖关系时出现错误。请注意，这是一个严重的错误。"
  _          -> "There was an error reorganizing the dependency graph. If you see this, something is very wrong."

missingPkg_4 :: [NonEmpty PkgName] -> Language -> Doc AnsiStyle
missingPkg_4 pns = \case
  Polish     -> vsep $ "Następujące cykle zależności zostały wykryte:" : pns'
  Arabic     -> vsep $ pns' <> [":تم اكتشاف دورات التبعية التالية"]
  Spanish    -> vsep $ "Se detectaron los siguientes ciclos de dependencia:" : pns'
  Italian    -> vsep $ "Sono stati individuati i seguenti cicli di dipendenza:" : pns'
  Dutch      -> vsep $ "De volgende afhankelijkheidscycli zijn gedetecteerd:" : pns'
  Ukrainian  -> vsep $ "Було помічено цикл залежностей:" : pns'
  Romanian   -> vsep $ "Aceste cicluri de dependență a fost detectate:" : pns'
  Vietnamese -> vsep $ "Phát hiện chu kỳ gói phụ thuộc: " : pns'
  Czech      -> vsep $ "Byly zjištěny následující cykly závislostí:" : pns'
  Korean     -> vsep $ "다음과 같은 종속성 주기가 발견되었습니다:" : pns'
  Hindi      -> vsep $ "निम्नलिखित निर्भरता चक्रों का पता लगाया गया:" : pns'
  Chinese    -> vsep $ "检测到循环依赖：" : pns'
  _          -> vsep $ "The following dependency cycles were detected:" : pns'
  where
    pns' :: [Doc ann]
    pns' = map (hsep . map pretty . L.intersperse "=>" . map pnName . toList) pns

missingPkg_5 :: PkgName -> Language -> Doc AnsiStyle
missingPkg_5 (PkgName p) = \case
  Polish     -> bt p <> " nie istnieje."
  Arabic     -> ".ليس موجود " <> bt p
  Spanish    -> bt p <> " no existe."
  Italian    -> bt p <> " non esiste."
  Dutch      -> bt p <> " bestaat niet."
  Ukrainian  -> "Пакунок " <> bt p <> " не існує."
  Romanian   -> "Pachetul " <> bt p <> " nu există."
  Vietnamese -> bt p <> " không tồn tại."
  Czech      -> bt p <> " neexistuje."
  Korean     -> bt p <> "은(는) 존재하지 않습니다."
  Hindi      -> bt p <> " मौजूद नहीं है।"
  Chinese    -> bt p <> " 不存在。"
  _          -> bt p <> " does not exist."

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
    Chinese    -> "可用语言如下："
    Esperanto  -> "La sekvaj lingvo estas disponebla:"
    Dutch      -> "De volgende talen zijn beschikbaar:"
    Ukrainian  -> "Доступні наступні мови:"
    Romanian   -> "Aceste pacheturi sunt disponibile:"
    Vietnamese -> "Ngôn ngữ khả dụng:"
    Czech      -> "K dispozici jsou následující jazyky:"
    Korean     -> "다음 언어는 이용 가능합니다:"
    Hindi      -> "निम्नलिखित भाषाएँ उपलब्ध हैं:"
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
    Chinese    -> "Aura 有新版本可用。要先更新吗？"
    Swedish    -> "Det finns en uppdatering tillgänglig till Aura. Vill du uppdatera Aura först?"
    Esperanto  -> "Ĝisdatigo de Aura estas disponebla. Ĉu ĝisdatigas ĝin?"
    Dutch      -> "Er is een update van Aura beschikbaar. Wilt u Aura nu bijwerken?"
    Ukrainian  -> "Доступно оновлення для Aura. Бажаєте оновити її першою?"
    Romanian   -> "O versiune nouă de Aura este disponibilă. Să se actualizeze înainte de toate?"
    Vietnamese -> "Đã có cập nhật cho Aura. Cập nhật?"
    Czech      -> "K dispozici aktualizace Aury. Nejprve ji aktualizovat?"
    Korean     -> "Aura의 최신버전이 있습니다. 업데이트를 하시겠습니까?"
    Hindi      -> "Aura अपडेट उपलब्ध है। पहले इसे अपडेट करें?"
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
    Chinese    -> "未指定有效的包。"
    Esperanto  -> "Ne validajn pakaĵojn specifis"
    Dutch      -> "Er zijn geen geldige pakketten opgegeven."
    Ukrainian  -> "Валідні пакунки не вказані."
    Romanian   -> "Nu s-a specificat nici un pachet valabil."
    Vietnamese -> "Tên của gói được yêu cầu không đúng."
    Czech      -> "Nejsou zadány žádné platné balíčky."
    Korean     -> "유효한 패키지가 지정되지 않았습니다."
    Hindi      -> "कोई वैध पैकेज निर्दिष्ट नहीं है।"
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
    Dutch      -> "Wilt u doorgaan?"
    Ukrainian  -> "Продовжити?"
    Romanian   -> "Continuați?"
    Vietnamese -> "Tiếp tục?"
    Czech      -> "Pokračovat?"
    Korean     -> "계속하시겠습니까?"
    Hindi      -> "जारी रखें?"
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
    Chinese    -> "安装被手动中止。"
    Esperanto  -> "Instalon ĉesigi permane"
    Dutch      -> "De installatie is handmatig afgebroken."
    Ukrainian  -> "Встановлення скасовано користувачем."
    Romanian   -> "Instalarea anulată manual."
    Vietnamese -> "Quá trình cài đặt được hủy."
    Czech      -> "Instalace byla ručně přerušena."
    Korean     -> "설치가 중지되었습니다."
    Hindi      -> "इंस्टॉलेशन मैन्युअल रूप से निरस्त किया गया।"
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
    Dutch      -> "Bezig met vaststellen van afhankelijkheden…"
    Ukrainian  -> "Визначення залежностей..."
    Romanian   -> "Se determin dependențele..."
    Vietnamese -> "Xác định các gói phụ thuộc..."
    Czech      -> "Určování závislostí..."
    Korean     -> "종속성 확인 중..."
    Hindi      -> "निर्भरता का निर्धारण..."
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
    Chinese    -> p <> " 已被标记为忽略。仍要安装吗？"
    Swedish    -> p <> " är markerad som ignorerad. Vill du installera ändå?"
    Esperanto  -> p <> " estas markita kiel malatenta. Ĉu instali?"
    Dutch      -> p <> " is gemarkeerd als genegeerd. Wilt u het pakket toch installeren?"
    Romanian   -> p <> " e marcat ca ignorat. Să se instaleze oricum?"
    Vietnamese -> p <> " được đánh dấu là Bỏ qua. Vẫn cài đặt nó?"
    Czech      -> p <> " je označeno jako ignorováno. Přesto nainstalovat?"
    Korean     -> p <> "은(는) 무시됨으로 표시됩니다. 설치를 하시겠습니까?"
    Hindi      -> p <> " को उपेक्षित के रूप में चिह्नित किया गया है। फिर भी इंस्टॉल करें?"
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
    Dutch      -> "De volgende pakketten zijn geen AUR-pakketten:"
    Ukrainian  -> "Нижче вказано те, що не є пакунком AUR:"
    Romanian   -> "Aceste pachete nu se află pe AUR:"
    Vietnamese -> "Các gói sau không thuộc AUR:"
    Czech      -> "Následující nejsou balíčky AUR:"
    Korean     -> "AUR 패키지가 아닙니다:"
    Hindi      -> "निम्नलिखित AUR पैकेज नहीं हैं:"
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
    Dutch      -> "De volgende pakketten zijn al geinstalleerd:"
    Ukrainian  -> "Наступні пакунки вже встановлені:"
    Romanian   -> "Aceste pachete sunt deja instalate:"
    Vietnamese -> "Các gói sau đã sẵn sàng cài đặt:"
    Czech      -> "Následující balíčky jsou již nainstalovány:"
    Korean     -> "아래 패키지는 이미 설치되어 있습니다:"
    Hindi      -> "निम्नलिखित पैकेज पहले से ही इंस्टॉल हैं:"
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
    Dutch      -> "Pakketbron-afhankelijkheden:"
    Ukrainian  -> "Залежності репозиторія:"
    Romanian   -> "Dependențe din repertorii:"
    Vietnamese -> "Các repo phụ thuộc:"
    Czech      -> "Závislosti úložiště:"
    Korean     -> "리포지토리 종속성:"
    Hindi      -> "रिपोजिटरी निर्भरताएं:"
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
    Dutch      -> "AUR-pakketten:"
    Ukrainian  -> "Пакунки AUR:"
    Romanian   -> "Pachete din AUR:"
    Vietnamese -> "Gói AUR:"
    Czech      -> "Balíčky AUR:"
    Korean     -> "AUR 패키지:"
    Hindi      -> "AUR पैकाग्रेस:"
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
    Czech      -> "Závislosti AUR:"
    Korean     -> "AUR 종속성:"
    Hindi      -> "AUR निर्भरताएं:"
    Chinese    -> "AUR 依赖："
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
    Chinese    -> p <> " 的 PKGBUILD 尚未保存。"
    Swedish    -> p <> " har ännu ingen PKGBUILD."
    Esperanto  -> p <> " ne havas PKGBUILD jam."
    Dutch      -> p <> " heeft nog geen opgeslagen PKGBUILD."
    Ukrainian  -> "В " <> p <> " ще не зберігається PKGBUILD."
    Romanian   -> p <> " încă nu are un PKGBUILD descărcat."
    Vietnamese -> p <> " không có sẵn PKGBUILD."
    Czech      -> p <> " ještě nemá uložený PKGBUILD."
    Korean     -> p <> "의 PKGBUILD는 저장되지 않았습니다."
    Hindi      -> p <> " में अभी तक कोई PKGBUILD संग्रहीत नहीं है."
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
    Dutch      -> p <> " PKGBUILD-aanpassingen:"
    Ukrainian  -> "Зміни PKGBUILD в " <> p <> ":"
    Romanian   -> "Schimbări in PKGBUILD pentru " <> p <> ":"
    Vietnamese -> "Thay đổi trong PKGBUILD của " <> p <> ":"
    Czech      -> "Změny PKGBUILD v " <> p <> ":"
    Korean     -> p <> "의 PKGBUILD 변경 사항:"
    Hindi      -> p <> "PKGBUILD परिवर्तन:"
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
    Dutch      -> "Bij te werken AUR-pakketten:"
    Ukrainian  -> "Пакунки AUR, готові для оновлення:"
    Romanian   -> "Pachete din AUR de actualizat:"
    Vietnamese -> "Cập nhật các gói AUR:"
    Czech      -> "Balíčky AUR k aktualizaci:"
    Korean     -> "업그레이드할 AUR 패키지:"
    Hindi      -> "अपग्रेड करने के लिए AUR पैकेज:"
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
    Chinese    -> "以下包在缓存中没有版本，无法降级："
    Esperanto  -> "La sekvaj pakaĵoj havas ne kaŝmemorigitajn versiojn, do ĝi ne povas malpromociigi:"
    Dutch      -> "De volgende pakketten zijn niet gecachet en kunnen daarom niet worden afgewaardeerd."
    Ukrainian  -> "Наступних пакунків немає в кеші. Отже, вони не можуть відкотитися до старої версії:"
    Romanian   -> "Aceste pachete nu au nici o versiune disponibilă în cache, așa că nu pot fi retrogradate:"
    Vietnamese -> "Những gói sau không có bản nào trong cache, vì vậy không thể hạ cấp:"
    Czech      -> "Následující nemají žádné verze v mezipaměti, a proto je nelze downgradovat:"
    Korean     -> "이 패키지는 캐시에 저장된 다른 버전이 없으므로 다운그레이드를 할 수 없습니다."
    Hindi      -> "निम्नलिखित का कैश में कोई संस्करण नहीं है, और इसलिए इसे डाउनग्रेड नहीं किया जा सकता:"
    _          -> "The following have no versions in the cache, and thus can’t be downgraded:"

reportBadDowngradePkgs_2 :: PkgName -> Language -> Doc AnsiStyle
reportBadDowngradePkgs_2 (PkgName p) = \case
  Spanish    -> pretty p <+> "no tiene una versión en la caché."
  Arabic     -> ".ليس  لديه اصدار في الذاكرة التخزين الموقت" <+> pretty p
  Italian    -> pretty p <+> "non ha alcuna versione nella cache."
  Dutch      -> pretty p <+> "is niet gecachet."
  Ukrainian  -> pretty p <+> "не має версії в кеші."
  Romanian   -> pretty p <+> "nu are nici o versiune în cache."
  Vietnamese -> pretty p <+> "không có bản nào trong cache."
  Czech      -> pretty p <+> "nemá žádnou verzi v mezipaměti."
  Korean     -> pretty p <+> "의 캐시에는 저장된 다른 버전이 없습니다."
  Hindi      -> pretty p <+> "का कैश में कोई संस्करण नहीं है."
  Chinese    -> pretty p <+> "在缓存中没有版本。"
  _          -> pretty p <+> "has no version in the cache."

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
    Dutch      -> "Bezig met ophalen van pakketinformatie…"
    Ukrainian  -> "Збираємо інформацію про пакунок..."
    Romanian   -> "Se obțin informații despre pachete..."
    Vietnamese -> "Cập nhật thông tin của gói..."
    Czech      -> "Načítání informací o balíčku..."
    Korean     -> "패키지 정보 가져오는 중..."
    Hindi      -> "पैकेज की जानकारी प्राप्त की जा रही है..."
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
    Dutch      -> "Bezig met vergelijken van pakketversies…"
    Ukrainian  -> "Порівнюємо версії пакунків..."
    Romanian   -> "Se compar versiunile pacheturilor..."
    Vietnamese -> "So sánh phiên bản của gói..."
    Czech      -> "Porovnání verzí balíčků..."
    Korean     -> "패키지 버전 비교 중..."
    Hindi      -> "पैकेज संस्करणों की तुलना करना..."
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
    Dutch      -> "Er hoeven geen AUR-pakketten te worden bijgewerkt."
    Ukrainian  -> "Пакунки AUR не потребують оновлення."
    Romanian   -> "Nu e nevoie să se actualizeze nici un pachet din AUR."
    Vietnamese -> "Không có cập nhật cho các gói AUR."
    Czech      -> "Není nutné žádné aktualizace balíčku AUR."
    Korean     -> "업그레이드가 필요하지 않습니다."
    Hindi      -> "कोई AUR पैकेज अपग्रेड आवश्यक नहीं है।"
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
    Dutch      -> "Bezig met verwijderen van onnodige make-afhankelijkheden…"
    Ukrainian  -> "Видаляємо непотрібні залежності make..."
    Romanian   -> "Se șterg dependențele de compilare inutile..."
    Vietnamese -> "Loại bỏ các gói phụ thuộc khi make không cần thiết..."
    Czech      -> "Odstranění nepotřebných make závislostí..."
    Korean     -> "필요없는 make 의존성 제거 중..."
    Hindi      -> "अनावश्यक निर्भरताएँ हटाया जा रहा है..."
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
    Chinese    -> "保留" <> s <> " 个包的状态记录，并删除其它的？"
    Swedish    -> s <> " paket kommer att bevaras. Ta bort resten?"
    Esperanto  -> s <> " statoj de pakaĵoj teniĝas. Ĉu forigi la ceteron?"
    Dutch      -> s <> " pakketstatussen worden behouden. Wilt u de rest verwijderen?"
    Ukrainian  -> s <> " стан пакунків будуть залишені. Видалити решту?"
    Romanian   -> "Stările pachetului " <> s <> " vor fi păstrate. Să se șteargă restul?"
    Vietnamese -> "Trạng thái của gói " <> s <> " sẽ được lưu lại. Loại bỏ phần còn lại?"
    Czech      -> s <> " stavy balíčků budou zachovány. Odstranit zbytek?"
    Korean     -> s <> " 패키지는 유지됩니다. 나머지를 제거하시겠습니까?"
    Hindi      -> s <> " पैकेज स्थितियों को रखा जाएगा। बाकी हटा दें?"
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
    Chinese    -> "未删除任何包的状态记录。"
    Swedish    -> "Inga paket togs bort."
    Esperanto  -> "Ne statojn de pakaĵoj forigis."
    Dutch      -> "Er zijn geen pakketstatussen verwijderd."
    Ukrainian  -> "Стани пакунків залишились недоторкані."
    Romanian   -> "Nici o stare de pachet a fost ștearsă."
    Vietnamese -> "Không có trạng thái gói nào được lưu."
    Czech      -> "Nebyly odstraněny žádné stavy balíčku."
    Korean     -> "아무 패키지도 삭제되지 않았습니다."
    Hindi      -> "कोई पैकेज स्थिति हटाई नहीं गई थी।"
    _          -> "No package states were removed."

cleanStates_4 :: Int -> Language -> Doc AnsiStyle
cleanStates_4 n = \case
  Japanese   -> "現在のパッケージ状態記録：" <+> pretty n <+> "個。"
  Arabic     -> ".محفوظة " <+> pretty n <+> " لديك حاليا حالات حزمة"
  Polish     -> "Chwilowo posiadasz" <+> pretty n <+> "zapisanych stanów pakietów."
  Spanish    -> "Actualmente tiene " <+> pretty n <+> "estados de paquetes guardados."
  Russian    -> "У вас сейчас " <+> pretty n <+> pluralRussian " сохраненное состояние пакета" " сохраненных состояний пакета" " сохраненных состояний пакетов." n
  Italian    -> "Al momento ci sono" <+> pretty n <+> "stati di pacchetti salvati."
  Esperanto  -> "Vi havas " <+> pretty n <+> " konservajn statojn de pakaĵoj."
  Dutch      -> "U heeft momenteel" <+> pretty n <+> "opgeslagen pakketstatussen."
  Ukrainian  -> "Зараз ви маєте " <+> pretty n <+> " збережених станів пакунків."
  Romanian   -> "Momentan aveți " <+> pretty n <+> " stări de pachet salvate."
  Vietnamese -> "Bạn hiện đã lưu " <+> pretty n <+> " trạng thái gói."
  Czech      -> "V současné době máte " <+> pretty n <+> " uložených stavů balíčků."
  Korean     -> "현재" <+> pretty n <+> "개의 패키지 상태가 저장되어 있습니다."
  Hindi      -> "वर्तमान में आपके पास " <+> pretty n <+> " सहेजी गई पैकेज स्थितियाँ हैं।"
  Chinese    -> "目前已保存 " <+> pretty n <+> " 个包的状态记录。"
  _          -> "You currently have " <+> pretty n <+> " saved package states."

cleanStates_5 :: Text -> Language -> Doc AnsiStyle
cleanStates_5 t = \case
  Japanese   -> "一番最近に保存されたのは：" <+> pretty t
  Arabic     -> pretty t <+> ":احدث ما تم حفظه"
  Polish     -> "Ostatnio zapisane:" <+> pretty t
  Spanish    -> "Guardado recientemente:" <+> pretty t
  Russian    -> "Последнее сохраненное:" <+> pretty t
  Italian    -> "Salvato più recentemente:" <+> pretty t
  Esperanto  -> "Lastaj konservaj:" <+> pretty t
  Dutch      -> "Onlangs opgeslagen:" <+> pretty t
  Ukrainian  -> "Останні збереженні:" <+> pretty t
  Romanian   -> "Cel mai recent salvat:" <+> pretty t
  Vietnamese -> "Lần lưu gần nhất:" <+> pretty t
  Czech      -> "Naposledy uložené:" <+> pretty t
  Korean     -> "최근에 저장된 패키지:" <+> pretty t
  Hindi      -> "हाल ही में सहेजा गया:" <+> pretty t
  Chinese    -> "最近一次保存：" <+> pretty t
  _          -> "Most recently saved:" <+> pretty t

cleanStates_6 :: Int -> Language -> Doc AnsiStyle
cleanStates_6 n = \case
  Polish     -> pretty n <+> "jest przypiętych i nie zostanie usuniętych."
  Arabic     -> ".اذا كانو مثبتين ولا يمكن ازالتهم " <+> pretty n
  Spanish    -> pretty n <+> "de estos están anclados y no se eliminarán."
  Italian    -> pretty n <+> "di questi sono stati fissati, perciò non saranno rimossi."
  Dutch      -> pretty n <+> "hiervan zijn vastgezet en worden daarom niet verwijderd."
  Ukrainian  -> pretty n <+> "були закріплені та залишуться недоторканими."
  Romanian   -> pretty n <+> "dintre astea sunt fixate, și nu vor fi șterse."
  Vietnamese -> pretty n <+> "trong số chúng đã được ghim, và sẽ không bị loại bỏ."
  Czech      -> pretty n <+> "z nich jsou připnuté a nebudou odstraněny."
  Korean     -> pretty n <+> "은(는) 고정되어 삭제되지 않습니다."
  Hindi      -> "इनमें से" <+> pretty n <+> "को पिन कर दिया गया है, और हटाया नहीं जाएगा।"
  Chinese    -> "其中 " <+> pretty n <+> " 个已被固定，不会被删除。"
  _          -> pretty n <+> "of these are pinned, and won't be removed."

readState_1 :: Language -> Doc AnsiStyle
readState_1 = \case
    Polish     -> "Ten plik stanu nie mógł zostać odczytany. Czy jest to prawidłowy plik JSON?"
    Arabic     -> "صحيح؟ JSON فشل في تحليل ملف الحالة. هل"
    Spanish    -> "Ese archivo de estado no se pudo analizar. ¿Es un archivo JSON válido?"
    Portuguese -> "O arquivo de estado não pôde ser interpretado. É um arquivo JSON válido?"
    Russian    -> "Это состояние не распознано. Это корректный JSON?"
    Italian    -> "Non è stato possibile analizzare il file di stato. E' correttamente formattato in JSON?"
    Esperanto  -> "Tiu statdosiero paneis sintake analizi. Ĉu ĝi estas valida JSON?"
    Dutch      -> "Dit statusbestand kan niet worden verwerkt. Bevat het bestand geldige JSON?"
    Ukrainian  -> "Стан не був розпізнаний правильно. Це точно коректний JSON?"
    Romanian   -> "Acel fișier de stare nu se putea analiza. Este un fișier JSON valabil?"
    Vietnamese -> "Thất bại trong việc lấy dữ liệu từ tệp. Đó có đúng là tệp JSON?"
    Czech      -> "Tento stavový soubor se nepodařilo analyzovat. Je to legální JSON?"
    Korean     -> "상태 파일을 분석할 수 없습니다. 올바른 JSON 입니까?"
    Hindi      -> "वह स्थिति फ़ाइल पार्स करने में विफल रही. क्या यह वैध JSON है?"
    Chinese    -> "无法解析状态文件。这是否是一个合法的 JSON 文件？"
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
    Chinese    -> "要安装哪个版本的" <> p <> "？"
    Esperanto  -> "Kiu versio de " <> p <> " vi volas?"
    Dutch      -> "Welke versie van " <> p <> " wilt u?"
    Ukrainian  -> "Яку версію пакунку " <> p <> " ви бажаєте?"
    Romanian   -> "Care versiune al pachetului " <> p <> " o doriți?"
    Vietnamese -> "Bạn muốn sử dụng phiên bản nào của " <> p <> "?"
    Czech      -> "Jakou verzi " <> p <> " chcete?"
    Korean     -> "어느 " <> p <> " 버전을 설치하시겠습니까?"
    Hindi      -> "आपको " <> p <> " का कौन सा संस्करण चाहिए?"
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
    Dutch      -> "De back-uplocatie bestaat niet."
    Ukrainian  -> "Шлях до резервної копії не існує."
    Romanian   -> "Locul de reservă nu există."
    Vietnamese -> "Đường dẫn sao lưu không tồn tại."
    Czech      -> "Umístění zálohy neexistuje."
    Korean     -> "백업 위치를 찾을 수 없습니다."
    Hindi      -> "बैकअप स्थान मौजूद नहीं है।"
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
    Dutch      -> "Bezig met back-uppen van cache aan naar " <> dir
    Ukrainian  -> "Зберігаємо резервну копію до " <> dir
    Romanian   -> "Se copiază cache-ul de rezervă către " <> dir
    Vietnamese -> "Sao lưu cache vào " <> dir
    Czech      -> "Zálohování mezipaměti do " <> dir
    Korean     -> "캐시를 백업하는 중 " <> dir
    Hindi      -> "कैश का बैकअप " <> dir <> " में लिया जा रहा है।"
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
    Dutch      -> "Te back-uppen pakketbestanden: " <> n
    Ukrainian  -> "Файли пакунку для резервної копії: " <> n
    Romanian   -> "Fișiere de pachet pentru copiare de rezervă: " <> n
    Vietnamese -> "Các tệp của gói sẽ được sao lưu: " <> n
    Czech      -> "Soubory k zálohování: " <> n
    Korean     -> "백업할 패키지 파일: " <> n
    Hindi      -> "बैकअप के लिए फ़ाइलों को पैकेज करें:" <> n
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
    Dutch      -> "Wilt u doorgaan met back-uppen?"
    Ukrainian  -> "Продовжити створення резервної копії?"
    Romanian   -> "Continuați cu copiile de rezervă?"
    Vietnamese -> "Tiến hành sao lưu?"
    Czech      -> "Pokračovat v zálohování."
    Korean     -> "백업 하시겠습니까?"
    Hindi      -> "क्या आप बैकअप जारी रखना चाहते हैं?"
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
    Chinese    -> "备份被手动中止。"
    Esperanto  -> "Enarkivigadon ĉesigis permane."
    Dutch      -> "Het back-upproces is handmatig afgebroken."
    Ukrainian  -> "Створення резервної копії перервано користувачем."
    Romanian   -> "Copiarea de rezervă anulată manual."
    Vietnamese -> "Quá trình sao lưu được hủy."
    Czech      -> "Zálohování ručně přerušeno."
    Korean     -> "백업이 중지되었습니다."
    Hindi      -> "बैकअप मैन्युअल रूप से निरस्त किया गया।"
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
    Dutch      -> "Er wordt een back-up gemaakt. Dit kan enkele minuten duren…"
    Ukrainian  -> "Створюємо резервну копію. Це може зайняти декілька хвилин..."
    Romanian   -> "Se fac copii de rezervă. Ar putea să dureze câteva minute..."
    Vietnamese -> "Đang sao lưu. Có thể sẽ mất vài phút..."
    Czech      -> "Zálohování. Může to trvat několik minut..."
    Korean     -> "백업 중입니다. 몇 분 정도 걸릴 수 있습니다..."
    Hindi      -> "बैकअप लिया जा रहा है। इसमें कुछ मिनट लग सकते हैं..."
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
    Dutch      -> "Bezig met kopiëren: #[" <> n <> "]"
    Ukrainian  -> "Копіюємо #[" <> n <> "]"
    Romanian   -> "Se copiază #[" <> n <> "]"
    Vietnamese -> "Sao chép #[" <> n <> "]"
    Czech      -> "Kopírování #[" <> n <> "]"
    Korean     -> "복사중 #[" <> n <> "]"
    Hindi      -> "कॉपी किया जा रहा है #[" <> n <> "]"
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
    Chinese    -> "这将会删除所有包缓存。"
    Esperanto  -> "Ĉi tiu forigos la TUTAN kaŝmemoron de pakaĵoj."
    Dutch      -> "Hiermee wordt de GEHELE pakketcache gewist."
    Ukrainian  -> "Ця операція ПОВНІСТЮ видалить кеш пакунків."
    Romanian   -> "Asta va șterge COMPLET cache-ul de pachete."
    Vietnamese -> "Điều này sẽ xóa TOÀN BỘ cache của gói."
    Czech      -> "Tím smažete CELOU mezipaměť balíčku."
    Korean     -> "모든 패키지 캐시가 삭제됩니다."
    Hindi      -> "यह संपूर्ण पैकेज कैश हटा देगा।"
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
    Czech      -> s <> " každého souboru balíčku bude zachován."
    Korean     -> "각 패키지 파일에 대해 " <> s <> "개의 파일이 유지되어야 합니다."
    Hindi      -> s <> "प्रत्येक पैकेज फ़ाइल को रखा जाएगा।"
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
    Chinese    -> "其余的将被删除。确定吗？"
    Esperanto  -> "La cetero foriĝos. Ĉu bone?"
    Dutch      -> "De rest wordt gewist. Weet u het zeker?"
    Ukrainian  -> "Все інше буде видалено. Гаразд?"
    Romanian   -> "Restul va fi șters. De acord?"
    Vietnamese -> "Xóa bỏ phần còn lại. Ok?"
    Czech      -> "Zbytek bude smazán. Ok?"
    Korean     -> "나머지는 모두 삭제됩니다. 계속하시겠습니까?"
    Hindi      -> "बाकी हटा दिया जाएगा। ठीक है?"
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
    Chinese    -> "缓存清理被手动中止。"
    Esperanto  -> "Puriganta Kaŝmemoro ĉesis permane."
    Dutch      -> "De cache-opruiming is handmatig afgebroken."
    Ukrainian  -> "Очищення кешу було перервано користувачем."
    Romanian   -> "Curățenia cache-ului anulată manual."
    Vietnamese -> "Đã hủy xóa cache."
    Czech      -> "Čištění mezipaměti bylo ručně přerušeno"
    Korean     -> "캐시 정리가 중지되었습니다."
    Hindi      -> "कैश की सफ़ाई मैन्युअल रूप से निरस्त कर दी गई।"
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
    Dutch      -> "Bezig met opruimen van pakketcache…"
    Ukrainian  -> "Очищуємо кеш пакунків..."
    Romanian   -> "Se curăță cache-ul de pachete..."
    Vietnamese -> "Xóa cache..."
    Czech      -> "Čištění mezipaměti balíčků"
    Korean     -> "패키지 캐시 정리 중..."
    Hindi      -> "पैकेज कैश साफ़ किया जा रहा है..."
    _          -> "Cleaning package cache..."

cleanCache_7 :: Word -> Word -> Language -> Doc AnsiStyle
cleanCache_7 (bt . tshow -> ps) (bt . tshow -> bytes) = \case
    Arabic     -> ".ميغابايت " <> bytes <> " الذي تاخذ " <> ps <> " الذاكرة التخزين الموقت فيه الرزمة"
    Polish     -> "Pamięć podręczna posiada " <> ps <> " pakietów, zajmujących " <> bytes <> " megabajtów."
    Spanish    -> "La caché contiene " <> ps <> " paquetes, consumiendo " <> bytes <> " megabytes."
    Ukrainian  -> "Кеш містить " <> ps <> " пакунків, які використовують " <> bytes <> " МБ місця."
    Romanian   -> "Cache-ul conține " <> ps <> " pachete, consumând " <> bytes <> " MB."
    Vietnamese -> "Có " <> ps <> " gói trong cache, chiếm " <> bytes <> " megabytes."
    Korean     -> ps <> "개의 패키지는 " <> bytes <> "MB 사용 중입니다."
    Hindi      -> "कैश में " <> ps <> " पैकेज हैं, जो " <> bytes <> " मेगाबाइट का उपभोग करते हैं।"
    Dutch      -> "De cache bevat " <> ps <> " pakketten, met een totale omvang van " <> bytes <> " megabytes."
    Chinese    -> "缓存中有 " <> ps <> " 个包，共计 " <> bytes <> " MB。"
    _          -> "The cache contains " <> ps <> " packages, consuming " <> bytes <> " megabytes."

cleanCache_8 :: Word -> Language -> Doc AnsiStyle
cleanCache_8 (bt . tshow -> bytes) = \case
    Arabic     -> ".ميغابايت محررة " <> bytes
    Polish     -> bytes <> " megabajtów zwolnionych."
    Spanish    -> bytes <> " megabytes liberados."
    Ukrainian  -> bytes <> " МБ звільнилось."
    Romanian   -> bytes <> " MB eliberat."
    Vietnamese -> "Giải phóng " <> bytes <> "megabytes."
    Czech      -> "Uvolněno " <> bytes <> " MB."
    Korean     -> bytes <> " MB 정리되었습니다."
    Hindi      -> bytes <> "मेगाबाइट मुक्त हो गए।"
    Dutch      -> bytes <> "megabytes vrijgemaakt."
    Chinese    -> "已释放 " <> bytes <> " MB。"
    _          -> bytes <> " megabytes freed."

cleanCache_9 :: Word -> Language -> Doc AnsiStyle
cleanCache_9 (bt . tshow -> w) = \case
    Romanian   -> w <> " versiuni din fiecare pachet instalat vor fi păstrate."
    Vietnamese -> "Sẽ giữ lại " <> w <> " phiên bản của các gói đã cài đặt."
    Czech      -> w <> " verze každého nainstalovaného balíčku budou zachovány."
    Korean     -> "각각의 설치된 패키지의 " <> w <> "은(는) 유지됩니다."
    Hindi      -> w <> "प्रत्येक स्थापित पैकेज के संस्करण रखे जाएंगे।"
    Dutch      -> "Er worden " <> w <> "versies van elke geïnstalleerd pakket bewaard."
    Chinese    -> "每个包将会保存 " <> w <> " 个版本。"
    _          -> w <> " versions of each installed package will be kept."

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
    Dutch      -> "Bezig met vaststellen van overbodige pakketbestanden…"
    Ukrainian  -> "Визначачення непотрібних пакунків..."
    Romanian   -> "Se determin fișiere de pachet inutile..."
    Vietnamese -> "Xác định các tệp của gói không cần thiết..."
    Czech      -> "Zjišťování nepotřebných souborů balíčků..."
    Korean     -> "필요 없는 패키지 파일 확인 중..."
    Hindi      -> "अनावश्यक पैकेज फ़ाइलों का निर्धारण..."
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
    Dutch      -> "Er zijn " <> s <> " overbodige pakketbestanden aangetroffen. Wilt u deze bestanden wissen?"
    Ukrainian  -> "Знайдено " <> s <> " непотрібних пакунків. Видалити?"
    Romanian   -> "S-au găsit " <> s <> " fișiere de pachet inutile. Ștergeți?"
    Vietnamese -> "Tìm thấy " <> s <> " gói không cần thiết. Xóa bỏ?"
    Czech      -> "Nepotřebné soubory balíčků: " <> s <> ". Vymazat?"
    Korean     -> s <> "는 필요 없는 패키지 파일입니다. 삭제하시겠습니까?"
    Hindi      -> s <> "अनावश्यक पैकेज फ़ाइलें मिलीं। हटाएं?"
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
    Dutch      -> "Deze zijn niet toegevoegd aan het logboek:"
    Ukrainian  -> "Наступних пакунків немає в лог файлі:"
    Romanian   -> "Acestea nu au apărut în log:"
    Vietnamese -> "Nội dung sau không có trong tệp log:"
    Czech      -> "Tyto se neobjevily v souboru log:"
    Korean     -> "로그 파일에 나타나지 않음:"
    Hindi      -> "ये लॉग फ़ाइल में दिखाई नहीं दिए हैं:"
    _          -> "These have not appeared in the log file:"

-------------------------------
-- Aura/AUR functions
-------------------------------

packageNotFound_1 :: Language -> Doc AnsiStyle
packageNotFound_1 = \case
  Romanian   -> "Nu s-a găsit nici un pachet."
  Vietnamese -> "Không tím thấy gói."
  Czech      -> "Nebyly nalezeny žádné balíčky."
  Korean     -> "패키지를 찾을 수 없습니다."
  Hindi      -> "कोई पैकेज नहीं मिला।"
  Chinese    -> "未找到软件包。"
  _          -> "No packages found."

-- https://github.com/fosskers/aura/issues/498
connectFailure_1 :: Language -> Doc AnsiStyle
connectFailure_1 = \case
  Polish     -> "Nie udało się nawiązać połączenia z AUR. Czy jesteś połączony z internetem?"
  Arabic     -> "هل انت متصل بالانترنت؟ .AURفشل الاتصال بـ"
  Spanish    -> "No se pudo contactar con el AUR. ¿Tienes conexión a internet?"
  Italian    -> "Non è stato possibile contattare l'AUR. Il computer è connesso ad internet?"
  Dutch      -> "Er kan geen verbinding worden gemaakt met de AUR. Bent u verbonden met het internet?"
  Ukrainian  -> "Не вдалося зв'язатись з AUR. У вас є підключення до інтернету?"
  Romanian   -> "Nu s-a putut contacta AUR. Sunteți conectat pe Internet?"
  Vietnamese -> "Mất kết nối tới AUR. Bạn có kết nối mạng không?"
  Czech      -> "Nepodařilo se kontaktovat AUR server. Máte připojení k internetu?"
  Korean     -> "AUR에 접근하지 못했습니다. 인터넷 연결 상태를 확인하십시오."
  Hindi      -> "AUR से संपर्क करने में असफल। क्या आपके पास इंटरनेट कनेक्शन है?"
  Chinese    -> "无法与 AUR 通信。请检查网络。"
  _          -> "Failed to contact the AUR. Do you have an internet connection?"

dependencyLookup_1 :: Text -> Language -> Doc AnsiStyle
dependencyLookup_1 t = \case
  Romanian   -> vsep ["A fost o problemă cu analiza recursivă de dependențe:", pretty t]
  Vietnamese -> vsep ["Có lỗi trong quá trình tìm kiếm gói phụ thuộc đệ quy:", pretty t]
  Czech      -> vsep ["Při rekurzivním vyhledávání závislostí došlo k problému:", pretty t]
  Korean     -> vsep ["재귀 종속성 조회 중 문제가 발생했습니다:", pretty t]
  Hindi      -> vsep ["पुनरावर्ती निर्भरता लुकअप के दौरान एक समस्या थी:", pretty t]
  Chinese    -> vesp ["在递归依赖查找中遇到了问题：", pretty t]
  _          -> vsep ["There was an issue during recursive dependency lookup:", pretty t]

miscAURFailure_1 :: Language -> Doc AnsiStyle
miscAURFailure_1 = \case
  Polish     -> "Wystąpił nieznany błąd podczas próby łączenia z AUR."
  Arabic     -> ".بطريقة غير معروفة AURفشل الاتصال بـ"
  Spanish    -> "El contacto con el AUR falló de alguna manera desconocida."
  Italian    -> "C'è stato un errore sconosciuto nel contattare l'AUR."
  Dutch      -> "Er kan om onbekende reden geen verbinding worden gemaakt met de AUR."
  Ukrainian  -> "Зв'язок з AUR було обірвано невідомим чином."
  Romanian   -> "Nu s-a putut contacta AUR dintr-un motiv necunoscut."
  Vietnamese -> "Bất ngờ không thể kết nối tới AUR."
  Czech      -> "Kontaktování AUR se nezdařilo neznámým způsobem."
  Korean     -> "알 수 없는 문제로 AUR에 접근하지 못했습니다."
  Hindi      -> "AUR से संपर्क किसी अज्ञात तरीके से विफल रहा।"
  Chinese    -> "与 AUR 通信时遇到了未知的错误。"
  _          -> "Contacting the AUR failed in some unknown way."

miscAURFailure_3 :: Language -> Doc AnsiStyle
miscAURFailure_3 = \case
  Polish     -> "Plik JSON zwrócony z AUR nie mógł zostać rozszyfrowany."
  Arabic     -> ".AURالذي تم ارجاعه من اﻟ JSONفشل فك شفرة اﻟ"
  Spanish    -> "El JSON devuelto por el servidor AUR no se pudo decodificar."
  Ukrainian  -> "JSON, який повернувся з сервера AUR, неможливо розшифрувати."
  Romanian   -> "JSON-ul întors de server-ul AUR nu putea fi decodat."
  Vietnamese -> "Không thể giải mã tệp JSON lấy từ máy chủ AUR."
  Czech      -> "JSON vrácený ze serveru AUR nelze dekódovat."
  Korean     -> "AUR 서버에서 받은 JSON을 디코딩할 수 없습니다."
  Hindi      -> "AUR सर्वर से लौटाया गया JSON डिकोड नहीं किया जा सका।"
  Dutch      -> "De AUR-server stuurde JSON terug die niet kan worden ontsleuteld."
  Chinese    -> "无法解码 AUR 服务器返回的 JSON。网络连接是否被污染？"
  _          -> "The JSON returned from the AUR server could not be decoded."

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
    Czech      -> "Zastaralý!"
    Korean     -> "최신 버전이 아님!"
    Hindi      -> "पुराना हो चुका है!"
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
    Dutch      -> "Actueel"
    Ukrainian  -> "Найновіший"
    Romanian   -> "Actializat"
    Vietnamese -> "Mới nhất"
    Czech      -> "Aktuální"
    Korean     -> "최신 버전"
    Hindi      -> "अप टू डेट"
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
    Chinese    -> "孤立包！"
    Swedish    -> "Föräldralös!"
    Esperanto  -> "Orfita!"
    Dutch      -> "Onteigend!"
    Ukrainian  -> "Осиротів!"
    Romanian   -> "Orfan!"
    Vietnamese -> "Gói lẻ!"
    Czech      -> "Opuštěno!"
    Korean     -> "관리되지 않는 패키지입니다!"
    Hindi      -> "अनाथ!"
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
    Dutch      -> "De pakketstatus is bewaard."
    Ukrainian  -> "Стан пакунків збережено."
    Romanian   -> "Stare de pachete salvată."
    Vietnamese -> "Đã lưu trạng thái gói."
    Czech      -> "Stav balíčku byl uložen."
    Korean     -> "패키지 상태가 저장되었습니다."
    Hindi      -> "सहेजे गए पैकेज की स्थिति."
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
    Chinese    -> "不存在给定降级版本的包："
    Swedish    -> "Den begärda nedgraderingen finns inte tillgänglig för:"
    Esperanto  -> "Petitajn malpromociajn versiojn ne estas disponebla de:"
    Dutch      -> "De verzochte afwaardeerversies zijn niet beschikbaar voor:"
    Ukrainian  -> "Запитані версії для відкату не доступні для:"
    Romanian   -> "Versiunea solicitată pentru retrogradare nu este disponibilă pentru:"
    Vietnamese -> "Không thể hạ cấp cho:"
    Czech      -> "Požadované nižší verze nejsou k dispozici pro:"
    Korean     -> "요청한 다운그레이드 버전은 다음 패키지에 사용할 수 없습니다:"
    Hindi      -> "अनुरोधित डाउनग्रेड संस्करण इसके लिए उपलब्ध नहीं है:"
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
    Chinese    -> "包状态未有保存。（使用 -B 保存当前状态）"
    Swedish    -> "Inga sparade tillstånd att återhämta. (Använd -B för att spara det nuvarande tillståndet)"
    Esperanto  -> "Ne konservitaj statoj restaŭros. (Uzu -B konservi la aktualan staton)"
    Dutch      -> "Er zijn geen bewaarde statussen om te herstellen. (ken -B toe om de huidige status te bewaren)"
    Ukrainian  -> "Немає збережених станів для відновлення. (Викоривуйте -B для збереження теперішнього стану)"
    Romanian   -> "Nu există vreo stare de recuperat. (Folosiți -B pentru a salva starea actuală)"
    Vietnamese -> "Không có trạng thái nào có thể lưu. (Dùng -B để lưu trạng thái hiện tại)"
    Czech      -> "Žádné uložené stavy k obnovení. (Pro uložení aktuálního stavu použijte -B)"
    Korean     -> "복원할 패키지 상태가 없습니다. (-B를 사용해 현재 상태를 저장)"
    Hindi      -> "कोई भी सहेजी गई स्थिति पुनर्स्थापित नहीं की जाएगी। (वर्तमान स्थिति को सहेजने के लिए -B का उपयोग करें)"
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
    Chinese    -> "没有需要变更的包。"
    Swedish    -> "Inga paket behöver ändras."
    Esperanto  -> "Ne pakaĵoj devas ŝanĝiĝi."
    Dutch      -> "Er zijn geen pakketten die aangepast moeten worden."
    Ukrainian  -> "Пакунки не потребують оновлення."
    Romanian   -> "Nu trebuie schimbat nici un pachet."
    Vietnamese -> "Không có gói nào cần thay đổi."
    Czech      -> "Žádné balíčky není třeba měnit."
    Korean     -> "패키지를 변경할 필요가 없습니다."
    Hindi      -> "किसी पैकेज को बदलने की आवश्यकता नहीं है।"
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
    Dutch      -> "Er kan niet worden vastgesteld met welk gebruikersaccount er gebouwd dient te worden."
    Ukrainian  -> "Не вдається визначити користувача, від імені якого буде проводитись збірка."
    Romanian   -> "Nu se poate determina cu care cont de utilizator să se compileze."
    Vietnamese -> "Không thể xác định tài khoản người dùng nào để build."
    Czech      -> "Nelze určit, se kterým uživatelským účtem se má provezt build."
    Korean     -> "사용자가 컴파일할 것인지 확일할 수 없습니다."
    Hindi      -> "यह निर्धारित नहीं कर सकता कि किस उपयोगकर्ता खाते से निर्माण किया जाए।"
    Chinese    -> "无法确定用于构建的用户。"
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
    Dutch      -> "Uw pacman.conf-bestand kan niet worden verwerkt."
    Ukrainian  -> "Не вдалось зрозуміти вміст файлу pacman.conf."
    Romanian   -> "Nu se poate analiza fișierul pacman.conf."
    Vietnamese -> "Không thể lấy dữ liệu từ tệp pacman.conf của bạn."
    Czech      -> "Nelze analyzovat soubor pacman.conf."
    Korean     -> "pacman.conf 파일 분석에 실패했습니다."
    Hindi      -> "आपकी pacman.conf फ़ाइल को पार्स करने में असमर्थ।"
    Chinese    -> "pacman.conf 文件无法解析。"
    _          -> "Unable to parse your pacman.conf file."

provides_1 :: PkgName -> Language -> Doc AnsiStyle
provides_1 (bt . pnName -> pro) = \case
    Polish     -> pro <+> "jest wymagany/a jako zależność, dostarczana przez wiele pakietów. Proszę wybrać jeden:"
    Arabic     -> ":مطلوب باعتباره تبعية ، والتي يتم توفيرها بواسطة حزم متعددة. رجاءا اختر واحدة" <+> pro
    Spanish    -> pro <+> "se requiere como una dependencia, que es proporcionada por múltiples paquetes. Por favor, seleccione uno:"
    Italian    -> pro <+> "è richiesto come dipendenza; si trova in molteplici pacchetti. Selezionarne uno:"
    Dutch      -> pro <+> "is vereist als afhankelijkheid, maar wordt door meerdere pakketten aangeleverd. Kies 1 pakket:"
    Ukrainian  -> pro <+> "потрібен як залежність, яка надається декількома пакунками. Оберіть один з них:"
    Romanian   -> pro <+> "este necesar ca dependență, care e provizionat de mai multe pachete. Selectați unul dintre ele:"
    Vietnamese -> pro <+> "là gói phụ thuộc, được cung cấp từ nhiều gói khác. Hãy chọn một:"
    Czech      -> pro <+> "je vyžadována jako závislost, kterou poskytuje několik balíčků. Prosím vyberte jeden:"
    Korean     -> pro <+> "는 종속성으로 필요하며 여러 패키지에서 제공됩니다. 다음 중 하나를 선택하십시오:"
    Hindi      -> pro <+> "एक निर्भरता के रूप में आवश्यक है, जो कई पैकेजों द्वारा प्रदान की जाती है। कृपया एक का चयन करें:"
    Chinese    -> "依赖项" <+> pro <+> "可由多个包提供。请选择："
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
    Chinese    -> "要编辑 " <> p <> " 的 PKGBUILD 文件吗？"
    Esperanto  -> "Ĉu vi volas redakti la PKGBUILD de " <> p <> "?"
    Dutch      -> "Wilt u het PKGBUILD-bestand van " <> p <> " bewerken?"
    Ukrainian  -> "Бажаєте відредагувати PKGBUILD для пакунку " <> p <> "?"
    Romanian   -> "Doriți să modificați PKGBUILD-ul pachetului " <> p <> "?"
    Vietnamese -> "Bạn có muốn chỉnh sửa PKGBUILD của " <> p <> "?"
    Czech      -> "Chcete upravit PKGBUILD z " <> p <> "?"
    Korean     -> p <> "의 PKGBUILD를 편집하시겠습니까?"
    Hindi      -> "क्या आप " <> p <> " के PKGBUILD को संपादित करना चाहेंगे?"
    _          -> "Would you like to edit the PKGBUILD of " <> p <> "?"

hotEdit_2 :: Language -> Doc AnsiStyle
hotEdit_2 = \case
  Polish     -> "Czy chcesz edytować plik .install?"
  Arabic     -> "؟.installهل تريد تعديل ملف اﻟ"
  Spanish    -> "¿Desea editar el archivo .install?"
  Ukrainian  -> "Бажаєте відредагувати файл .intall?"
  Romanian   -> "Doriți să modificați fișierul .install?"
  Vietnamese -> "Bạn có muốn chỉnh sửa tệp .install?"
  Czech      -> "Chcete upravit soubor .install?"
  Korean     -> ".install 파일을 수정하시겠습니까?"
  Hindi      -> "क्या आप .install फ़ाइल को संपादित करना चाहेंगे?"
  Dutch      -> "Wilt u het .install-bestand bewerken?"
  Chinese    -> "要编辑 .install 文件吗？"
  _          -> "Would you like to edit the .install file?"

hotEdit_3 :: FilePath -> Language -> Doc AnsiStyle
hotEdit_3 fp = \case
  Polish     -> "Czy chcesz edytować " <> pretty fp <> "?"
  Arabic     -> "؟" <> pretty fp <> " هل تريد التعديل"
  Spanish    -> "¿Desea editar " <> pretty fp <> "?"
  Ukrainian  -> "Бажаєте відредагувати " <> pretty fp <> "?"
  Romanian   -> "Doriți să modificați " <> pretty fp <> "?"
  Vietnamese -> "Bạn có muốn chỉnh sửa " <> pretty fp <> "?"
  Czech      -> "Chcete upravit " <> pretty fp <> "?"
  Korean     -> pretty fp <> "을(를) 수정하시겠습니까?"
  Hindi      -> "क्या आप " <> pretty fp <> " को संपादित करना चाहेंगे?"
  Dutch      -> "Wilt u " <> pretty fp <> "bewerken?"
  Chinese    -> "要编辑 " <> pretty fp <> " 吗？"
  _          -> "Would you like to edit " <> pretty fp <> "?"

------------------------------
-- Pkgbuild Security functions
------------------------------
security_1 :: PkgName -> Language -> Doc AnsiStyle
security_1 (PkgName p) = \case
  Polish     -> "PKGBUILD dla" <+> bt p <+> "był zbyt zawiły do odczytania - może zawierać złośliwy kod."
  Arabic     -> ".كان معقدا جدا للتحليل - يمكن أن يكون تعتيم مشوش للشفرة" <+> bt p <+> "تبع PKGBUILDاﻟ"
  Spanish    -> "El PKGBUILD de" <+> bt p <+> "era demasiado complejo de analizar - puede estar ofuscando código malicioso."
  Italian    -> "Il PKGBUILD di" <+> bt p <+> "è troppo complesso per essere analizzato - è possibile che stia offuscando codice malevolo."
  Dutch      -> "Het PKGBUILD-bestand van" <+> bt p <+> " was te complex om te verwerken. Het bestand bevat mogelijk verborgen schadelijke code."
  Ukrainian  -> "PKGBUILD пакунку" <+> bt p <+> "був надто складним для аналізу - він може містити замаскований шкідливий код."
  Romanian   -> "PKGBUILD-ul pachetului" <+> bt p <+> "este prea complicat de analizat - ar putea sa acopere cod rău intenționat."
  Vietnamese -> "PKGBUILD của" <+> bt p <+> "quá khó để đọc - nó có thể chứa đoạn mã nguy hiểm."
  Czech      -> "PKGBUILD z" <+> bt p <+> "byl příliš složitý na analýzu - může obsahovat matoucí škodlivý kód."
  Korean     -> "이 " <+> bt p <+> " PKGBUILD는 너무 복잡하여 분석할 수 없습니다 - 난독화된 코드일 수 있습니다."
  Hindi      -> bt p <+> "का PKGBUILD पार्स करने के लिए बहुत जटिल था - यह दुर्भावनापूर्ण कोड को अस्पष्ट कर सकता है।"
  Chinese    -> bt p <+> "的 PKGBUILD 文件太复杂了，无法分析 - 其中可能隐藏恶意代码。"
  _          -> "The PKGBUILD of" <+> bt p <+> "was too complex to parse - it may be obfuscating malicious code."

security_2 :: Text -> Language -> Doc AnsiStyle
security_2 (bt -> t) = \case
  Polish    -> t <+> "może zostać użyty do pobrania arbitralnych skryptów, które nie są śledzone przez ten PKGBUILD."
  Arabic     -> ".هذه PKGBUILDيمكن ان يحمل ملفات عشروتىية ليست مسجلة باﻟ" <+> t
  Spanish    -> t <+> "se puede usar para descargar scripts arbitrarios que este PKGBUILD no rastrea."
  Italian    -> t <+> "può essere usato per scaricare script arbitrari non tracciati da questo PKGBUILD."
  Dutch      -> t <+> "kan gebruikt worden om willekeurige scripts te downloaden die niet worden bijgehouden door dit PKGBUILD-bestand."
  Ukrainian  -> t <+> "може завантажувати довільні скріпти, які не відстежуються цим PKGBUILD."
  Romanian   -> t <+> "se poate folosi pentru a descărca scripturi neurmărite de acest PKGBUILD."
  Vietnamese -> t <+> "có thể dùng để tải xuống các tập lệnh sẽ không được kiểm soát bởi PKGBUILD."
  Czech      -> t <+> "lze použít ke stažení libovolných skriptů, které tento PKGBUILD nesleduje."
  Korean     -> t <+> "은(는) PKGBUILD에서 추적되지 않는 임의 스크립트를 다운로드하는 데 사용할 수 있습니다."
  Hindi      -> t <+> "का उपयोग उन मनमानी स्क्रिप्ट को डाउनलोड करने के लिए किया जा सकता है जिन्हें इस PKGBUILD द्वारा ट्रैक नहीं किया जाता है।"
  Chinese    -> t <+> "可用于下载任意脚本，且不被 PKGBUILD 跟踪。"
  _          -> t <+> "can be used to download arbitrary scripts that aren't tracked by this PKGBUILD."

security_3 :: Text -> Language -> Doc AnsiStyle
security_3 (bt -> t) = \case
  Polish     -> t <+> "może zostać użyty do wykonywania arbitralnego kodu, który nie jest śledzony przez ten PKGBUILD."
  Arabic     -> ".هذه PKGBUILDيمكن ان يستعمل ملفات عشروتىية ليست مسجلة باﻟ" <+> t
  Spanish    -> t <+> "se puede usar para ejecutar código arbitrario que este PKGBUILD no rastrea."
  Italian    -> t <+> "può essere usato per eseguire codice arbitrario non tracciato da questo PKGBUILD."
  Dutch      -> t <+> "kan gebruikt worden om willekeurige code uit te voeren die niet worden bijgehouden door dit PKGBUILD-bestand."
  Ukrainian  -> t <+> "може виконувати довільний код, який не відстежуються цим PKGBUILD."
  Romanian   -> t <+> "se poate folosi pentru a executa cod arbitrar neurmărit de acest PKGBUILD."
  Vietnamese -> t <+> "có thể dùng để chạy các đoạn mã không được kiểm soát bởi PKGBUILD. "
  Czech      -> t <+> "lze použít ke spuštění libovolného kódu, který tento PKGBUILD nesleduje."
  Korean     -> t <+> "은(는) PKGBUILD에서 추적되지 않는 임의 스크립트를 실행하는 데 사용할 수 있습니다."
  Hindi      -> t <+> "इस PKGBUILD द्वारा ट्रैक नहीं किए गए मनमाने कोड को निष्पादित करने के लिए उपयोग किया जा सकता है।"
  Chinese    -> t <+> "可用于执行任意代码，且不被 PKGBUILD 跟踪。"
  _          -> t <+> "can be used to execute arbitrary code not tracked by this PKGBUILD."

security_4 :: Text -> Language -> Doc AnsiStyle
security_4 (bt -> t) = \case
  Polish     -> t <+> "wskazuje na to, że ktoś może próbować uzyskać dostęp root'a do twojej maszyny."
  Arabic     -> ".تشير ان شخصا ما يحاول الوصول الى قوت المسؤول على جهازك" <+> t
  Spanish    -> t <+> "indica que alguien puede estar intentando obtener acceso de root a su máquina."
  Italian    -> t <+> "indica che forse qualcuno sta cercando di ottenere accesso alla tua macchina come root."
  Dutch      -> t <+> "geeft aan dat iemand mogelijk roottoegang tot uw apparaat probeert te krijgen."
  Ukrainian  -> t <+> "вказує на те, що хтось може спробувати отримати доступ root до вашої машини."
  Romanian   -> t <+> "indică că cineva are putea încerca să obțină acces root asupra mașinăria dumneavoastră."
  Vietnamese -> t <+> "được xác định là có người đang có giành quyền truy cập vào root trên thiết bị của bạn."
  Czech      -> t <+> "znamená, že se někdo možná pokouší získat přístup root k vašemu počítači."
  Korean     -> t <+> "은(는) 누군가 컴퓨터에 대한 루트 액세스 권한을 얻으려고 할 수 있음을 나타냅니다."
  Hindi      -> t <+> "यह दर्शाता है कि कोई व्यक्ति आपकी मशीन तक रूट पहुंच प्राप्त करने का प्रयास कर रहा है।"
  Chinese    -> t <+> "可用于获取 root 访问权限。"
  _          -> t <+> "indicates that someone may be trying to gain root access to your machine."

security_5 :: PkgName -> Language -> Doc AnsiStyle
security_5 (PkgName p) = \case
  Polish     -> "UWAGA: PKGBUILD dla " <+> bt p <+> "zawiera wyrażenia bash znajdujące się na czarnej liście."
  Arabic     -> ".في القائمة السودء bash في تعبيرات" <+> bt p <+> "باجل PKGBUILDتحذير: اﻟ"
  Spanish    -> "ADVERTENCIA: El PKGBUILD de" <+> bt p <+> "contiene expresiones bash en la lista negra."
  Italian    -> "ATTENZIONE: Il PKGBUILD di" <+> bt p <+> "contiene espressioni bash presenti nella lista nera."
  Dutch      -> "WAARSCHUWING: De PKGBUILD van" <+> bt p <+> "bevat bash-uitdrukkingen die op de zwarte lijst staan."
  Ukrainian  -> "УВАГА! PKGBUILD пакунку" <+> bt p <+> "містить вирази bash, які занесені в чорний список."
  Romanian   -> "ATENȚIE! PKGBUILD-ul pachetului" <+> bt p <+> "conține expresii de bash pe lista neagră."
  Vietnamese -> "CẢNH BÁO: PKGBUILD của" <+> bt p <+> "chứa những câu lệnh bash nguy hiểm."
  Czech      -> "VAROVÁNÍ: PKGBUILD z" <+> bt p <+> "obsahuje zakazany bash výrazy"
  Korean     -> "경고: " <+> bt p <+> "의 PKGBUILD에는 블랙리스트에 있는 bash 식이 포함되어 있습니다."
  Hindi      -> "चेतावनी: PKGBUILD" <+> bt p <+> "में ब्लैकलिस्टेड बैश एक्सप्रेशन शामिल हैं।"
  Chinese    -> "警告：" <+> bt p <+> "的 PKGBUILD 中包含了黑名单 bash 表达式。"
  _          -> "WARNING: The PKGBUILD of" <+> bt p <+> "contains blacklisted bash expressions."

security_6 :: Language -> Doc AnsiStyle
security_6 = \case
  Polish     -> "Czy chcesz zakończyć proces budowania?"
  Arabic     -> "هل تريد اقاف البناء؟"
  Spanish    -> "¿Desea salir del proceso de compilación?"
  Italian    -> "Terminare la compilazione?"
  Dutch      -> "Wilt u het bouwproces afbreken?"
  Ukrainian  -> "Бажаєте скасувати процес збірки?"
  Romanian   -> "Doriți anula procesul de compilare?"
  Vietnamese -> "Bạn có muốn dừng quá trình build?"
  Czech      -> "Přejete si ukončit build?"
  Korean     -> "빌드 프로세스를 종료하시겠습니까?"
  Hindi      -> "क्या आप निर्माण प्रक्रिया छोड़ना चाहते हैं?"
  Chinese    -> "是否要退出构建过程?"
  _          -> "Do you wish to quit the build process?"

security_7 :: Language -> Doc AnsiStyle
security_7 = \case
  Polish     -> "Anulowano dalsze przetwarzanie by uniknąć egzekucji potencjalnie złośliwego kodu bash"
  Arabic     -> ".الذي يحتمل أن يكون ضارا bash تم الغاء المعالجة الاضافيه لتجنب صدور"
  Spanish    -> "Se canceló el procesamiento posterior para evitar el código bash potencialmente malicioso."
  Italian    -> "Non saranno eseguite altre operazioni al fine di evitare l'esecuzione di codice bash potenzialmente malevolo."
  Dutch      -> "De verdere verwerking is afgebroken om het uitvoeren van potentieel schadelijke bash-code te voorkomen."
  Ukrainian  -> "Подальша установка скасована, щоб уникнути потенційно шкідливого коду bash."
  Romanian   -> "S-a cancelat procesarea ulterioară pentru a evita cod de bash potențial rău intenționat."
  Vietnamese -> "Hãy dừng những quá trình tiếp theo để ngắn đoạn mã bash nguy hiểm."
  Czech      -> "Další proces byl zrušen, aby se zabránilo potenciálně škodlivému bash kódu."
  Korean     -> "잠재적으로 악의적인 bash 코드를 방지하기 위해 추가 처리가 취소되었습니다."
  Hindi      -> "संभावित रूप से दुर्भावनापूर्ण बैश कोड से बचने के लिए आगे की प्रक्रिया रद्द कर दी गई।"
  Chinese    -> "已取消后续处理以避免潜在的恶意 bash 代码。"
  _          -> "Cancelled further processing to avoid potentially malicious bash code."

security_8 :: Text -> Language -> Doc AnsiStyle
security_8 (bt -> t) = \case
  Polish     -> t <+> "jest komendą bash zawartą w polach tablicy twojego PKGBUILD."
  Arabic     -> ".الخاص بك PKGBUILDمضمن بالحقول المصفوفة باﻟ bash امر" <+> t
  Spanish    -> t <+> "es un comando bash integrado en los campos de la matriz del PKGBUILD."
  Italian    -> t <+> "è un comando bash presente all'interno degli array del tuo PKGBUILD."
  Dutch      -> t <+> "is een bash-opdracht die is opgenomen in uw PKGBUILD-reeksvelden."
  Ukrainian  -> t <+> "- це команда bash, що вбудована в ваші поля масиву PKGBUILD"
  Romanian   -> t <+> "este o comandă bash integrată în matricele din PKGBUILD."
  Vietnamese -> t <+> "là lệnh bash được lồng trong mảng của PKGBUILD."
  Czech      -> t <+> "je bash příkaz vložený do polí pole PKGBUILD."
  Korean     -> t <+> "는 PKGBUILD 배열 필드에 표시된 bash 명령입니다."
  Hindi      -> t <+> "आपके PKGBUILD सरणी फ़ील्ड में इनलाइन एक बैश कमांड है।"
  Chinese    -> t <+> "存在于 PKGBUILD 数组中，似乎是 bash 命令。"
  _          -> t <+> "is a bash command inlined in your PKGBUILD array fields."

security_9 :: Text -> Language -> Doc AnsiStyle
security_9 (bt -> t) = \case
  Polish     -> t <+> "jest dziwną rzeczą w polach tablicy. Czy to bezpieczne?"
  Arabic     -> "شيء غريب ان يكون لديك في الحقول المصفوفة, هل هيا امن؟" <+> t
  Spanish    -> t <+> "es algo extraño para tener en sus campos de matriz. ¿Es seguro?"
  Italian    -> t <+> "è una cosa strana da trovare all'interno degli array. E' sicura?"
  Dutch      -> t <+> "is een vreemde eend in de bijt in uw reeksvelden. Is dit wel veilig?"
  Ukrainian  -> t <+> "- дивна річ в полях масиву. Це безпечно?"
  Romanian   -> t <+> "e ciudat să se afle în matricele dumneavoastră. Asta este sigur?"
  Vietnamese -> t <+> "là đoạn mã lạ trong mảng. Nó có an toàn không?"
  Czech      -> t <+> "je zvláštní věc mít v polích. Je to bezpečné?"
  Korean     -> t <+> "이것은 배열 안에서 볼 수 있는 이상한 것입니다. 안전합니까?"
  Hindi      -> t <+> "आपके सरणी फ़ील्ड में होना एक अजीब चीज़ है। क्या यह सुरक्षित है?"
  Chinese    -> t <+> "存在于数组字段，很怪。这安全吗？"
  _          -> t <+> "is a strange thing to have in your array fields. Is it safe?"

security_10 :: Text -> Language -> Doc AnsiStyle
security_10 (bt -> t) = \case
  Polish     -> t <+> "sugeruje, że ktoś próbował być sprytny używając zmiennych do ukrycia złośliwych komend."
  Arabic     -> ".يعني ان شخصا ما كان يحاول ان يكون ذكيا مع المتغيرات لاخفاء الاوامر الخبيثة" <+> t
  Spanish    -> t <+> "implica que alguien estaba tratando de ser astuto con las variables para ocultar comandos maliciosos."
  Italian    -> t <+> "implica che qualcuno stava trafficando con le variabili per nascondere comandi malevoli."
  Dutch      -> t <+> "impliceert dat iemand op slinkse wijze probeerde om te gaan met variabelen om schadelijke opdrachten te verbergen."
  Ukrainian  -> t <+> "означає, що хтось намагається обдурити змінними, щоб сховати небеспечні команди."
  Romanian   -> t <+> "implică că cineva încearcă să fie șmecher cu variabile pentru a ascunde comenzi rele intenționate."
  Vietnamese  -> t <+> "được xác định là có ai đó đang cố ẩn những câu lệnh nguy hiểm trong các biến."
  Czech      -> t <+> "naznačuje že se někdo snažil být chytrý s proměnnými, aby skryl škodlivé příkazy."
  Korean     -> t <+> "누군가 악의적인 명령을 숨기기 위해 변수를 교묘하게 다루려고 했다는 것을 암시합니다."
  Hindi      -> t <+> "का तात्पर्य यह है कि कोई व्यक्ति दुर्भावनापूर्ण आदेशों को छिपाने के लिए वेरिएबल्स के साथ चतुराई बरतने की कोशिश कर रहा था।"
  Chinese    -> t <+> "表明有人试图巧妙地利用变量来隐藏恶意命令。"
  _          -> t <+> "implies that someone was trying to be clever with variables to hide malicious commands."

security_11 :: Language -> Doc AnsiStyle
security_11 = \case
  Polish     -> "Ten PKGBUILD jest zbyt zawiły do odczytania - może ukrywać w sobie złośliwy kod."
  Arabic     -> ".كان معقدا جدا للتحليل - يمكن ان يخفي برنامج ضار PKGBUILDذلك اﻟ"
  Spanish    -> "Éste PKGBUILD es demasiado complejo para analizar, puede estar ofuscando código malicioso."
  Ukrainian  -> "Цей PKGBUILD був надто складним для аналізу - він може містити шкідливий код."
  Romanian   -> "Acel PKGBUILD este prea complicat de analizat - are putea ascunde cod rău intenționat."
  Vietnamese -> "Không thể đọc PKGBUILD - nó có thể chứa đoạn mã nguy hiểm."
  Czech      -> "Tento PKGBUILD je příliš složitý na to, aby jej bylo možné analyzovat/rezebrat – může obsahovat matoucí škodlivý kód."
  Korean     -> "이 PKGBUILD는 분석하기에 너무 복잡합니다 - 악성코드를 난독화하고 있을 수 있습니다."
  Hindi      -> "वह PKGBUILD पार्स करने के लिए बहुत जटिल है - यह दुर्भावनापूर्ण कोड को अस्पष्ट कर सकता है।"
  Dutch      -> "Deze PKGBUILD is te complex om te verwerken. Het bestand bevat mogelijk verborgen schadelijke code."
  Chinese    -> "PKGBUILD 文件太复杂了，无法分析 - 其中可能隐藏恶意代码。"
  _          -> "That PKGBUILD is too complex to parse - it may be obfuscating malicious code."

security_12 :: Language -> Doc AnsiStyle
security_12 = \case
  Polish     -> "Potencjalne luki w bezpieczeństwie wykryte w PKGBUILD"
  Arabic     -> ".PKGBUILDاحتمال وجود ثغرات امنية في اﻟ"
  Spanish    -> "Posibles vulnerabilidades de PKGBUILD detectadas."
  Ukrainian  -> "Потенційні вразливості знайдено в PKGBUILD."
  Romanian   -> "Vulnerabilități potențiale detectate în PKGBUILD."
  Vietnamese -> "Phát hiện lỗ hổng trong PKGBUILD."
  Czech      -> "Byla zjištěna potenciální bezpečnostní chyba v PKGBUILD."
  Korean     -> "잠재적인 PKGBUILD 취약점 발견됨."
  Hindi      -> "संभावित PKGBUILD कमजोरियों का पता चला।"
  Dutch      -> "Er zijn mogelijke kwetsbaarheden aangetroffen in het PKGBUILD-bestand."
  Chinese    -> "在 PKGBUILD 中检测到潜在的漏洞。"
  _          -> "Potential PKGBUILD vulnerabilities detected."

security_13 :: Word -> Language -> Doc AnsiStyle
security_13 (bt -> w) = \case
  Polish     -> "Sprawdzanie PKGBUILD" <+> w <+> "w poszukiwaniu luk w bezpieczeństwie..."
  Arabic     -> "...بحثا عن نقاط ضعف" <+> w <+> "تبع PKGBUILDتحقق اﻟ"
  Spanish    -> "Comprobando" <+> w <+> "PKGBUILDs por vulnerabilidades..."
  Ukrainian  -> "Перевіряємо" <+> w <+> "PKGBUILD-ів на вразливості..."
  Romanian   -> "Se verifică PKGBUILD-uri" <+> w <+> "pentru vulnerabilități..."
  Vietnamese -> "Tìm kiếm" <+> w <+> "lỗ hổng trong PKGBUILD..."
  Czech      -> "Kontrola bezpečnostních chyb v" <+> w <+> "PKGBUILD"
  Korean     -> w <+> "PKGBUILD 취약점 확인 중..."
  Hindi      -> "जांच की जा रही है" <+> w <+> "कमजोरियों के लिए PKGBUILDs..."
  Dutch      -> "Bezig met controleren van" <+> w <+> "PKGBUILDs op kwetsbaarheden…"
  Chinese    -> "检查 " <+> w <+> "PKGBUILD 中是否存在漏洞..."
  _          -> "Checking" <+> w <+> "PKGBUILDs for vulnerabilities..."

security_14 :: Language -> Doc AnsiStyle
security_14 = \case
  Polish     -> "Nie wykryto żadnych luk w bezpieczeństwie."
  Arabic     -> ".لا تم العثور على نقاط ضعف"
  Spanish    -> "No se detectaron vulnerabilidades."
  Ukrainian  -> "Ніяких вразливостей не було знайдено."
  Romanian   -> "Nu s-a găsit nici o vulnerabilitate."
  Vietnamese -> "Không tìm thấy lỗ hổng."
  Czech      -> "Nebyly nalezeny žádné bezpečnostní chyby."
  Korean     -> "발견된 취약점이 없습니다."
  Hindi      -> "कोई कमज़ोरी नहीं पाई गई।"
  Dutch      -> "Er zijn geen kwetsbaarheden aangetroffen."
  Chinese    -> "未发现漏洞。"
  _          -> "No vulnerabilities detected."

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
    Czech      -> "[A,n]"
    Hindi      -> "[य/न]"
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
    Czech      -> ["a", "ano"]
    Hindi      -> ["य", "हाँ"]
    _          -> ["y", "yes"]

----------------------
-- Pluralization rules
----------------------
pluralRussian :: Integral n => a -> a -> a -> n -> a
pluralRussian singular plural1 plural2 n | n % 10 == 1 && n % 100 /= 11 = singular
                                         | n % 10 `elem` [2, 3, 4] = plural1
                                         | otherwise = plural2
