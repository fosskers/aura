language-name = Українська

# AUR Packages (-A)
A-install-deps = Визначення залежностей...
A-install-repo-pkgs = Залежності репозиторія:
A-install-aur-pkgs = Пакунки AUR:
A-install-path-comp = Не вдалось витягнути останній компонент: { $path }
A-install-ignored = { $file } позначено як "проігнорований". Всерівно встановити?

A-build-prep = Підготовка тек для збірки
A-build-pkg = Збираємо { $pkg }...
A-build-diff = Показати різницю файлів збірки?
A-build-hotedit-pkgbuild = Редагувати PKGBUILD?
A-build-hotedit-install = Бажаєте відредагувати файл .install?
A-build-fail = Збірка пакету не вдалась:
A-build-e-pkgctl = Збірка всередині ізольованогосередовища не вдалась.
A-build-e-makepkg = Сталась помилка makepkg.
A-build-e-edit = Не вдалось редагувати: { $file }
A-build-e-tarball = Не вдалось перемістити: { $file }
A-build-e-filename = Не вдалось витягти назву файлу з: { $file }
A-build-e-copies = Не вдалось скопіювати файли збірки.
A-build-e-perm = Не вдалось вказати дозволи для { $dir }
A-build-pkglist = Не вдалося визначити шляхи виводу makepkg з: { $dir }
A-build-pull = Не вдалося використати 'git pull' для отримання останніх оновлень.
A-build-continue = Ви все одно бажаєте продовжити?

A-i-repo = Репозиторій
A-i-version = Версія
A-i-status = Статус AUR
A-i-maintainer = Супроводжувач
A-i-proj-url = Адреса проекту
A-i-aur-url = Адреса AUR
A-i-license = Ліцензія
A-i-group = Групи
# A-i-provides = Надає
A-i-depends = Залежить від
A-i-make = Зібрати залежності
A-i-opt = Необов'язкові
A-i-check = Перевірити залежності
A-i-votes = Голоси
A-i-pop = Популярність
A-i-desc = Опис
A-i-keywords = Ключові слова
A-i-submitted = Затверджено
A-i-updated = Оновлено

A-u-fetch-info = Збираємо інформацію про пакунок...
A-u-comparing = Порівнюємо версії пакунків...
A-u-no-upgrades = Пакунки AUR не потребують оновлення.
A-u-to-upgrade = Пакунки AUR, готові для оновлення:
A-u-git = Пакети VCS які будуть перебудовані:

A-w = Клонування { $package }...

# A-y-refreshing = Оновлення локальних клонів відомих пакетів AUR...
A-y-pulling = Завантаження останніх змін

# Snapshots (-B)
B-saved = Стан пакунків збережено.
B-clean = Видалити застарілі знімки?
B-none = Знімків не знайдено.
B-select = Виберіть знімок для відновлення:

# Cache (-C)
C-size = Поточний розмір кешу: { $size }
C-b-file = { $target } вже існує і не є каталогом.
C-b-nonempty = Ціль { $target } існує але вона не пуста!
C-b-target = Зберігаємо резервну копію до { $target }
C-b-curr = Не вдалось прочитати поточну теку.

C-i-latest = Останній
C-i-created = Створено
C-i-installed = Встановлено
C-i-sig = Підпис
C-i-size = Розмір архіву
C-i-avail = Доступні версії

C-c-keep = { $pkgs } версія кожного пакунку залишиться недоторканою. Все інше буде видалено.
C-c-freed = { $bytes } звільнилось.

C-downgrade-which = Яку версію пакунку { $pkg } ви бажаєте?

C-y-no-work = Кеш пакетів синхронізовано.
C-t-invalids = Видалення недійсних архівів пакетів.

# Logs (-L)
L-first = Перше встановлення
L-upgrades = Оновлення
L-recent = Останні дії
L-search-err = Пошук у ваших журналах за допомогою { $cmd } не вдався.
L-view-err = Не вдалось відкрити ваш лог ALPM.

# Opening Pages (open)
open-err = Не вдалось відкрити { $url }.

# System Statistics (stats)
stats-local = Не вдалося завантажити мовні дані.
stats-host = Хост
stats-user = Користувач
stats-distro = Розповсюдження
stats-editor = Редактор
stats-pkgs = Встановлені пакети
stats-aura-cache = Кеш пакетів Aura
stats-pacman-cache = Кеш пакетів Pacman
stats-aura-build = Кеш збірки Aura
stats-tmp = Тека /tmp

# System Validation (check)
check-start = Перевірка вашої системи.
check-missing-exec = Виправлення: Будьласка встановіть { $exec } та переконайтесь що він у вашій PATH.
check-env = Середовище
check-env-editor = Змінна EDITOR встановлена?
check-env-editor-exec = Значення EDITOR ({ $exec }) може виконуватись?
check-env-editor-vi = Запасний редактор vi може виконуватись?
check-env-exec = { $exec } встановлено та може виконуватись?
check-env-lang = { $cmd } містить значення LANG? ({ $lang })
check-env-lang-fix = Оновіть ваш { $file } що б включити { $lang }.
check-env-lang-fix2 = Визначте змінну LANG!
check-env-lang-known = Aura перекладено на вашу мову?
check-env-java-bin = Інструментарій Java встановлено?
check-env-java-bin-fix = Запропоновано встановити { $pkg }.
check-env-java-set = Середовище Java встановлено?
check-env-java-set-fix = Перегляньте { $cmd }.
check-pconf = Конфігурація Pacman (/etc/pacman.conf)
check-pconf-par = Паралельне завантаження увімкнено?
check-pconf-par-fix = { $setting } вимкнено, або встановлено на 1. Встановіть { $set } для швидшого завантаження пакетів.
check-pconf-ignores = Немає пакетів, що перекриваються та ігноруються?
check-pconf-ignores-fix = Ці пакети ігноруються в pacman.conf та aura.toml: { $pkgs }
check-pconf-pacnew = Всі файли .pacnew враховано?
check-pconf-pacnew-broken = Помилка: Виклик { $fd } завершився повним провалом.

check-pconf-pacnew-old = { $path } є старшим за його .pacnew на { $days ->
    [один] 1 день.
   *[багато] {$days} днів.
 }

check-aconf = Конфігурація Aura
check-aconf-aura-exists = Файл конфігурації Aura існує?
check-aconf-aura-exists-fix = Виправлення: перевірте { $cmd }
check-aconf-aura-parse = Файл налаштувань Aura можливо зчитати?
check-aconf-old-dirs = Старих тек Aura немає?
check-aconf-old-conf = Старих файлів конфігурації Aura немає?
check-mconf = Налаштування Makepkg ({ $path })
check-mconf-packager = Змінну PACKAGER встановлено?
check-mconf-packager-fix = Виправлення: Встановити { $cmd } у { $path }
check-snapshots = Знімки пакетів
check-snapshot-usable = Всі знімки мають відповідні архіви?
check-snapshot-usable-fix = Виправлення: Ви можете видалити застарілі абонеробочі знімки за допомогою { $command }
check-cache = Кеш архівів пакетів
check-cache-exists = Всі зазначені кеші існують?
check-cache-tarballs = Всі архіви дійсні?
check-cache-tarballs-fix = Fix: Ви можете видалити недійсні архіви за допомогою { $command }
check-cache-missing = Кожен встановлений офіційний пакет має архів?
check-cache-missing-fix = Fix: Ви можете завантажити відсутні офіційні архіви за допомогою { $command }
check-cache-missing-for = Кожен встановлений пакет AUR має архів?
check-cache-missing-for-fix = Виправлення: Перегляньте відсутні пакети за допомогою { $cmd } та перевстановити їх вручну.
check-pkgs = Стан пакету
check-pkgs-old = Усі явно встановлені пакети, що не входять до складу залежностей, оновлені?
check-pkgs-old-warn = { $pkg } востаннє оновлено { $days } тому.
check-pkgs-empty = All package clones are populated?
check-pkgs-empty-fix = Виправлення: Видаліть наступні каталоги.

# Thanks
thanks-you = Дякую вам за використання Aura.
thanks-colin = Aura від Colin Woodbury, 2012 - 2024
thanks-pacman = Подяка командам Pacman та Arch Linux за створення міцної основи.
thanks-everyone = Подяка вкладникам, жертводавцям та Користувачам Aura.
thanks-logo = Логотип Aura від Cristiano Vitorino.
thanks-translators = Aura переклали:

# Configuration (conf)
conf-toml-err = Не вдалося серіалізувати поточну конфігурацію.

# Dependencies (deps)
deps-io = Не вдалося створити образ залежностей.

# Runtime Environment
env-missing-editor = Наданий EDITOR не знаходиться в PATH.
env-pconf = Не вдалось зрозуміти вміст файлу pacman.conf.

# Pacman Calls
pacman-external = Виклик функції pacman не вдався.
pacman-u = Не вдалось викликати pacman -U.
pacman-s = Не вдалось викликати pacman -S.
pacman-misc = Виклик pacman повернув ненульове значення.

# Aura-specific Directories
dir-mkdir = Не вдалось створити теку: { $dir }.
dir-home = Неможливо визначити теку конфігурації Aura.
dir-cache = Неможливо визначити теку кешу Aura.

# Dependency Resolution
dep-exist = Пакет { $pkg } не існує.
dep-exist-par = Залежнісь { $pkg } від { $par } не існує.
dep-graph = Графік залежностей мав певні порушення у структурі.
dep-cycle = Виявлено цикл залежностей: { $cycle }
dep-multi = Під час визначення залежностей сталося кілька помилок.

# Git Operations
git-diff = Помилка під час виконання команди git diff для: { $file }
git-hash = Не вдалося зчитати хеш Git у Rust.
git-pull = Виклик git pull не вдався: { $dir }
git-clone = Виклик git clone не вдався: { $dir }
git-io = Виклик git провалився.

# Faur Calls
faur-fetch = Звернення до сервера метаданих провалилось: { $pkg }
faur-unknown = Невідомий пакет: { $pkg }
faur-too-many = Faur надав більше результатів, ніж очікувалося: { $pkg }

# Common Errors
err-alpm = Не вдалося відкрити дескриптор ALPM.
err-config-path = Не вдалося визначити шлях до файлу конфігурації Aura.
err-curl = Транзакція CURL завершилася невдало: { $err }
err-file-del = Не вдалось видалити: { $file }
err-file-open = Не вдалося відкрити дескриптор файлу для: { $file }
err-file-write = Не вдалось записати файл: { $file }
err-json-decode = Не вдалося розшифрувати JSON із: { $url }
err-json-write = Не вдалося записати JSON у: { $file }
err-mutex = Мутекс був «отруєний».
err-pool-create = Не вдалося створити пул з'єднань ALPM.
err-pool-get = Не вдалося отримати дескриптор ALPM із пулу з'єднань.
err-read-dir = Не вдалось прочитати теку: { $dir }
err-srcinfo = Не вдалося проаналізувати файл .SRCINFO: { $file }
err-sudo = Запускати Aura з sudo не необов'язково.
err-time-conv = Не вдалося перетворити часовий штамп.
err-time-format = Не вдалося відформатувати рядок часу.
err-user-input = Не вдалося отримати введені користувачем дані.
err-utf8 = Помилка під час перетворення в формат UTF-8.
err-write = Не вдалося записати дані у stdout.

# Common Fields
common-yes = Так
common-no = Ні
common-name = Ім'я
common-done = Готово.
common-no-packages = Пакет не вказано.
common-no-work = Нічого робити.
common-cancelled = Дія скасована.
common-replace = Ви можете видалити { $old } на користь { $new }.

# Misc.
proceed = Продовжити?
proceed-affirmative = y
proceed-affirmative-alt = Y
proceed-negative = n
