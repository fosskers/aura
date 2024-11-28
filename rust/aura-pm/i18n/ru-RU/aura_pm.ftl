language-name = Русский

# AUR Packages (-A)
A-install-deps = Определение зависимостей...
A-install-repo-pkgs = Зависимости из репозитория:
A-install-aur-pkgs = Пакеты AUR:
A-install-path-comp = Не удалось извлечь последний компонент из: { $path }
A-install-ignored = { $file } помечен как "игнорируемый". Установить все равно?

A-build-prep = Подготовка каталогов сборки...
A-build-pkg = Сборка { $pkg }...
A-build-diff = Показать различия в файлах сборки?
A-build-hotedit-pkgbuild = Редактировать PKGBUILD?
A-build-hotedit-install = Редактировать файл .install?
A-build-fail = Сборка пакета не удалась, причина:
A-build-e-pkgctl = Сборка в изолированной chroot-среде не удалась.
A-build-e-makepkg = makepkg не удалось.
A-build-e-edit = Не удалось отредактировать: { $file }
A-build-e-tarball = Не удалось переместить: { $file }
A-build-e-filename = Не удалось извлечь имя файла из: { $file }
A-build-e-copies = Не удалось скопировать файлы сборки.
A-build-e-perm = Не удалось установить права доступа для файла: { $dir }
A-build-pkglist = Не удалось определить пути вывода makepkg из: { $dir }
A-build-pull = Не удалось получить последние коммиты - возможно, вы собираете старую версию!
A-build-continue = Продолжить сборку других пакетов?

A-i-repo = Репозиторий
A-i-version = Версия
A-i-status = Статус AUR
A-i-maintainer = Сопровождающий
A-i-proj-url = URL проекта
A-i-aur-url = AUR URL
A-i-license = Лицензия
A-i-group = Группы
A-i-provides = Предоставляет
A-i-depends = Зависит от
A-i-make = Зависимости сборки
A-i-opt = Необязательные зависимости
A-i-check = Зависимости проверки
A-i-votes = Голоса
A-i-pop = Популярность
A-i-desc = Описание
A-i-keywords = Ключевые слова
A-i-submitted = Отправлено
A-i-updated = Обновлено

A-u-fetch-info = Получение информации о пакете...
A-u-comparing = Сравнение версий пакетов...
A-u-no-upgrades = Обновление пакетов из AUR не требуется.
A-u-to-upgrade = Пакеты AUR для обновления:
A-u-git = VCS пакеты для пересборки:

A-w = Клонирование { $package }...

A-y-refreshing = Обновление локальных клонов известных пакетов AUR...
A-y-pulling = Получение последних коммитов

# Snapshots (-B)
B-saved = Состояние пакета сохранено.
B-clean = Удалить устаревшие снимки?
B-none = Нет доступных снимков.
B-select = Выберите снимок для восстановления:

# Cache (-C)
C-size = Текущий размер кэша: { $size }

C-b-file = { $target } уже существует и не является каталогом.
C-b-nonempty = Цель { $target } существует, но не пуста!
C-b-target = Резервное копирование кэша в { $target }
C-b-curr = Не удалось прочитать текущий каталог.

C-i-latest = Последняя версия
C-i-created = Создано
C-i-installed = установлено
C-i-sig = Подпись
C-i-size = Размер архива
C-i-avail = Доступные версии

C-c-keep = { $pkgs -> 
    [one] Будет сохранён 1 файл
    [few] Будут сохранены {$pkgs} файла
   *[many] Будут сохранены {$pkgs} файлов
} каждого пакета. Остальные будут удалены.
C-c-freed = Освобождено { $bytes }.

C-downgrade-which = Какую версию { $pkg } вы хотите?

C-y-no-work = Кэш пакетов уже синхронизирован.
C-t-invalids = Удаление недействительных архивов пакетов.

# Logs (-L)
L-first = Первая установка
L-upgrades = Обновления
L-recent = Недавние действия
L-search-err = Поиск в журналах с помощью { $cmd } не удался.
L-view-err = Не удалось открыть журнал ALPM.

# Opening Pages (open)
open-err = Не удалось открыть { $url }.

# System Statistics (stats)
stats-local = Не удалось загрузить языковые данные.
stats-host = Хост
stats-user = Пользователь
stats-distro = Дистрибутив
stats-editor = Редактор
stats-pkgs = Установленные пакеты
stats-aura-cache = Кэш пакетов Aura
stats-pacman-cache = Кэш пакетов Pacman
stats-aura-build = Кэш сборки Aura
stats-tmp = Каталог /tmp

# System Validation (check)
check-start = Проверка системы.
check-missing-exec = Исправление: Пожалуйста, установите { $exec } и/или убедитесь, что он находится в вашем PATH.
check-env = Окружение
check-env-editor = Переменная EDITOR установлена?
check-env-editor-exec = Значение EDITOR ({ $exec }) исполняемо?
check-env-editor-vi = Резервный редактор vi исполняем?
check-env-exec = { $exec } установлен и исполняем?
check-env-lang = { $cmd } содержит значение LANG? ({ $lang })
check-env-lang-fix = Исправление: Обновите ваш { $file }, чтобы включить { $lang }.
check-env-lang-fix2 = Исправление: Установите переменную LANG!
check-env-lang-known = Aura локализована для вашего LANG?
check-env-java-bin = Инструменты Java установлены?
check-env-java-bin-fix = Исправление: Рекомендуется установить { $pkg }.
check-env-java-set = Окружение Java настроено?
check-env-java-set-fix = Исправление: См. { $cmd }.
check-pconf = Конфигурация Pacman (/etc/pacman.conf)
check-pconf-par = Параллельная загрузка активирована?
check-pconf-par-fix = Исправление: { $setting } выключен или установлен в 1. Установите { $set } для более быстрой загрузки архивов.
check-pconf-ignores = Нет перекрывающихся игнорируемых пакетов?
check-pconf-ignores-fix = Следующие пакеты игнорируются как в pacman.conf, так и в aura.toml: { $pkgs }
check-pconf-pacnew = Все файлы .pacnew учтены?
check-pconf-pacnew-broken = Ошибка: Вызов { $fd } полностью провалился.

check-pconf-pacnew-old = { $path } старше своего .pacnew на { $days ->
    [one] 1 день.
    [few] {$days} дня.
   *[many] {$days} дней.
}

check-aconf = Конфигурация Aura
check-aconf-aura-exists = Файл конфигурации Aura существует?
check-aconf-aura-exists-fix = Исправление: Рассмотрите { $cmd }
check-aconf-aura-parse = Файл конфигурации Aura может быть обработан?
check-aconf-old-dirs = Нет старых каталогов Aura?
check-aconf-old-conf = Нет старых файлов конфигурации Aura?
check-mconf = Конфигурация Makepkg ({ $path })
check-mconf-packager = PACKAGER установлен?
check-mconf-packager-fix = Исправление: Установите { $cmd } в { $path }
check-snapshots = Снимки пакетов
check-snapshot-usable = Все снимки имеют соответствующие архивы?
check-snapshot-usable-fix = Исправление: Вы можете удалить старые/неиспользуемые снимки с помощью { $command }
check-cache = Кэш архивов пакетов
check-cache-exists = Все указанные кэши существуют?
check-cache-tarballs = Все архивы действительны?
check-cache-tarballs-fix = Исправление: Вы можете удалить недействительные архивы с помощью { $command }
check-cache-missing = Для каждого установленного официального пакета есть архив?
check-cache-missing-fix = Исправление: Вы можете загрузить отсутствующие официальные архивы с помощью { $command }
check-cache-missing-for = Для каждого установленного пакета AUR есть архив?
check-cache-missing-for-fix = Исправление: Просмотрите отсутствующие пакеты с помощью { $cmd } и переустановите их вручную.
check-pkgs = Статус пакетов
check-pkgs-old = Все явно установленные пакеты, не являющиеся зависимостями, обновлены?
check-pkgs-old-warn = { $pkg } был последний раз обновлен { $days ->
    [one] 1 день
    [few] {$days} дня
   *[many] {$days} дней
} назад.
check-pkgs-empty = Все клоны пакетов заполнены?
check-pkgs-empty-fix = Исправление: Удалите следующие каталоги.

# Thanks
thanks-you = Спасибо за использование Aura.
thanks-colin = Aura от Colin Woodbury, 2012 - 2024
thanks-pacman = Спасибо командам Pacman и Arch Linux за предоставление прочной основы.
thanks-everyone = Спасибо всем, кто внес свой вклад в Aura, пожертвовал и пользовался ею.
thanks-logo = Логотип Aura от Cristiano Vitorino.
thanks-translators = Aura локализована:

# Configuration (conf)
conf-toml-err = Не удалось сериализовать текущую конфигурацию.

# Dependencies (deps)
deps-io = Не удалось сгенерировать изображение зависимостей.

# Runtime Environment
env-missing-editor = Указанный EDITOR не найден в PATH.
env-pconf = Не удалось разобрать файл pacman.conf.

# Pacman Calls
pacman-external = Вызов pacman полностью провалился.
pacman-u = Вызов pacman -U не удался.
pacman-s = Вызов pacman -S не удался.
pacman-misc = Вызов pacman вернул ненулевой код завершения.

# Aura-specific Directories
dir-mkdir = Не удалось создать каталог: { $dir }.
dir-home = Не удалось определить конфигурационный каталог Aura.
dir-cache = Не удалось определить каталог кэша Aura.

# Dependency Resolution
dep-exist = Пакет { $pkg } не существует.
dep-exist-par = Зависимость { $pkg } от { $par } не существует.
dep-graph = Граф зависимостей каким-то образом был сформирован неправильно.
dep-cycle = Обнаружен цикл зависимостей: { $cycle }
dep-multi = Возникло несколько ошибок во время разрешения зависимостей.

# Git Operations
git-diff = git diff не удался для: { $file }
git-hash = Чтение хэша git в Rust не удалось.
git-pull = git pull не удался: { $dir }
git-clone = git clone не удался: { $dir }
git-io = Вызов git каким-то образом не удался.

# Faur Calls
faur-fetch = Вызов сервера метаданных полностью провалился: { $pkg }
faur-unknown = Неизвестный пакет: { $pkg }
faur-too-many = Faur вернул больше результатов, чем ожидалось: { $pkg }

# Common Errors
err-alpm = Не удалось открыть дескриптор ALPM.
err-config-path = Не удалось определить путь к файлу конфигурации Aura.
err-curl = Транзакция CURL не удалась: { $err }
err-file-del = Не удалось удалить: { $file }
err-file-open = Не удалось открыть дескриптор файла: { $file }
err-file-write = Не удалось записать файл: { $file }
err-json-decode = Не удалось декодировать JSON из: { $url }
err-json-write = Не удалось записать JSON в: { $file }
err-mutex = Mutex был отравлен.
err-pool-create = Не удалось создать пул подключений ALPM.
err-pool-get = Не удалось получить дескриптор ALPM из пула подключений.
err-read-dir = Не удалось прочитать каталог: { $dir }
err-srcinfo = Не удалось разобрать .SRCINFO: { $file }
err-sudo = Запуск Aura с sudo не требуется.
err-time-conv = Не удалось преобразовать временную метку.
err-time-format = Не удалось отформатировать временную строку.
err-user-input = Не удалось получить ввод пользователя.
err-utf8 = Преобразование UTF-8 не удалось.
err-write = Каким-то образом не удалось записать в stdout.

# Common Fields
common-yes = Да
common-no = Нет
common-name = Имя
common-done = Готово.
common-no-packages = Не указаны пакеты.
common-no-work = Нечего делать.
common-cancelled = Действие отменено.
common-replace = Вы можете удалить { $old } в пользу { $new }.

# Misc.
proceed = Продолжить?
proceed-affirmative = д
proceed-affirmative-alt = Д
proceed-negative = н
