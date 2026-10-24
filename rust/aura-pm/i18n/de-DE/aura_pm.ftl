language-name = Deutsch

# AUR Packages (-A)
A-install-deps = Bestimme Abhängigkeiten...
A-install-repo-pkgs = Abhängigkeiten in den Paketquellen:
A-install-aur-pkgs = AUR Pakete:
A-install-path-comp = Konnte letzte Komponente nicht extrahieren von: { $path }
A-install-ignored = { $file } ist als "ignored" markiert. Trotzdem installieren?

A-build-prep = Bereite build-Verzeichnisse vor...
A-build-pkg = Baue Paket { $pkg }...
A-build-diff = Diffs der build-Dateien anzeigen?
A-build-hotedit-pkgbuild = PKGBUILD editieren?
A-build-hotedit-install = Die .install file editieren?
A-build-fail = Bauen fehlgeschlagen.
A-build-e-pkgctl = Bauen in isolierter chroot-Umgebung fehlgeschlagen.
A-build-e-makepkg = makepkg fehlgeschlagen.
A-build-e-edit = Editieren folgender Datei fehlgeschlagen: { $file }
A-build-e-tarball = Verschieben folgender Datei fehlgeschlagen: { $file }
A-build-e-filename = Konnte Dateiname nicht extrahieren von: { $file }
A-build-e-copies = Konnte build-Dateien nicht kopieren.
A-build-e-perm = Konnte Berechtigungen nicht setzen für: { $dir }
A-build-pkglist = Konnte makepkg Output-Pfade nicht herausfinden für: { $dir }
A-build-pull = Konnte neuesten commit nicht pullen - möglicherweise wird eine ältere Version installiert!
A-build-continue = Möchten Sie trotzdem fortfahren?

A-i-repo = Repository
A-i-version = Version
A-i-status = AUR-Status
A-i-maintainer = Maintainer
A-i-proj-url = Projekt-URL
A-i-aur-url = AUR-URL
A-i-license = Lizenz
A-i-group = Gruppe
A-i-provides = Liefert
A-i-depends = Hängt ab von
A-i-make = Build-Abhängigkeiten
A-i-opt = Optionale Abhängigkeiten
A-i-check = Check-Abhängigkeiten
A-i-votes = Stimmen
A-i-pop = Beliebtheit
A-i-desc = Beschreibung
A-i-keywords = Schlüsselwörter
A-i-submitted = Eingereicht
A-i-updated = Aktualisiert

A-u-fetch-info = Rufe Paketinformationen ab...
A-u-comparing = Vergleiche Paketversionen...
A-u-no-upgrades = Keine Aktualisierungen für AUR-Paket notwendig.
A-u-to-upgrade = Zu aktualisierendes AUR-Paket:
A-u-git = Neu zu buildende VCS-Pakete:

A-w = Klone { $package }...

A-y-refreshing = Lokale Klone bekannter AUR-Pakete werden aktualisiert...
A-y-pulling = Neueste commits werden gepullt

# Snapshots (-B)
B-saved = Paketzustand gesichert.
B-clean = Veraltete snapshots entfernen?
B-none = Keine nutzbaren snapshots gefunden.
B-select = Wähle einen snapshot zur Wiederherstellung aus:

# Cache (-C)
C-size = Aktuelle Cache-Größe: { $size }

C-b-file = { $target } existiert bereits und ist kein Verzeichnis.
C-b-nonempty = Das Ziel { $target } existiert, ist aber nicht leer!
C-b-target = Sichere Cache in { $target }
C-b-curr = Das aktuelle Verzeichnis konnte nicht gelesen werden.

C-i-latest = Neueste
C-i-created = Erstellt
C-i-installed = Installiert
C-i-sig = Signatur
C-i-size = Archivgröße
C-i-avail = Verfügbare Versionen

C-c-keep = { $pkgs } jeder Paketdatei wird behalten. Der Rest wird gelöscht.
C-c-freed = { $bytes } freigegeben.

C-downgrade-which = Welche Version von { $pkg } möchtest du haben?

C-y-no-work = Der Paket-Cache ist bereits synchronisiert.
C-t-invalids = Ungültige Paketarchive werden entfernt.

# Logs (-L)
L-first = Erste Installation
L-upgrades = Aktualisierungen
L-recent = Letzte Aktionen
L-search-err = Die Suche in deinen Logdateien mit { $cmd } ist fehlgeschlagen.
L-view-err = Das ALPM-Logdatei konnte nicht geöffnet werden.

# Opening Pages (open)
open-err = { $url } konnte nicht geöffnet werden.

# System Statistics (stats)
stats-local = Die Sprachdateien konnten nicht geladen werden.
stats-host = Host
stats-user = Benutzer
stats-distro = Distribution
stats-editor = Editor
stats-pkgs = Installierte Pakete
stats-aura-cache = Aura-Paket-Cache
stats-pacman-cache = Pacman-Paket-Cache
stats-aura-build = Aura-Build-Cache
stats-tmp = /tmp-Verzeichnis

# System Validation (check)
check-start = System wird überprüft.
check-missing-exec = Lösung: Installiere { $exec } und/oder stelle sicher, dass es in deinem PATH liegt.
check-env = Umgebung
check-env-editor = EDITOR-Variable gesetzt?
check-env-editor-exec = EDITOR-Wert ({ $exec }) ist ausführbar?
check-env-editor-vi = Backup-Editor vi ist ausführbar?
check-env-exec = { $exec } installiert und ausführbar?
check-env-lang = { $cmd } beinhaltet LANG-Wert? ({ $lang })
check-env-lang-fix = Ergänze { $lang } in deiner Datei { $file }.
check-env-lang-fix2 = Setze deine LANG-Variable!
check-env-lang-known = Ist Aura für deinen LANG-Wert lokalisiert?
check-env-java-bin = Sind die Java-Werkzeuge installiert?
check-env-java-bin-fix = Erwäge, { $pkg } zu installieren.
check-env-java-set = Ist die Java-Umgebung eingerichtet?
check-env-java-set-fix = Siehe { $cmd }.
check-pconf = Pacman-Konfiguration (/etc/pacman.conf)
check-pconf-par = Sind parallele Downloads aktiviert?
check-pconf-par-fix = { $setting } is deaktiviert, oder auf 1 gesetzt. Setze { $set }, um Paketarchive schneller herunterzuladen.
check-pconf-ignores = Gibt es keine Überschneidungen bei ignorierten Paketen?
check-pconf-ignores-fix = Die folgenden Pakete werden in pacman.conf und in aura.toml ignoriert: { $pkgs }
check-pconf-pacnew = Wurden alle .pacnew-Dateien berücksichtigt?
check-pconf-pacnew-broken = Der Aufruf von { $fd } ist vollständig fehlgeschlagen.

check-pconf-pacnew-old = { $path } ist { $days ->
   [one] 1 Tag
  *[many] {$days} Tage
} älter als die zugehörige .pacnew-Datei.

check-aconf = Aura-Konfiguration
check-aconf-aura-exists = Existiert die Aura-Konfigurationsdatei?
check-aconf-aura-exists-fix = Lösung: Erwäge { $cmd }.
check-aconf-aura-parse = Kann die Aura-Konfigurationsdatei eingelesen werden?
check-aconf-old-dirs = Existieren keine alten Aura-Verzeichnisse?
check-aconf-old-conf = Existieren keine alten Aura-Konfigurationsdateien?
check-mconf = Makepkg-Konfiguration ({ $path })
check-mconf-packager = Ist PACKAGER gesetzt?
check-mconf-packager-fix = Lösung: Setze { $cmd } in { $path }.
check-snapshots = Paketsnapshots
check-snapshot-usable = Gibt es zu allen Snapshots die entsprechenden Paketarchive?
check-snapshot-usable-fix = Lösung: Du kannst alte oder unbrauchbare Snapshots mit { $command } entfernen.
check-cache = Paketarchiv-Caches
check-cache-exists = Existieren alle angegebenen Caches?
check-cache-tarballs = Sind alle Paketarchive gültig?
check-cache-tarballs-fix = Lösung: Du kannst ungültige Paketarchive mit { $command } entfernen.
check-cache-missing = Gibt es für jedes installierte offizielle Paket ein Paketarchiv?
check-cache-missing-fix = Lösung: Du kannst fehlende Archive offizieller Pakete herunterladen mit { $command }.
check-cache-missing-for = Gibt es für jedes installierte AUR-Paket ein Paketarchiv?
check-cache-missing-for-fix = Lösung: Zeige die fehlenden Pakete mit { $cmd } an und installiere sie manuell neu.
check-pkgs = Paketstatus
check-pkgs-old = Sind alle explizit installierten Pakete, die keine Abhängigkeiten sind, auf dem neuesten Stand?
check-pkgs-old-warn = { $pkg } wurde zuletzt vor { $days } Tagen aktualisiert.
check-pkgs-empty = Sind alle Paketklone befüllt?
check-pkgs-empty-fix = Lösung: Lösche die folgenden Verzeichnisse.

# Thanks
thanks-you = Vielen Dank, dass du Aura verwendest.
thanks-colin = Aura von Colin Woodbury, 2012 - 2024
thanks-pacman = Vielen Dank an die Pacman- und Arch-Linux-Teams für die solide Grundlage.
thanks-everyone = Vielen Dank an alle Mitwirkenden, Spender und Nutzer von Aura.
thanks-logo = Das Aura-Logo stammt von Cristiano Vitorino.
thanks-translators = Aura wird übersetzt von:

# Configuration (conf)
conf-toml-err = Die aktuelle Konfiguration konnte nicht serialisiert werden.

# Dependencies (deps)
deps-io = Das Abhängigkeitsdiagramm konnte nicht erstellt werden.

# Runtime Environment
env-missing-editor = Der angegebene EDITOR liegt nicht in PATH.
env-pconf = Deine pacman.conf-Datei konnte nicht gelesen werden.

# Pacman Calls
pacman-external = Ein Aufruf von pacman ist vollständig fehlgeschlagen.
pacman-u = Ein Aufruf von pacman -U ist fehlgeschlagen.
pacman-s = Ein Aufruf von pacman -S ist fehlgeschlagen.
pacman-misc = Ein Aufruf von pacman hat einen Exit-Code ungleich null zurückgegeben.

# Aura-specific Directories
dir-mkdir = Das Verzeichnis konnte nicht erstellt werden: { $dir }.
dir-home = Auras Konfigurationsverzeichnis konnte nicht ermittelt werden.
dir-cache = Auras Cache-Verzeichnis konnte nicht ermittelt werden.

# Dependency Resolution
dep-exist = Das Paket { $pkg } existiert nicht.
dep-exist-par = Die Abhängigkeit { $pkg } von { $par } existiert nicht.
dep-graph = Der Abhängigkeitsgraph ist fehlerhaft aufgebaut.
dep-cycle = Abhängigkeitszyklus erkannt: { $cycle }
dep-multi = Bei der Auflösung der Abhängigkeiten sind mehrere Fehler aufgetreten.

# Git Operations
git-diff = Ein git-diff-Aufruf ist fehlgeschlagen für: { $file }
git-hash = Der Git-Hash konnte nicht in Rust eingelesen werden.
git-pull = Ein git-pull-Aufruf ist fehlgeschlagen: { $dir }
git-clone = Ein git-clone-Aufruf ist fehlgeschlagen: { $dir }
git-io = Ein git-Aufruf ist fehlgeschlagen.

# Faur Calls
faur-fetch = Der Aufruf des Metadatenservers ist fehlgeschlagen: { $pkg }
faur-unknown = Unbekanntes Paket: { $pkg }
faur-too-many = Faur hat mehr Ergebnisse als erwartet zurückgegeben: { $pkg }

# Common Errors
err-alpm = Der ALPM-Handle konnte nicht geöffnet werden.
err-config-path = Der Pfad zur Aura-Konfigurationsdatei konnte nicht ermittelt werden.
err-curl = Eine CURL-Übertragung ist fehlgeschlagen: { $err }
err-file-del = Löschen fehlgeschlagen: { $file }
err-file-open = Der Datei-Handle konnte nicht geöffnet werden für: { $file }
err-file-write = Schreiben der Datei fehlgeschlagen: { $file }
err-json-decode = JSON konnte nicht von { $url } dekodiert werden.
err-json-write = JSON konnte nicht in { $file } geschrieben werden.
err-mutex = Ein Mutex wurde vergiftet.
err-pool-create = Der ALPM-Verbindungspool konnte nicht erstellt werden.
err-pool-get = Aus dem Verbindungspool konnte kein ALPM-Handle abgerufen werden.
err-read-dir = Das Verzeichnis konnte nicht gelesen werden: { $dir }
err-srcinfo = Die .SRCINFO-Datei konnte nicht eingelesen werden: { $file }
err-sudo = Aura muss nicht mit sudo ausgeführt werden.
err-time-conv = Ein Zeitstempel konnte nicht konvertiert werden.
err-time-format = Eine Zeitangabe konnte nicht formatiert werden.
err-user-input = Die Benutzereingabe konnte nicht gelesen werden.
err-utf8 = Eine UTF-8-Konvertierung ist fehlgeschlagen.
err-write = Die Ausgabe auf stdout ist fehlgeschlagen.

# Common Fields
common-yes = Ja
common-no = Nein
common-name = Name
common-done = Erledigt.
common-no-packages = Keine Pakete angegeben.
common-no-work = Nichts zu tun.
common-cancelled = Aktion abgebrochen.
common-replace = Du kannst { $old } zugunsten von { $new } löschen.

# Misc.
proceed = Fortfahren?
proceed-affirmative = j
proceed-affirmative-alt = J
proceed-negative = n
