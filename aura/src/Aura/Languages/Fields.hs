{-# LANGUAGE LambdaCase #-}

module Aura.Languages.Fields where

import Aura.Languages.Base
import BasePrelude

---

package :: Language -> String
package = \case
    Japanese   -> "パッケージ"
    Polish     -> "Pakiet"
    Croatian   -> "Paket"
    Swedish    -> "Paket"
    German     -> "Paket"
    Spanish    -> "Paquete"
    Portuguese -> "Pacote"
    French     -> "Paquet"
    Russian    -> "Пакет"
    Italian    -> "Package"
    Serbian    -> "Пакет"
    Norwegian  -> "Pakke"
    Indonesia  -> "Paket"
    _          -> "Package"

firstInstall :: Language -> String
firstInstall = \case
    Japanese   -> "初インストール"
    Polish     -> "Pierwsza instalacja"
    Croatian   -> "Prva instalacija"
    Swedish    -> "Första installation"
    German     -> "Erste Installation"
    Spanish    -> "Primera instalación"
    Portuguese -> "Primeira instalação"
    French     -> "Première installation"
    Russian    -> "Первая установка"
    Italian    -> "Prima installazione"
    Serbian    -> "Прва инсталација"
    Norwegian  -> "Første installasjon"
    Indonesia  -> "Versi sistem"
    _          -> "First Install"

upgrades :: Language -> String
upgrades = \case
    Japanese   -> "アップグレード回数"
    Polish     -> "Aktualizacje"
    Croatian   -> "Nadogradnje"
    Swedish    -> "Uppgraderingar"
    German     -> "Aktualisierungen"
    Spanish    -> "Actualizaciones"
    Portuguese -> "Atualizações"
    French     -> "Mises à jours"
    Russian    -> "Обновления"
    Italian    -> "Upgrades"
    Serbian    -> "Ажурирања"
    Norwegian  -> "Oppgraderinger"
    Indonesia  -> "Tingkatkan"
    _          -> "Upgrades"

recentActions :: Language -> String
recentActions = \case
    Japanese   -> "近況"
    Polish     -> "Ostatnie akcje"
    Croatian   -> "Nedavne radnje"
    Swedish    -> "Nyliga händelser"
    German     -> "Letzte Aktionen"
    Spanish    -> "Acciones Recientes"
    Portuguese -> "Ações Recentes"
    French     -> "Actions récentes"
    Russian    -> "Недавние действия"
    Italian    -> "Azioni recenti"
    Serbian    -> "Недавне радње"
    Norwegian  -> "Nylige hendelser"
    Indonesia  -> "Aksi sekarang"
    _          -> "Recent Actions"

repository :: Language -> String
repository = \case
    Japanese   -> "リポジトリ"
    Polish     -> "Repozytorium"
    Croatian   -> "Repozitorij"
    Swedish    -> "Repository"
    German     -> "Repository"
    Spanish    -> "Repositorio"
    Portuguese -> "Repositório"
    French     -> "Dépôt"
    Russian    -> "Репозиторий"
    Italian    -> "Repository"
    Serbian    -> "Ризница"
    Norwegian  -> "Depot"
    Indonesia  -> "Lumbung"
    _          -> "Repository"

name :: Language -> String
name = \case
    Japanese   -> "名前"
    Polish     -> "Nazwa"
    Croatian   -> "Ime"
    Swedish    -> "Namn"
    German     -> "Name"
    Spanish    -> "Nombre"
    Portuguese -> "Nome"
    French     -> "Nom"
    Russian    -> "Название"
    Italian    -> "Nome"
    Serbian    -> "Име"
    Norwegian  -> "Navn"
    Indonesia  -> "Nama"
    _          -> "Name"

version :: Language -> String
version = \case
    Japanese   -> "バージョン"
    Polish     -> "Wersja"
    Croatian   -> "Verzija"
    Swedish    -> "Version"
    German     -> "Version"
    Spanish    -> "Versión"
    Portuguese -> "Versão"
    French     -> "Version"
    Russian    -> "Версия"
    Italian    -> "Versione"
    Serbian    -> "Верзија"
    Norwegian  -> "Versjon"
    Indonesia  -> "Versi"
    _          -> "Version"

aurStatus :: Language -> String
aurStatus = \case
    Japanese   -> "パッケージ状態"
    Polish     -> "Status w AUR"
    Croatian   -> "AUR Stanje"
    German     -> "AUR-Status"
    Spanish    -> "Estado en AUR"
    Portuguese -> "Estado no AUR"
    French     -> "Statut de AUR"
    Russian    -> "Статус в AUR"
    Italian    -> "Stato in AUR"
    Serbian    -> "Статус у AUR-у"
    Indonesia  -> "Status AUR"
    _          -> "AUR Status"

-- NEEDS TRANSLATION
maintainer :: Language -> String
maintainer = \case
    Japanese   -> "管理者"
    Spanish    -> "Mantenedor"
    French     -> "Mainteneur"
    Russian    -> "Ответственный"
    Norwegian  -> "Vedlikeholder"
    Indonesia  -> "Pemelihara"
    _          -> "Maintainer"

projectUrl :: Language -> String
projectUrl = \case
    Japanese   -> "プロジェクト"
    Polish     -> "URL Projektu"
    Croatian   -> "URL Projekta"
    Swedish    -> "Projekt URL"
    German     -> "Projekt-URL"
    Spanish    -> "URL del proyecto"
    Portuguese -> "URL do projeto"
    French     -> "URL du projet"
    Russian    -> "URL проекта"
    Italian    -> "URL del progetto"
    Serbian    -> "Страница пројекта"
    Norwegian  -> "Prosjekt-URL"
    Indonesia  -> "URL Proyek"
    _          -> "Project URL"

aurUrl :: Language -> String
aurUrl = \case
    Japanese   -> "パッケージページ"
    Polish     -> "URL w AUR"
    German     -> "AUR-URL"
    Spanish    -> "URL de AUR"
    Portuguese -> "URL no AUR"
    French     -> "URL AUR"
    Russian    -> "URL в AUR"
    Italian    -> "URL AUR"
    Serbian    -> "Страница у AUR-у"
    Indonesia  -> "URL AUR"
    _          -> "AUR URL"

license :: Language -> String
license = \case
    Japanese   -> "ライセンス"
    Polish     -> "Licencja"
    Croatian   -> "Licenca"
    Swedish    -> "Licens"
    German     -> "Lizenz"
    Spanish    -> "Licencia"
    Portuguese -> "Licença"
    French     -> "Licence"
    Russian    -> "Лицензия"
    Italian    -> "Licenza"
    Serbian    -> "Лиценца"
    Norwegian  -> "Lisens"
    Indonesia  -> "Lisensi"
    _          -> "License"

dependsOn :: Language -> String
dependsOn = \case
    Japanese   -> "従属パッケージ"
    Polish     -> "Zależności"
    Croatian   -> "Zavisnosti"
    German     -> "Hängt ab von"
    Spanish    -> "Dependencias"
    Portuguese -> "Dependências"
    French     -> "Dépends de"
    Russian    -> "Зависит от"
    Italian    -> "Dipende da"
    Norwegian  -> "Er avhengig av"
    Indonesia  -> "Bergantung pada"
    _          -> "Depends On"

buildDeps :: Language -> String
buildDeps = \case
    Japanese   -> "作成時従属パ"
    German     -> "Build-Abhängigkeiten"
    Spanish    -> "Dependencias de compilación"
    French     -> "Dépendances de compilation"
    Russian    -> "Зависимости сборки"
    Indonesia  -> "Dependensi bangun"
    _          -> "Build Deps"

votes :: Language -> String
votes = \case
    Japanese   -> "投票数"
    Polish     -> "Głosy"
    Croatian   -> "Glasovi"
    Swedish    -> "Röster"
    German     -> "Stimmen"
    Spanish    -> "Votos"
    Portuguese -> "Votos"
    French     -> "Votes"
    Russian    -> "Голоса"
    Italian    -> "Voti"
    Serbian    -> "Гласови"
    Norwegian  -> "Stemmer"
    Indonesia  -> "Suara"
    _          -> "Votes"

popularity :: Language -> String
popularity = \case
    Japanese   -> "人気"
    _          -> "Popularity"

description :: Language -> String
description = \case
    Japanese   -> "概要"
    Polish     -> "Opis"
    Croatian   -> "Opis"
    Swedish    -> "Beskrivning"
    German     -> "Beschreibung"
    Spanish    -> "Descripción"
    Portuguese -> "Descrição"
    French     -> "Description"
    Russian    -> "Описание"
    Italian    -> "Descrizione"
    Serbian    -> "Опис"
    Norwegian  -> "Beskrivelse"
    Indonesia  -> "Deskripsi"
    _          -> "Description"

makeDeps :: Language -> String
makeDeps = \case
    Polish     -> "Zależności Make"
    Croatian   -> "Make Zavisnosti"
    German     -> "Make-Abhängigkeiten"
    Spanish    -> "Dependencias de compilación"
    Portuguese -> "Depenências de compilação"
    French     -> "Dépendances de compilation"
    Russian    -> "Зависимости Make"
    Indonesia  -> "Dependensi bangun"
    _          -> "Make Deps"
