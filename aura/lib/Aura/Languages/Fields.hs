{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module    : Aura.Languages.Fields
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- The various fields for @-Ai@ output.

module Aura.Languages.Fields where

import Aura.Types (Language(..))
import RIO (Text)

---

package :: Language -> Text
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
    Italian    -> "Pacchetto"
    Serbian    -> "Пакет"
    Norwegian  -> "Pakke"
    Indonesia  -> "Paket"
    Esperanto  -> "Pakaĵo"
    Dutch      -> "Pakket"
    _          -> "Package"

firstInstall :: Language -> Text
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
    Esperanto  -> "Unua Instalo"
    Dutch      -> "Eerste installatie"
    _          -> "First Install"

upgrades :: Language -> Text
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
    Italian    -> "Aggiornamenti"
    Serbian    -> "Ажурирања"
    Norwegian  -> "Oppgraderinger"
    Indonesia  -> "Tingkatkan"
    Esperanto  -> "Noveldonoj"
    Dutch      -> "Upgrades"
    _          -> "Upgrades"

recentActions :: Language -> Text
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
    Esperanto  -> "Ĵusaj Agoj"
    Dutch      -> "Recente Acties"
    _          -> "Recent Actions"

repository :: Language -> Text
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
    Esperanto  -> "Deponejo"
    Dutch      -> "Repository"
    _          -> "Repository"

name :: Language -> Text
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
    Esperanto  -> "Nomo"
    Dutch      -> "Naam"
    _          -> "Name"

version :: Language -> Text
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
    Esperanto  -> "Versio"
    Dutch      -> "Versie"
    _          -> "Version"

aurStatus :: Language -> Text
aurStatus = \case
    Japanese   -> "パッケージ状態"
    Polish     -> "Status w AUR"
    Croatian   -> "AUR Stanje"
    German     -> "AUR-Status"
    Spanish    -> "Estado en AUR"
    Portuguese -> "Estado no AUR"
    French     -> "Statut de AUR"
    Russian    -> "Статус в AUR"
    Italian    -> "Stato nell'AUR"
    Serbian    -> "Статус у AUR-у"
    Indonesia  -> "Status AUR"
    Esperanto  -> "Stato en AUR"
    Dutch      -> "AUR Status"
    _          -> "AUR Status"

-- NEEDS TRANSLATION
maintainer :: Language -> Text
maintainer = \case
    Japanese   -> "管理者"
    Spanish    -> "Mantenedor"
    Portuguese -> "Mantenedor"
    French     -> "Mainteneur"
    Russian    -> "Ответственный"
    Italian    -> "Mantenitore"
    Norwegian  -> "Vedlikeholder"
    Indonesia  -> "Pemelihara"
    Esperanto  -> "Daŭriganto"
    Dutch      -> "Onderhouder"
    _          -> "Maintainer"

projectUrl :: Language -> Text
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
    Esperanto  -> "URL de Projekto"
    Dutch      -> "Project URL"
    _          -> "Project URL"

aurUrl :: Language -> Text
aurUrl = \case
    Japanese   -> "パッケージページ"
    Polish     -> "URL w AUR"
    German     -> "AUR-URL"
    Spanish    -> "URL de AUR"
    Portuguese -> "URL no AUR"
    French     -> "URL AUR"
    Russian    -> "URL в AUR"
    Italian    -> "URL nell'AUR"
    Serbian    -> "Страница у AUR-у"
    Indonesia  -> "URL AUR"
    Esperanto  -> "URL en AUR"
    Dutch      -> "AUR URL"
    _          -> "AUR URL"

license :: Language -> Text
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
    Esperanto  -> "Permesilo"
    Dutch      -> "Licentie"
    _          -> "License"

dependsOn :: Language -> Text
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
    Esperanto  -> "Dependi de"
    Dutch      -> "Hangt af van"
    _          -> "Depends On"

buildDeps :: Language -> Text
buildDeps = \case
    Japanese   -> "作成時従属パ"
    German     -> "Build-Abhängigkeiten"
    Spanish    -> "Dependencias de compilación"
    Portuguese -> "Dependências de compilação"
    French     -> "Dépendances de compilation"
    Russian    -> "Зависимости сборки"
    Italian    -> "Dipendenze di compilazione"
    Indonesia  -> "Dependensi bangun"
    Esperanto  -> "Muntaj Dependecoj"
    Dutch      -> "Bouw afhankelijkheden"
    _          -> "Build Deps"

votes :: Language -> Text
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
    Esperanto  -> "Balotiloj"
    Dutch      -> "Stemmen"
    _          -> "Votes"

popularity :: Language -> Text
popularity = \case
    Japanese   -> "人気"
    Spanish    -> "Popularidad"
    Portuguese -> "Popularidade"
    Italian    -> "Popolarità"
    Esperanto  -> "Populareco"
    Dutch      -> "Populairiteit"
    _          -> "Popularity"

description :: Language -> Text
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
    Esperanto  -> "Priskribo"
    Dutch      -> "Beschrijving"
    _          -> "Description"
