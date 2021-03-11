{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

-- |
-- Module    : Aura.Settings.External
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- A simple parser for .conf files, along with types for aura-specific config
-- files.

module Aura.Settings.External
  ( -- * Aura Config
    AuraConfig(..)
  , getAuraConf
  , auraConfig
    -- * Parsing
  , Config(..)
  , config
  ) where

import           Aura.Languages (langFromLocale)
import           Aura.Settings
import           Aura.Shell (getTrueUser)
import           Aura.Types
import           Aura.Utils (hush)
import           RIO hiding (some, try)
import qualified RIO.ByteString as BS
import           RIO.Directory
import           RIO.FilePath ((</>))
import qualified RIO.Map as M
import qualified RIO.Text as T
import           Text.Megaparsec hiding (single)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- Aura-specific Configuration

data AuraConfig = AuraConfig
  { acLang      :: !(Maybe Language)
  , acEditor    :: !(Maybe FilePath)
  , acUser      :: !(Maybe User)
  , acBuildPath :: !(Maybe FilePath)
  , acASPath    :: !(Maybe FilePath)
  , acVCSPath   :: !(Maybe FilePath)
  , acAnalyse   :: !(Maybe BuildSwitch) }
  deriving stock (Show)

userAuraConfPath :: Environment -> Maybe FilePath
userAuraConfPath env = case getTrueUser env of
  Nothing            -> Nothing
  Just (User "root") -> Nothing
  Just (User u)      -> Just $ "/home/" </> T.unpack u </> ".config/aura/aura.conf"

systemAuraConfPath :: FilePath
systemAuraConfPath = "/etc/aura.conf"

-- | Attempt to get a valid Aura config from a specified path.
getAuraConfFrom :: FilePath -> IO (Maybe Config)
getAuraConfFrom path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      file <- decodeUtf8Lenient <$> BS.readFile path
      pure . hush $ parse config "aura config" file

getAuraConf :: Environment -> IO Config
getAuraConf env = case userAuraConfPath env of
  Nothing   -> bad
  Just path -> getAuraConfFrom path >>= maybe bad pure
  where
    bad = fromMaybe (Config M.empty) <$> getAuraConfFrom systemAuraConfPath

auraConfig :: Config -> AuraConfig
auraConfig (Config m) = AuraConfig
  { acLang = one "language" >>= langFromLocale
  , acEditor = T.unpack <$> one "editor"
  , acUser = User <$> one "user"
  , acBuildPath = T.unpack <$> one "buildpath"
  , acASPath = T.unpack <$> one "allsourcepath"
  , acVCSPath = T.unpack <$> one "vcspath"
  , acAnalyse = one "analyse" >>= readMaybe . T.unpack >>= bool (Just NoPkgbuildCheck) Nothing
  }
  where
    one x = M.lookup x m >>= listToMaybe

--------------------------------------------------------------------------------
-- Parsing

-- | The (meaningful) contents of a .conf file.
newtype Config = Config (Map Text [Text]) deriving (Show)

-- | Parse a `Config`.
config :: Parsec Void Text Config
config = do
  garbage
  cs <- some $ fmap Right (try pair) <|> fmap Left single
  eof
  pure . Config . M.fromList $ rights cs

single :: Parsec Void Text ()
single = L.lexeme garbage . void $ manyTill letterChar newline

pair :: Parsec Void Text (Text, [Text])
pair = L.lexeme garbage $ do
  n <- T.stripEnd <$> takeWhile1P Nothing (/= '=')
  void $ char '='
  space
  rest <- T.words <$> takeWhile1P Nothing (/= '\n')
  pure (n, rest)

-- Thu 23 Apr 2020 06:57:59 PM PDT
-- Thank you me-from-the-past for documenting this.
-- | All skippable content. Using `[]` as block comment markers is a trick to
-- skip conf file "section" lines.
garbage :: Parsec Void Text ()
garbage = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "[" "]")
