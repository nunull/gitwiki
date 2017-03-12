module Network.GitWiki.Types where

import qualified Data.Text.Lazy as T
import           Data.Time
import           Data.Version

data Config = Config { wikiDir :: String
                     , version :: Version
                     }

data Page = Page { slug :: String
                 , content :: T.Text
                 , revision :: String
                 }

data Revision = Revision { hash :: String
                         , pageSlug :: String
                         , authorName :: String
                         , authorEmail :: String
                         , date :: UTCTime
                         , message :: String
                         , isLatest :: Bool
                         }

data User = User { name :: String
                 , email :: String
                 , password :: String
                 , admin :: Bool
                 }

instance Eq User where
  x == y = email x == email y
