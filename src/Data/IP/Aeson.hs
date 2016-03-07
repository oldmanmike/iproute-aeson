module Data.IP.Aeson where

import Control.Monad
import Data.Aeson
import Data.IP
import qualified Data.Text as T


instance ToJSON IP where
    toJSON = String . T.pack . show

instance FromJSON IP where
    parseJSON (String ip) = pure (read $ T.unpack ip :: IP)
    parseJSON _ = mzero

instance ToJSON IPv4 where
    toJSON = String . T.pack . show

instance FromJSON IPv4 where
    parseJSON (String ip) = pure (read $ T.unpack ip :: IPv4)
    parseJSON _ = mzero

instance ToJSON IPv6 where
    toJSON = String . T.pack . show

instance FromJSON IPv6 where
    parseJSON (String ip) = pure (read $ T.unpack ip :: IPv6)
    parseJSON _ = mzero
