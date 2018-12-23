-- Copyright 13-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | HTTP reader

module Data.Servers.Reader (Data.Servers.Reader.read) where

import qualified Data.ByteString.Lazy.Char8 as C8
import Network.HTTP.Conduit (simpleHttp)

read :: String -> IO String
read uri = simpleHttp uri >>= return . C8.unpack
