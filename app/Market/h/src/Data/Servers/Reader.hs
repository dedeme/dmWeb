-- Copyright 13-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | HTTP reader

module Data.Servers.Reader (
  Data.Servers.Reader.read,
  trim,
  fdrop,
  ftake,
  readDouble
  ) where

import Control.Exception
import Data.List
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.HTTP.Conduit (simpleHttp)

-- |@'read' uri@| - Reads /uri/ and return its content or "" if reading fails.
read :: String -> IO String
read uri = catch (simpleHttp uri >>= return . C8.unpack)
                 ((\_ -> return "") :: SomeException -> IO String)

-- |@'trim' s@ - Removes intial blanks and returns the rest of /s/ until the
--               first blank.
trim :: String -> String
trim = takeWhile (> ' ') . dropWhile (<= ' ')

-- |@'fdrop' s sub@ - Returns the rest of /s/ after the first occurrence of
--                    /sub/ or "" if /sub/ does not occur in /s/
fdrop :: String -> String -> String
fdrop sub "" = ""
fdrop sub s@(_:rest) = if isPrefixOf sub s then s else fdrop sub rest

-- |@'ftake' s sub@ - Returns the part of /s/ previous to the first occurrence
--                    of /sub/
ftake :: String -> String -> String
ftake sub s = ftake' "" s
  where
  ftake' r "" = reverse r
  ftake' r s@(c:cs) = if isPrefixOf sub s
                      then reverse r
                      else ftake' (c:r) cs

regularizeN = (remComma . remPoint)
  where
  remComma = map (\c -> if c == ',' then '.' else c)
  remPoint = filter (/= '.')

-- |@'readDouble' s@ - Returns /s/ as a Double.
readDouble :: String -> IO Double
readDouble s = do
  r <- try $ evaluate $ Prelude.read $
                        regularizeN s :: IO (Either SomeException Double)
  case r of
    Left _ -> return (-1)
    Right v -> return v
