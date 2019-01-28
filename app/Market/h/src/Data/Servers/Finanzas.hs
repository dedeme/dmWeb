-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Finanzas page

module Data.Servers.Finanzas (quote) where

import Control.Exception
import Text.Printf (printf)
import qualified Data.Servers.Reader as Reader
import qualified Data.Servers.Codes as Codes

codes = do
  cs <- Codes.codes "serverFinanzas"
  return $ cs ++ [
      ("MDF", "d-felguera"),
      ("PVA", "pescanova")
    ]

-- |@'quote' nick@ - Returns the current quote of /nick/ or -1 if it can not
--                   be read.
quote :: String -> IO Double
quote nick = do
  url <- uri nick
  html <- Reader.read url
  q html

uri :: String -> IO String
uri nick = do
  cs <- codes
  case lookup nick cs of
    Nothing -> return "http://localhost"
    Just c -> return $ printf "http://www.finanzas.com/cotizaciones/%s/" c

q :: String -> IO Double
q html = do
  tx <- try $ evaluate $ Reader.trim $
              (Reader.ftake "<") $
              (drop 13) $
              (Reader.fdrop "</time><span>") $
              (Reader.fdrop "<h2>Cotizaciones</h2>") html
        :: IO (Either SomeException String)
  case tx of
    Left _ -> return (-1)
    Right n -> Reader.readDouble n
