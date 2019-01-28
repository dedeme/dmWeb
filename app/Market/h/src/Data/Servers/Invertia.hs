-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Invertia page

module Data.Servers.Invertia (quote) where

import Control.Exception
import Text.Printf (printf)
import qualified Data.Servers.Reader as Reader
import qualified Data.Servers.Codes as Codes

codes = do
  cs <- Codes.codes "serverInvertia"
  return $ cs ++ [
      ("MDF", "duro-felguera/RV011DUROFEL"),
      ("PVA", "pescanova/RV011PESCANO")
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
    Just c -> return $ printf ("https://www.invertia.com/es/mercados/bolsa/" ++
                               "empresas/evolucion/-/empresa/santander/%s") c

q :: String -> IO Double
q html = do
  tx <- try $ evaluate $ Reader.trim $ tail $ tail $
              (Reader.ftake "<") $
              (Reader.fdrop "\">") $
              (Reader.fdrop "&Uacute;ltimo") html
        :: IO (Either SomeException String)
  case tx of
    Left _ -> return (-1)
    Right n -> Reader.readDouble n
