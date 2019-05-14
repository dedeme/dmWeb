-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Yahoo page

module Data.Servers.Yahoo (quote, uri) where

import Control.Exception
import Text.Printf (printf)
import qualified Data.Servers.Reader as Reader
import qualified Data.Servers.Codes as Codes

codes = do
  cs <- Codes.codes "serverYahoo"
  return $ cs ++ [
      ("PVA", "PVA.MC")
    ]

-- |@'quote' nick@ - Returns the current quote of /nick/ or -1 if it can not
--                   be read.
quote :: String -> IO Double
quote nick = do
  url <- uri nick
  html <- Reader.read url
  q html

-- |@'uri' nick@ - Returns the url of nick web page.
uri :: String -> IO String
uri nick = do
  cs <- codes
  case lookup nick cs of
    Nothing -> return "http://localhost"
    Just c -> return $ printf "https://es.finance.yahoo.com/quote/%s?p=%s" c c

q :: String -> IO Double
q html = do
  tx <- try $ evaluate $ Reader.trim $ tail $
              (Reader.ftake "<") $
              (Reader.fdrop ">") $
              (Reader.fdrop "Trsdu(0.3s) Fw(b) Fz(36px) Mb(-4px) D(ib)") html
        :: IO (Either SomeException String)
  case tx of
    Left _ -> return (-1)
    Right n -> Reader.readDouble n
