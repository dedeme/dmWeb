-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Finanzas page

module Data.Servers.Finanzas (quote) where

import Control.Exception
import Text.Printf (printf)
import qualified Data.Servers.Reader as Reader

codes :: [(String, String)]
codes = [
    ("A3M", "atresmedia"),
    ("ACS", "acs"),
    ("ACX", "acerinox"),
    ("ADZ", "adolfo-dguez"),
    ("AENA", "aena"),
    ("ALM", "almirall"),
    ("AMS", "amadeus"),
    ("ANA", "acciona"),
    ("APPS", "applus-servi"),
    ("BBVA", "bbva"),
    ("BIO", "biosearch"),
    ("BKIA", "bankia"),
    ("BKT", "bankinter"),
    ("BME", "bme"),
    ("CABK", "caixabank"),
    ("CAF", "c-a-f"),
    ("CIE", "cie-automot"),
    ("CLNX", "cellnex"),
    ("COL", "inm-colonial"),
    ("DIA", "dia"),
    ("EBRO", "ebro-foods"),
    ("ECR", "ercros"),
    ("EDR", "edreams-odig"),
    ("EKT", "euskaltel"),
    ("ELE", "endesa"),
    ("ENC", "ence"),
    ("ENG", "enagas"),
    ("FAE", "faes"),
    ("FCC", "fcc"),
    ("FDR", "fluidra"),
    ("FER", "ferrovial"),
    ("GEST", "gestamp"),
    ("GRF", "grifols"),
    ("GSJ", "san-jose"),
    ("HIS", "hispania"),
    ("IAG", "iag"),
    ("IBE", "iberdrola"),
    ("IDR", "indra-a"),
    ("ITX", "inditex"),
    ("LGT", "lingotes"),
    ("LOG", "logista"),
    ("MAP", "mapfre"),
    ("MEL", "melia-hotels"),
    ("MRL", "merlin-prop"),
    ("MTS", "arcelormitt"),
    ("NAT", "natra"),
    ("NHH", "nh-hotels"),
    ("NTGY", "naturgy"),
    ("NTH", "naturhouse"),
    ("OHL", "ohl"),
    ("PHM", "pharma-mar"),
    ("PRS", "prisa"),
    ("PSG", "prosegur"),
    ("QBT", "quabit"),
    ("REE", "r-e-c"),
    ("REP", "repsol"),
    ("SAB", "b-sabadell"),
    ("SAN", "santander"),
    ("SCYR", "sacyr"),
    ("SGRE", "siemens-gam"),
    ("SLR", "solaria"),
    ("TEF", "telefonica"),
    ("TL5", "mediaset"),
    ("TLGO", "talgo"),
    ("TRE", "tecnicas-reu"),
    ("TUB", "tubacex"),
    ("VID", "vidrala"),
    ("VIS", "viscofan"),
    ("VOC", "vocento"),
    ("ZOT", "zardoya-otis"),
---------------------
    ("MDF", "d-felguera"),
    ("PVA", "pescanova")
  ]

-- |@'quote' nick@ - Returns the current quote of /nick/ or -1 if it can not
--                   be read.
quote :: String -> IO Double
quote nick = do
  html <- Reader.read $ uri nick
  q html

uri :: String -> String
uri nick = case lookup nick codes of
  Nothing -> "http://localhost"
  Just c -> printf "http://www.finanzas.com/cotizaciones/%s/" c

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
