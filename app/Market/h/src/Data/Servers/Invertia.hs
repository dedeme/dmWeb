-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Invertia page

module Data.Servers.Invertia (quote) where

import Control.Exception
import Text.Printf (printf)
import qualified Data.Servers.Reader as Reader

codes :: [(String, String)]
codes = [
    ("A3M", "RV011A3TV"),
    ("ACS", "RV011ACS"),
    ("ACX", "RV011ACERINO"),
    ("ADZ", "RV011ADOLFOD"),
    ("AENA", "RV011AENA"),
    ("ALM", "RV011ALMIRAL"),
    ("AMS", "RV011AMADEUS"),
    ("ANA", "RV011ACCIONA"),
    ("APPS", "applus-services/RV011APPLUS"),
    ("BBVA", "RV011BBV"),
    ("BIO", "RV011BIO"),
    ("BKIA", "RV011BANKIA"),
    ("BKT", "RV011BANKINT"),
    ("BME", "RV011BME"),
    ("CABK", "RV011CRITERI"),
    ("CAF", "cons-y-auxiliar-de-ferrocarriles/RV011CONSAUX"),
    ("CIE", "cie-automotive/RV011AZCOITI"),
    ("CLNX", "RV011CELLNEX"),
    ("COL", "RV011COLONIA"),
    ("DIA", "RV011DIA"),
    ("EBRO", "RV011EBREVA"),
    ("ECR", "RV011ERCROS"),
    ("EDR", "RV011EDREAM"),
    ("EKT", "RV011EUSKATE"),
    ("ELE", "RV011ENDESA"),
    ("ENC", "RV011ENCE"),
    ("ENG", "RV011ENAGAS"),
    ("FAE", "RV011FAES"),
    ("FCC", "RV011FOMENTO"),
    ("FDR", "RV011FLUIDRA"),
    ("FER", "RV011FERROVI"),
    ("GEST", "gestamp-automocion/RV011GESTAMP"),
    ("GRF", "RV011GRIFOLS"),
    ("GSJ", "RV011SANJOSE"),
    ("HIS", "RV011HISPA"),
    ("IAG", "RV011IAG"),
    ("IBE", "RV011IBERDRO"),
    ("IDR", "RV011INDRA"),
    ("ITX", "RV011INDITEX"),
    ("LGT", "RV011LINGOTE"),
    ("LOG", "RV011LOGISTA"),
    ("MAP", "RV011MAPFRE"),
    ("MEL", "RV011SOLMELI"),
    ("MRL", "RV011MERLINP"),
    ("MTS", "RV011MITTALE"),
    ("NAT", "RV011NATRA"),
    ("NHH", "RV011NHHOTEL"),
    ("NTGY", "naturgy/RV011GASNATU"),
    ("NTH", "RV011NATURH"),
    ("OHL", "RV011OHL"),
    ("PHM", "RV011ZELTIA"),
    ("PRS", "RV011PRISA"),
    ("PSG", "RV011PROSEGU"),
    ("QBT", "quabit/RV011ASTROC"),
    ("REE", "RV011REE"),
    ("REP", "RV011REPSOL"),
    ("SAB", "RV011BKSABAD"),
    ("SAN", "RV011BSCH"),
    ("SCYR", "RV011VALLEHE"),
    ("SGRE", "RV011GAMESA"),
    ("SLR", "RV011SOLARIA"),
    ("TEF", "RV011TELEFON"),
    ("TL5", "RV011TELECIN"),
    ("TLGO", "RV011TALGO"),
    ("TRE", "RV011TECREU"),
    ("TUB", "RV011TUBACEX"),
    ("VID", "vidrala/RV011VIDRALA"),
    ("VIS", "RV011VISCOFA"),
    ("VOC", "RV011VOCENTO"),
    ("ZOT", "RV011ZARDOYA"),
---------------------
    ("MDF", "duro-felguera/RV011DUROFEL"),
    ("PVA", "pescanova/RV011PESCANO")
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
  Just c -> printf ("https://www.invertia.com/es/mercados/bolsa/" ++
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
