-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Yahoo page

module Data.Servers.Yahoo (quote, uri) where

import Control.Exception
import Text.Printf (printf)
import qualified Data.Servers.Reader as Reader

codes :: [(String, String)]
codes = [
    ("A3M", "A3M.MC"),
    ("ACS", "ACS.MC"),
    ("ACX", "ACX.MC"),
    ("ADZ", "ADZ.MC"),
    ("AENA", "AENA.MC"),
    ("ALM", "ALM.MC"),
    ("AMS", "AMS.MC"),
    ("ANA", "ANA.MC"),
    ("APPS", "APPS.MC"),
    ("BBVA", "BBVA.MC"),
    ("BIO", "BIO.MC"),
    ("BKIA", "BKIA.MC"),
    ("BKT", "BKT.MC"),
    ("BME", "BME.MC"),
    ("CABK", "CABK.MC"),
    ("CAF", "CAF.MC"),
    ("CIE", "CIE.MC"),
    ("CLNX", "CLNX.MC"),
    ("COL", "COL.MC"),
    ("DIA", "DIA.MC"),
    ("EBRO", "EBRO.MC"),
    ("ECR", "ECR.MC"),
    ("EDR", "EDR.MC"),
    ("EKT", "EKT.MC"),
    ("ELE", "ELE.MC"),
    ("ENC", "ENC.MC"),
    ("ENG", "ENG.MC"),
    ("FAE", "FAE.MC"),
    ("FCC", "FCC.MC"),
    ("FDR", "FDR.MC"),
    ("FER", "FER.MC"),
    ("GEST", "GEST.MC"),
    ("GRF", "GRF.MC"),
    ("GSJ", "GSJ.MC"),
    ("HIS", "HIS.MC"),
    ("IAG", "IAG.MC"),
    ("IBE", "IBE.MC"),
    ("IDR", "IDR.MC"),
    ("ITX", "ITX.MC"),
    ("LGT", "LGT.MC"),
    ("LOG", "LOG.MC"),
    ("MAP", "MAP.MC"),
    ("MEL", "MEL.MC"),
    ("MRL", "MRL.MC"),
    ("MTS", "MTS.MC"),
    ("NAT", "NAT.MC"),
    ("NHH", "NHH.MC"),
    ("NTGY", "NTGY.MC"),
    ("NTH", "NTH.MC"),
    ("OHL", "OHL.MC"),
    ("PHM", "PHM.MC"),
    ("PRS", "PRS.MC"),
    ("PSG", "PSG.MC"),
    ("QBT", "QBT.MC"),
    ("REE", "REE.MC"),
    ("REP", "REP.MC"),
    ("SAB", "SAB.MC"),
    ("SAN", "SAN.MC"),
    ("SCYR", "SCYR.MC"),
    ("SGRE", "SGRE.MC"),
    ("SLR", "SLR.MC"),
    ("TEF", "TEF.MC"),
    ("TL5", "TL5.MC"),
    ("TLGO", "TLGO.MC"),
    ("TRE", "TRE.MC"),
    ("TUB", "TUB.MC"),
    ("VID", "VID.MC"),
    ("VIS", "VIS.MC"),
    ("VOC", "VOC.MC"),
    ("ZOT", "ZOT.MC"),
---------------------
    ("MDF", "MDF.MC"),
    ("PVA", "PVA.MC")
  ]

-- |@'quote' nick@ - Returns the current quote of /nick/ or -1 if it can not
--                   be read.
quote :: String -> IO Double
quote nick = do
  html <- Reader.read $ uri nick
  q html

-- |@'uri' nick@ - Returns the url of nick web page.
uri :: String -> String
uri nick = case lookup nick codes of
  Nothing -> "http://localhost"
  Just c -> printf "https://es.finance.yahoo.com/quote/%s?p=%s" c c

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
