-- Copyright 20-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Profits calculator

module Trader (calculate) where

import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.File as File
import qualified Dm.Cgi as Cgi
import qualified Data.Pf as Pf
import Data.Pf (Pf)
import qualified Data.Params as Params
import Data.Params (Params (..))
import qualified Orders as Orders
import qualified Conf as Conf
import qualified Global as G

readOpenCloses :: String -> IO [(Double, Double)]
readOpenCloses nk = do
  qs <- File.read (G.quotesBase ++ "/quotes/" ++ nk ++ ".db")
  return $ map fMap $ filter (/= "") $ reverse $ lines qs
  where
    fMap s =  let (':':s0) = dropWhile (/= ':') s
                  op = takeWhile (/= ':') s0
                  (':':s1) = dropWhile (/= ':') s0
                  cl = takeWhile (/= ':') s1
              in  (read op, read cl)

calc :: Double -> [(Double, Double)] -> Params -> (Double, [(Double, Double)])
calc bet qs ps = (bet, reverse $ take 250 $ reverse qs)

-- |@'calculate' nick@ - Returns an object with next data: {
--                       profits: Double (percentage),
--                       quotes: [[Double (close), Double (ref)]]}
calculate :: String -> IO [(String, JSValue)]
calculate nk = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  paramsMb <- Params.readParams nk
  params <- case paramsMb of
              Nothing -> Params.readCurrent
              Just ps -> return ps
  qs <- readOpenCloses nk
  return $ toJs bet $ calc bet (filter f qs) params
  where
    f (o, c) = (o >= 0) && (c >= 0)
    toJs bet (profits, qs) = [
      ("profits", Js.wDouble (profits / bet)),
      ("quotes", Js.wList $ map toJs2 qs)
      ]
    toJs2 (q, ref) = Js.wList [Js.wDouble q, Js.wDouble ref]
