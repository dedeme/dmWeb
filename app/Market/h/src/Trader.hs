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

readCloses :: String -> IO [Double]
readCloses nk = do
  qs <- File.read (G.quotesBase ++ "/quotes/" ++ nk ++ ".db")
  return $ map fMap $ filter (/= "") $ reverse $ lines qs
  where
    fMap s =  let (':':s0) = dropWhile (/= ':') s
                  (':':s1) = dropWhile (/= ':') s0
                  cl = takeWhile (/= ':') s1
              in  read cl

calc :: Double -> [Double] -> Params -> (Double, [(Double, Double)])
calc _ [] _ = (0, [])
calc bet qs@(begin:before) (Params d bs ss) =
  c' 0 [] 0 0 begin (-1) before (drop d qs) False
  where
    c' rpr rqs st pr begin ref before after buying =
      let ref' =  if ref > 0
                  then
                    if begin > 0
                      then
                        if buying
                        then if begin < ref then begin else ref
                        else if begin > ref then begin else ref
                      else ref
                  else
                    if begin > 0 then begin else ref
      in c rpr rqs st pr ref' before after buying
    c rpr rqs _ _ _ _ [] _ = (rpr, reverse $ take 250 rqs)
    c rpr rqs st pr ref (b:before) (a:after) True =
      if a < 0 || ref < 0
      then
        c' rpr rqs st pr b ref before after True
      else
        let dif = (a - ref) / ref
        in  if dif > bs
            then c' rpr ((a, refb ref):rqs) (stBuy a) a b b before after False
            else c' rpr ((a, refb ref):rqs) st pr b ref before after True
    c rpr rqs st pr ref (b:before) (a:after) False =
      if a < 0 || ref < 0
      then
        c' rpr rqs st pr b ref before after False
      else
        let dif = (ref - a) / ref
        in  if dif > ss
            then c' (rpr + prof st pr a) ((a, refs ref):rqs)
                   0 0 b b before after True
            else c' rpr ((a, refs ref):rqs) st pr b ref before after False
    stBuy pr = truncate $ bet / pr
    refb q = q + q * bs
    refs q = q - q * ss
    prof st bpr spr = (spr - bpr) * (fromIntegral st)

-- |@'calculate' nick@ - Returns an object with next data: {
--                       profits: Double (percentage),
--                       quotes: [[Double (close), Double (ref)]]}
calculate :: String -> IO [(String, JSValue)]
calculate nk = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  paramsMb <- Params.readParams nk
  params <- case paramsMb of
              Nothing -> Params.readBase
              Just ps -> return ps
  qs <- readCloses nk
  return $ toJs bet $ calc bet (filter (>= 0) qs) params
  where
    toJs bet (profits, qs) = [
      ("profits", Js.wDouble (profits / bet)),
      ("quotes", Js.wList $ map toJs2 qs)
      ]
    toJs2 (q, ref) = Js.wList [Js.wDouble q, Js.wDouble ref]
