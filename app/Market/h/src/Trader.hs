-- Copyright 20-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Profits calculator

module Trader (calculate, lastRef) where

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

readCloses :: String -> IO ([String], [Double])
readCloses nk = do
  qs <- File.read (G.quotesBase ++ "/quotes/" ++ nk ++ ".db")
  let pairs = filter (\(_, cl) -> cl >= 0)
                     (map fMap $ filter (/= "") $ reverse $ lines qs)
  return $ (map (\(d, _) -> d) pairs, map (\(_, cl) -> cl) pairs)
  where
    fMap s =  let d = takeWhile (/= ':') s
                  (':':s0) = dropWhile (/= ':') s
                  (':':s1) = dropWhile (/= ':') s0
                  cl = takeWhile (/= ':') s1
              in  (d, read cl)

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

-- |@'calculate' nick@ - Returns an object whose values are:
--
-- * "profits": Double (percentage),
-- * "quotes": [[String (date), Double (close), Double (ref)]]
calculate :: String -> IO [(String, JSValue)]
calculate nk = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  paramsMb <- Params.readParams nk
  params <- case paramsMb of
              Nothing -> Params.readBase
              Just ps -> return ps
  (ds, qs) <- readCloses nk
  return $ toJs bet (reverse $ take 250 $ reverse ds) $ calc bet qs params
  where
    toJs bet ds (profits, qs) = [
      ("profits", Js.wDouble (profits / bet)),
      ("quotes", Js.wList $ map toJs2 qs),
      ("dates", Js.wList $ map Js.wString ds)
      ]
    toJs2 (q, ref) = Js.wList [Js.wDouble q, Js.wDouble ref]

-- |@'lastRef' nick@ returns the last risk reference (with strip applied) of
--                   'nick'
lastRef :: String -> IO Double
lastRef nk = do
  if nk == "PVA" || nk == "MDF" || nk == "DIA"
  then return 0
  else do
    conf <- Conf.get
    let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
    paramsMb <- Params.readParams nk
    params <- case paramsMb of
                Nothing -> Params.readBase
                Just ps -> return ps
    (_, qs) <- readCloses nk
    let (_, ls) = calc bet (filter (>= 0) qs) params
    let ((_, rf):_) = reverse ls
    return rf
