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

readCloses :: String -> IO ([String], [Double], [Double])
readCloses nk = do
  qs <- File.read (G.quotesBase ++ "/quotes/" ++ nk ++ ".db")
  let ls = filter (\(_, op, cl) -> op >= 0 && cl >= 0)
                     (map fMap $ filter (/= "") $ reverse $ lines qs)
  return $ (
    map (\(d, _, _) -> d) ls,
    map (\(_, op, _) -> op) ls,
    map (\(_, _, cl) -> cl) ls)
  where
    fMap s =  let d = takeWhile (/= ':') s
                  (':':s0) = dropWhile (/= ':') s
                  op = takeWhile (/= ':') s0
                  (':':s1) = dropWhile (/= ':') s0
                  cl = takeWhile (/= ':') s1
              in  (d, read op, read cl)

calc :: Double -> [Double] -> [Double] -> Params -> Double ->
        (Double, [(Double, Double)], [(Bool, Int, Double)])
calc bet opens closes@(c:_) (Params step) force =
  cl (0, [], []) (0, 0) True (c * 0.95) c c c c opens closes
  where
  cl :: (Double, [(Double, Double)], [(Bool, Int, Double)]) -> (Int, Double) ->
        Bool -> Double -> Double -> Double -> Double -> Double ->
        [Double] -> [Double] ->
        (Double, [(Double, Double)], [(Bool, Int, Double)])
  cl (profits, ls, hs) _ _ _ _ _ _ _ [] _ =
    (profits, reverse $ take 250 ls, reverse hs)
  cl (profits, ls, hs) acc@(stocks, price)
                       toSell ref mm pmm1 pmm2 coq
                       (o:os) (c:cs)  =
    if toSell
    then
      let ref' = ref + (c - ref) * step
          ls' = (c, ref'):ls in
        if c <= ref'
        then
          let q = case os of [] -> c; (o':_) -> o'
              hs' = (True, stocks, q):hs
              opProf = q * 0.997 - price * 1.003
          in  cl (profits + fromIntegral stocks * opProf, ls', hs') (0, 0)
              False
              (if  c > coq || mm > pmm2 then mm else pmm2)
              c mm pmm1 c os cs
        else
          let mm' = if c > mm then c else mm
          in  cl (profits, ls', hs) acc True ref' mm' pmm1 pmm2 coq os cs
    else
      let ref' = ref - (ref - c) * step
          ls' = (c, ref'):ls in
        if c >= ref' || c == force
        then
          case os of
          [] -> cl (profits, ls', hs) acc True
                    (if c < coq || mm < pmm2 then mm else pmm2)
                    c mm pmm1 c os cs
          (o':_) ->
            let stocks = truncate(bet / c)
                hs' = (False, stocks, o'):hs
            in  cl (profits, ls', hs') (stocks, o') True
                    (if c < coq || mm < pmm2 then mm else pmm2)
                    c mm pmm1 c os cs
        else
          let mm' = if c < mm then c else mm
          in  cl (profits, ls', hs) acc False ref' mm' pmm1 pmm2 coq os cs

-- |@'calculate' nick@ - Returns an object whose values are:
--
-- * "profits": Double (percentage),
-- * "quotes": [[Double (close), Double (ref)]]
-- * "dates": [String(date)]
calculate :: String -> IO [(String, JSValue)]
calculate nk = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  params <- Params.readBase
  (ds, os, cs) <- readCloses nk
  return $ toJs bet (reverse $ take 250 $ reverse ds) $
    calc bet os cs params (G.force nk)
  where
    toJs bet ds (profits, qs, hs) = [
      ("profits", Js.wDouble (profits / bet)),
      ("quotes", Js.wList $ map toJs2 qs),
      ("historic", Js.wList $ map toJs3 hs),
      ("dates", Js.wList $ map Js.wString ds)
      ]
    toJs2 (q, ref) = Js.wList [Js.wDouble q, Js.wDouble ref]
    toJs3 (isSell, s, p) = Js.wList [Js.wBool isSell, Js.wInt s, Js.wDouble p]

-- |@'lastRef' nick@ returns the last risk reference (with strip applied) of
--                   'nick'
lastRef :: String -> IO Double
lastRef nk = do
  if nk == "PVA"
  then return 0
  else do
    conf <- Conf.get
    let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
    params <- Params.readBase
    (_, os, cs) <- readCloses nk
    let (_, ls, _) = calc bet os cs params (G.force nk)
    let ((_, rf):_) = reverse ls
    return rf
