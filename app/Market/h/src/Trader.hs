-- Copyright 20-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Profits calculator

module Trader (calculate, lastRef) where

import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.File as File
import qualified Dm.Cgi as Cgi
import qualified Data.Params as Params
import Data.Params (Params (..))
import qualified Orders as Orders
import qualified Conf as Conf
import qualified Global as G

data QRef = QRef {qrefQ :: Double, qrefR :: Double}

data Op = Op {opIsSell :: Bool, opSt :: Int, opPr :: Double}

data Pf = Pf {pfSt :: Int, pfPr :: Double}

data Co = Co {
  coPf :: Pf, -- Bought Stocks
  coToS :: Bool, -- If is to sell
  coRef :: Double, -- Current strip reference
  coMm :: Double, -- Current maximum-minimum
  coPmm1 :: Double, -- Previous maximum-minimum
  coPmm2 :: Double, -- Preivious previous maximum-minimum
  coPc :: Double -- Close of the last position change
}

data RCalc = RCalc {rcPrf :: Double, rcQr :: [QRef], rcOp :: [Op]}

readQuotes :: String -> IO ([String], [Double], [Double])
readQuotes nk = do
  qs <- File.read (G.quotesBase ++ "/quotes/" ++ nk ++ ".db")
  return $ unzip3 $ filter isValidQuote $ map fMap $ reverse $ validQs qs
  where
    isValidQuote (_, op, cl) = op >= 0 && cl >= 0
    validQs = (filter (/= "") . lines)
    fMap s =  let d = takeWhile (/= ':') s
                  (':':s0) = dropWhile (/= ':') s
                  op = takeWhile (/= ':') s0
                  (':':s1) = dropWhile (/= ':') s0
                  cl = takeWhile (/= ':') s1
              in  (d, read op, read cl)

calc :: Double -> [Double] -> [Double] -> Params -> Double -> RCalc
calc bet opens closes@(c:_) (Params step) force =
  cl (RCalc 0 [] []) (Co (Pf 0 0) True (c * 0.95) c c c c) opens closes
  where
  cl :: RCalc -> Co -> [Double] -> [Double] -> RCalc
  cl (RCalc profits ls hs) _ [] _ =
    RCalc profits (reverse $ take 250 ls) (reverse hs)
  cl (RCalc profits ls hs)
     (Co acc@(Pf stocks price) toSell ref mm pmm1 pmm2 coPc)
     (o:os) (c:cs)  =
    if toSell
    then
      let ref' = ref + (c - ref) * step
          ls' = (QRef c ref'):ls in
        if c <= ref'
        then
          let q = case os of [] -> c; (o':_) -> o'
              hs' = (Op True stocks q):hs
              opProf = q * 0.997 - price * 1.003
              newRef = if  c > coPc || mm > pmm2 then mm else pmm2
          in  cl (RCalc (profits + fromIntegral stocks * opProf) ls' hs')
                 (Co (Pf 0 0) False newRef c mm pmm1 c)
                 os cs
        else
          let mm' = if c > mm then c else mm
          in  cl (RCalc profits ls' hs)
                 (Co acc True ref' mm' pmm1 pmm2 coPc)
                 os cs
    else
      let ref' = ref - (ref - c) * step
          ls' = (QRef c ref'):ls in
        if c >= ref' || c == force
        then
          let newRef = if c < coPc || mm < pmm2 then mm else pmm2
          in case os of
             [] -> cl (RCalc profits ls' hs)
                      (Co acc True newRef c mm pmm1 c)
                      os cs
             (o':_) ->
                let stocks = truncate(bet / c)
                    hs' = (Op False stocks o'):hs
                in  cl (RCalc profits ls' hs')
                       (Co (Pf stocks o') True newRef c mm pmm1 c)
                       os cs
        else
          let mm' = if c < mm then c else mm
          in  cl (RCalc profits ls' hs)
                 (Co acc False ref' mm' pmm1 pmm2 coPc)
                 os cs

-- |@'calculate' nick@ - Returns an object whose values are:
--
-- * "profits": Double (percentage),
-- * "quotes": [[Double (close), Double (ref)]]
-- * "historic": [[Bool (isSell), Int (stocks), Double (price)]]
-- * "dates": [String(date)]
calculate :: String -> IO [(String, JSValue)]
calculate nk = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  params <- Params.readBase
  (ds, os, cs) <- readQuotes nk
  return $ toJs bet (reverse $ take 250 $ reverse ds) $
    calc bet os cs params (G.force nk)
  where
    toJs bet ds (RCalc profits qs hs) = [
      ("profits", Js.wDouble (profits / bet)),
      ("quotes", Js.wList $ map toJs2 qs),
      ("historic", Js.wList $ map toJs3 hs),
      ("dates", Js.wList $ map Js.wString ds)
      ]
    toJs2 (QRef q ref) = Js.wList [Js.wDouble q, Js.wDouble ref]
    toJs3 (Op isS s p) = Js.wList [Js.wBool isS, Js.wInt s, Js.wDouble p]

-- |@'lastRef' nick@ returns the last risk reference (with strip applied) of
--                   'nick'
lastRef :: String -> IO Double
lastRef nk = do
  conf <- Conf.get
  let bet = Cgi.get (Js.rMap conf) Js.rDouble "bet"
  params <- Params.readBase
  (_, os, cs) <- readQuotes nk
  let (RCalc _ ls _) = calc bet os cs params (G.force nk)
  let QRef _ rf = last ls
  let lastC = last cs
  if rf < lastC then return rf
  else do
    return lastC
