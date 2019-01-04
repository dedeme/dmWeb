-- Copyright 19-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Trade parameters data

module Data.Params (
  Params(..),
  toJs,
  fromJs,
  Data.Params.init,
  readCurrent,
  readNickParams,
  readParams,
  writeNickParams
  ) where

import Data.List
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.File as File
import qualified Dm.Cgi as Cgi
import qualified Global as G

-- | Annotation type
data Params = Params {
  days :: Int,
  buyStrip :: Double,
  sellString :: Double
} deriving (Show)

path :: FilePath
path = G.path["data", "params.db"]

-- |@'toJs' ps@ - Returns /ps/ JSONized
toJs :: Params -> JSValue
toJs (Params d bs ss) = Js.wList[Js.wInt d, Js.wDouble bs, Js.wDouble ss]

-- |@'fromJs' js@ - Returns a Params which was JSONized
fromJs :: JSValue -> Params
fromJs js = let [d, bs, ss] = Js.rList js
            in  Params (Js.rInt d) (Js.rDouble bs) (Js.rDouble ss)

-- |@'init'@ - Initilizes /params.db/
init :: IO ()
init = File.write path "[]"

-- |@'readCurrent'@ - Returns current parameters
readCurrent :: IO Params
readCurrent = do
  fleasCf <- File.read (G.fleasDir ++ "/conf.db")
  let cf = Js.rMap $ Js.fromStr fleasCf
  let minDays = Cgi.get cf Js.rInt "min_days"
  let maxDays = Cgi.get cf Js.rInt "max_days"
  let maxStrips = Cgi.get cf Js.rDouble "max_strip"
  let bests = G.fleasDir ++ "/bests"
  ls <- File.dir bests
  let (db:_) = reverse $ sort ls
  let path = bests ++ "/" ++ db
  d <- File.read path
  let (result:_) = reverse $ Js.rList (Js.fromStr d)
  let [_, frs, _, _] = Js.rList result
  let [flea, _ , _, _] = Js.rList frs
  let [_, ps] = Js.rList flea
  let [d, bs, ss] = Js.rList ps
  return $ Params (days (Js.rDouble d) minDays maxDays)
                  (Js.rDouble bs * maxStrips) (Js.rDouble ss * maxStrips)
  where
    days d n x = n + (truncate $ (d * fromIntegral (x - n))::Int)

-- |@'readNickParams' nick@ - Returns (nick, parameters) of a sold /nick/
readNickParams :: String -> IO (Maybe (String, Params))
readNickParams nick = do
  js <- File.read path
  let nps = map
              (\js -> let [nk, p] = Js.rList js
                      in  (Js.rString nk, fromJs p))
              (Js.rList $ Js.fromStr js)

  let ffind nk (nk', _) = nk == nk'
  return $ find (ffind nick) nps

-- |@'readParams' nick@ - Returns parameters of a sold /nick/
readParams :: String -> IO (Maybe Params)
readParams nick = do
  r <- readNickParams nick
  return $  case r of
              Nothing -> Nothing
              Just (_, ps) -> Just ps

writeNickParams :: [(String, Params)] -> IO()
writeNickParams nps = File.write path $ Js.toStr $ Js.wList $
  map (\(n, p) -> Js.wList [Js.wString n, toJs p]) nps
