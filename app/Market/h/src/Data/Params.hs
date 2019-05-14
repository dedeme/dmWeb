-- Copyright 19-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Trade parameters data

module Data.Params (
  Params(..),
  toJs,
  fromJs,
  Data.Params.init,
  readBase,
  writeBase
) where

import Data.List
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.File as File
import qualified Dm.Cgi as Cgi
import qualified Global as G

-- | Annotation type
data Params = Params {
  step :: Double
} deriving (Show)

basePath :: FilePath
basePath = G.path["data", "baseParams.db"]

-- |@'toJs' ps@ - Returns /ps/ JSONized
toJs :: Params -> JSValue
toJs (Params step) = Js.wList[Js.wDouble step]

-- |@'fromJs' js@ - Returns a Params which was JSONized
fromJs :: JSValue -> Params
fromJs js = let [step] = Js.rList js
            in  Params (Js.rDouble step)

-- |@'init'@ - Initilizes /params.db/
init :: IO ()
init = do
  File.write basePath "[]"

-- |@'readCurrent'@ - Returns current parameters
readCurrent :: IO Params
readCurrent = do
  bests <- File.read (G.fleasDir ++ "/_bests/AppMM1B.db")
  case Js.rList $ Js.fromStr bests of
    [] -> return $ Params 1.8752169e-2
    (flea:_) -> let fields = Js.rList flea
                in  return $ fromJs (fields !! 2)

-- |@'readBase'@ - Returns base parameters
readBase :: IO Params
readBase = do
  ps <- File.read basePath
  case ps of
    "[]" -> readCurrent
    _ -> return $ fromJs $ Js.fromStr ps

-- |@'writeBase' ps@ - Writes base parameters
writeBase :: Params -> IO()
writeBase p = File.write basePath $ Js.toStr $ toJs p
