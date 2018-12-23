-- Copyright 19-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Trade parameters data

module Data.Params (
  Params(..),
  toJs,
  fromJs
  ) where

import qualified Dm.Js as Js
import Dm.Js (JSValue)

-- | Annotation type
data Params = Params {
  days :: Int,
  buyStrip :: Double,
  sellString :: Double
} deriving (Show)

-- |@'toJs' ps@ - Returns /ps/ JSONized
toJs :: Params -> JSValue
toJs (Params d bs ss) = Js.wList[Js.wInt d, Js.wDouble bs, Js.wDouble ss]

-- |@'fromJs' js@ - Returns a Params which was JSONized
fromJs :: JSValue -> Params
fromJs js = let [d, bs, ss] = Js.rList js
            in  Params (Js.rInt d) (Js.rDouble bs) (Js.rDouble ss)
