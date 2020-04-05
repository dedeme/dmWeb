-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Path entry data.

module Com.PathEntry
  ( T(..)
  , new
  , toJs
  , fromJs
  ) where

import qualified Dm.Js as Js
import Dm.Result

--- T
data T = T
          { name :: String
          , path :: String
          , selected :: Bool
          , exists :: Bool
          }

--- new name path
new :: String -> String -> T
new ename epath = T ename epath True False

--- toJs e
toJs :: T -> Js.T
toJs (T n p s e) = Js.wa $ [ Js.ws n, Js.ws p, Js.wb s, Js.wb e ]

--- fromJs js
fromJs :: Js.T -> Result T
fromJs js =
  Js.ra js >>=
  \a ->       Js.rs (a!!0) >>=
        \n -> Js.rs (a!!1) >>=
        \p -> Js.rb (a!!2) >>=
        \s -> Js.rb (a!!3) >>=
        \e -> Right $ T n p s e
