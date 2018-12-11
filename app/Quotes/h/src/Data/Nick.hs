-- Copyright 03-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Nick data

module Data.Nick (
  Nick (..),
  new,
  toJs,
  fromJs
  ) where

import qualified Dm.Js as Js
import Dm.Js (JSValue)

-- | Nick data
data Nick = Nick {
  id :: String,
  name :: String,
  isIbex :: Bool,
  isSel :: Bool
}

-- | @'new' id name@ - Creates a new Nick
new :: String -> String -> Nick
new id name = Nick id name False False

-- | @'toJs' nk@ - Parses /nk/ to JSON.
toJs :: Nick -> JSValue
toJs (Nick id name isIbex isSel) = Js.wList [
    Js.wString id, Js.wString name,
    Js.wBool isIbex, Js.wBool isSel
  ]

-- | @'fromJs' js@ - Retrieves a Nick JSONized.
fromJs :: JSValue -> Nick
fromJs js = let [id, name, isIbex, isSel] = Js.rList js
                in  Nick (Js.rString id) (Js.rString name)
                         (Js.rBool isIbex) (Js.rBool isSel)
