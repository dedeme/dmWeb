-- Copyright 24-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Source Tree.

module Com.STree
  ( T(..)
  , toJs
  , fromJs
  ) where

import qualified Dm.Js as Js
import Dm.Result

--- T
--- data T = SDir String [T]  -- SDir name contents
---        | SFile String String -- SFile name(witout extension) documentation
data T = SDir String [T]  -- SDir name contents
       | SFile String String -- SFile name(witout extension) documentation

--- toJs stree
toJs :: T -> Js.T
toJs (SDir n ls) = Js.wa [Js.wb True, Js.ws n, Js.wList toJs ls]
toJs (SFile n d) = Js.wa [Js.wb False, Js.ws n, Js.ws d]

fromJs :: Js.T -> Result T
fromJs js = case Js.ra js of
              Right [tp, name, val] ->
                Js.rb tp >>=
                  ( \tp' ->
                    if tp'
                      then
                        Js.rs name >>=
                          \name' -> Js.rList fromJs val >>=
                            \val' -> Right $ SDir name' val'
                      else
                        Js.rs name >>=
                          \name' -> Js.rs val >>=
                            \val' -> Right $ SFile name' val'
                  )
              _ -> Left "STree.fromJs: Bad JSON"
