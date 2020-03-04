-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Capcha widget.

module Dm.Captcha
  ( Init(..)
  , T
  , new
  , wg
  , isZero
  , isOutOfLimit
  , inc
  , reset
  , check
  , newCombination
  ) where

import qualified Data.Var as Var
import Dm.Dom (Element, Property(..), q, q', isChecked)
import Data.Function (fmap)
import qualified Dm.Time as Time
import qualified Dm.Storage as Storage
import qualified Dm.Maybe as Maybe
import qualified Dm.Either as Either
import qualified Dm.Js as Js
import qualified Dm.Rnd as Rnd

--- Init
---  { storeId :: String
---  , counterLimit :: Maybe Int
---  , timeLimit :: Maybe Int
---  , zeroColor :: Maybe String
---  , oneColor :: Maybe String
---  }
---   storeId     : Local Store identifier. Names 'storeId' + "_counter" and
---                 'storeId + "_time' will be used.
---   counterLimit: Number of tries before showing captcha widget. Default 3.
---   timeLimit   : Milliseconds after which captcha counter is set to 0.
---                 Default 900000 (15 minutes)
---   zeroColor   : Color of cells to not mark.
---   oneColor    : color of cells to mark-.
data Init = Init
  { storeId :: String
  , counterLimit :: Maybe Int
  , timeLimit :: Maybe Int
  , zeroColor :: Maybe String
  , oneColor :: Maybe String
  }

--- T
--- Captcha type
data T = T
  { sId :: String
  , cLimit :: Int
  , tLimit :: Int
  , counter :: Var.Ref Int
  , combination :: Var.Ref [Int]

  , wg :: Element
  , zColor :: String
  , oColor :: String
  , checks :: [Element]
  }

data Cmd = Inc | Reset | NewCombination

-- PRIVATE

getCounter :: String -> Fay Int
getCounter sid = do
  now <- Time.now
  tm <- Storage.get (sid ++ "_time")
  ct <- Storage.get (sid ++ "_counter")
  case getTime tm of
    Nothing -> return 0
    Just t -> if Time.df now t > 0 then return 0
              else return $ Maybe.fromMaybe 0 $ getCounter' ct
  where
    getTime tm = tm Maybe.>>= \s ->
                   (Either.toMaybe $ Js.fromStr s Either.>>= Time.fromJs)
    getCounter' ct = ct Maybe.>>= \s ->
                       (Either.toMaybe $ Js.fromStr s Either.>>= Js.ri)

-- CONSTRUCTOR

--- new ini
--- Constructor
new :: Init -> Fay T
new ini = do
  counter <- getCounter (storeId ini) >>= Var.newRef
  combination <- fmap (take 4) (Rnd.shuffle [0..7::Int]) >>= Var.newRef
  wg <- q "div" [] []
  checks <- forM [1..8::Int]
                 (\_ -> q "input" [Att "type" "checkbox"] [] >>= return)
  let r = T
            { sId = storeId ini
            , cLimit = Maybe.fromMaybe 3 $ counterLimit ini
            , tLimit = Maybe.fromMaybe 900000 $ timeLimit ini
            , counter
            , combination

            , wg
            , zColor = Maybe.fromMaybe "#f0f0f0" $ zeroColor ini
            , oColor = Maybe.fromMaybe "#c0c0c0" $ oneColor ini
            , checks
            }
  view r
  return r

-- INFORMATION

--- isZero cp
--- Returns 'True' if captcha counter is zero.
isZero :: T -> Fay Bool
isZero cp = fmap (== 0) $ Var.get (counter cp)

--- isOutOfLimit cp
--- Returns 'True' if captcha counter is greater than its counter limit.
isOutOfLimit :: T -> Fay Bool
isOutOfLimit cp = fmap (>= (cLimit cp)) $ Var.get (counter cp)

--- check cp
--- Returns 'True' if checkboxes are correctly marked.
check :: T -> Fay Bool
check cp = do
  comb <- Var.get (combination cp)
  return $ all (eq comb) $ zip [(0::Int)..] (checks cp)
  where
    eq cb (ix, ck) = any (== ix) cb == isChecked ck

-- VIEW

view :: T -> Fay ()
view model = do
  mapM_ (\ck -> q' ck [Checked False] [] >> return ()) (checks model)
  tds <- mapM (\ck -> q "td" [] [return ck] >>= return) (checks model)
  mapM_ setTd $ zip [(0::Int)..] tds
  let row1 = map return $ take 4 tds
  let row2 = map return $ drop 4 tds
  _ <- q' (wg model) [RemoveAll]
        [ q "table" [Style "border: 1px solid;background-color: #fffff0"]
            [ q "tr" [] row1
            , q "tr" [] row2
            ]
        ]
  return ()
  where
    setTd (ix, td) = do
      comb <- Var.get (combination model)
      let color = case find (ix ==) comb of
                    Nothing -> zColor model
                    _ -> oColor model
      let st = "border: 1px solid;background-color:" ++ color
      q' td [ Style st] [] >> return ()


-- UPDATE

update :: T -> Cmd -> Fay ()
update cp cmd =
  case cmd of
    Inc -> do
      oldCt <- getCounter $ sId cp
      now <- Time.now
      Storage.set (sId cp ++ "_time") $
        Js.toStr $ Time.toJs $ Time.add (tLimit cp) now
      let ct = oldCt + 1
      Storage.set (sId cp ++ "_counter") $ Js.toStr $ Js.wi ct
      Var.set (counter cp) ct
    Reset -> do
      Storage.set (sId cp ++ "_counter") $ Js.toStr $ Js.wi 0
      Var.set (counter cp) 0
    NewCombination -> do
      newComb <- fmap (take 4) (Rnd.shuffle [0..7::Int])
      Var.set (combination cp) newComb
      view cp

--- inc cp
--- Increments captcha counter.
inc :: T -> Fay ()
inc cp = update cp Inc

--- reset cp
--- Sets captcha counter to 0.
reset :: T -> Fay ()
reset cp = update cp Reset

--- newCombination
--- Changes checkboxes combination and modifies view.
newCombination :: T -> Fay()
newCombination cp = update cp NewCombination

