-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Annotation data

module Data.Ann (
  Ann (..),
  Data.Ann.id,
  setId,
  date,
  toJs,
  fromJs
  ) where

import qualified Dm.Js as Js
import Dm.Js (JSValue)

-- | Annotation type
data Ann =
  -- |@'Sell' id date nick stocks price@ - Stocks sell
  Sell Int String String Int Double |
  -- |@'Buy' id date nick stocks price@ - Stocks buy
  Buy Int String String Int Double |
  -- |@'Withdrawal' id date money@ - Cash withdrawal
  Withdrawal Int String Double |
  -- |@'Income' id date money@ - Cash income
  Income Int String Double |
  -- |@'Profits' id date money description@ - Dividends and similar
  Profits Int String Double String |
  -- |@'Fees' id date money description@ - Direct broker fees, out of operations
  Fees Int String Double String |
  -- |@'Pdif' id date money description@ - Positive cash correction
  Pdif Int String Double String |
  -- |@'Ndif' id date money description@ - Negative cash correction
  Ndif Int String Double String |
  -- |@'Close' id date@ - Year close
  Close Int String
  deriving (Eq, Show)

-- |@'id' a@ - Returns the idenfitier of /a/
id :: Ann -> Int
id (Sell i _ _ _ _) = i
id (Buy i _ _ _ _) = i
id (Withdrawal i _ _) = i
id (Income i _ _) = i
id (Profits i _ _ _) = i
id (Fees i _ _ _) = i
id (Pdif i _ _ _) = i
id (Ndif i _ _ _) = i
id (Close i _) = i

-- |@'setId' id a@ - Sets the idenfitier of /a/
setId :: Int -> Ann -> Ann
setId id (Sell _ a b c d) = Sell id a b c d
setId id (Buy _ a b c d) = Buy id a b c d
setId id (Withdrawal _ a b) = Withdrawal id a b
setId id (Income _ a b) = Income id a b
setId id (Profits _ a b c) = Profits id a b c
setId id (Fees _ a b c) = Fees id a b c
setId id (Pdif _ a b c) = Pdif id a b c
setId id (Ndif _ a b c) = Ndif id a b c
setId id (Close _ a) = Close id a

-- |@'date' a@ - Returns the date of /a/
date :: Ann -> String
date (Sell _ d _ _ _) = d
date (Buy _ d _ _ _) = d
date (Withdrawal _ d _) = d
date (Income _ d _) = d
date (Profits _ d _ _) = d
date (Fees _ d _ _) = d
date (Pdif _ d _ _) = d
date (Ndif _ d _ _) = d
date (Close _ d) = d

-- |@'toJs' ann@ - Returns /ann/ JSONized
toJs :: Ann -> JSValue
toJs (Sell i d n st pr) = Js.wList[Js.wString "se",
                                   Js.wInt i,
                                   Js.wString d,
                                   Js.wString n,
                                   Js.wInt st,
                                   Js.wDouble pr]
toJs (Buy i d n st pr) = Js.wList[Js.wString "bu",
                                  Js.wInt i,
                                  Js.wString d,
                                  Js.wString n,
                                  Js.wInt st,
                                  Js.wDouble pr]
toJs (Withdrawal i d m) = Js.wList[Js.wString "wi",
                                   Js.wInt i,
                                   Js.wString d,
                                   Js.wDouble m]
toJs (Income i d m) = Js.wList[Js.wString "in",
                               Js.wInt i,
                               Js.wString d,
                               Js.wDouble m]
toJs (Profits i d m des) = Js.wList[Js.wString "pr",
                                    Js.wInt i,
                                    Js.wString d,
                                    Js.wDouble m,
                                    Js.wString des]
toJs (Fees i d m des) = Js.wList[Js.wString "fe",
                                 Js.wInt i,
                                 Js.wString d,
                                 Js.wDouble m,
                                 Js.wString des]
toJs (Pdif i d m des) = Js.wList[Js.wString "pd",
                                 Js.wInt i,
                                 Js.wString d,
                                 Js.wDouble m,
                                 Js.wString des]
toJs (Ndif i d m des) = Js.wList[Js.wString "nd",
                                 Js.wInt i,
                                 Js.wString d,
                                 Js.wDouble m,
                                 Js.wString des]
toJs (Close i d) = Js.wList[Js.wString "cl",
                            Js.wInt i,
                            Js.wString d]

-- |@'fromJs' js@ - Returns an Ann which was JSONized
fromJs :: JSValue -> Ann
fromJs js =
  let (tp:ps) = Js.rList js
  in  case Js.rString tp of
        "se" -> let [i, d, n, st, pr] = ps
                in  Sell (Js.rInt i)
                         (Js.rString d)
                         (Js.rString n)
                         (Js.rInt st)
                         (Js.rDouble pr)
        "bu" -> let [i, d, n, st, pr] = ps
                in  Buy (Js.rInt i)
                        (Js.rString d)
                        (Js.rString n)
                        (Js.rInt st)
                        (Js.rDouble pr)
        "wi" -> let [i, d, m] = ps
                in  Withdrawal (Js.rInt i) (Js.rString d) (Js.rDouble m)
        "in" -> let [i, d, m] = ps
                in  Income (Js.rInt i) (Js.rString d) (Js.rDouble m)
        "pr" -> let [i, d, m, des] = ps
                in  Profits (Js.rInt i) (Js.rString d)
                            (Js.rDouble m) (Js.rString des)
        "fe" -> let [i, d, m, des] = ps
                in  Fees (Js.rInt i) (Js.rString d)
                         (Js.rDouble m) (Js.rString des)
        "pd" -> let [i, d, m, des] = ps
                in  Pdif (Js.rInt i) (Js.rString d)
                         (Js.rDouble m) (Js.rString des)
        "nd" -> let [i, d, m, des] = ps
                in  Ndif (Js.rInt i) (Js.rString d)
                         (Js.rDouble m) (Js.rString des)
        "cl" -> let [i, d] = ps in Close (Js.rInt i) (Js.rString d)
        s -> error $ "'" ++ s ++ "' unknown Ann"
