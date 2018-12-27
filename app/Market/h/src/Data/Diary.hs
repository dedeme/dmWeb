-- Copyright 12-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Diary data

module Data.Diary (
  Data.Diary.init,
  add,
  remove,
  years,
  Data.Diary.take,
  books,
  yearBooks,
  Data.Diary.cash
  ) where

import Data.List
import Control.Monad (foldM)
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Data.Ann as Ann
import Data.Ann (Ann(..))
import qualified Data.Pf as Pf
import Data.Pf (Pf)
import qualified Data.Ledger as Ledger
import Data.Ledger (Ledger(..))
import qualified Fees as Fees
import qualified Global as G

path :: String -> String
path "" = G.path ["data", "Diaries"]
path year = G.path ["data", "Diaries", year]

data Dr = Dr {id :: Int, ann :: [Ann]}

toJs :: Dr -> JSValue
toJs (Dr id anns) = Js.wList [Js.wInt id, Js.wList $ map Ann.toJs anns]

fromJs :: JSValue -> Dr
fromJs js = let [id, anns] = Js.rList js
            in  Dr (Js.rInt id) (map Ann.fromJs $ Js.rList anns)

-- |@'init'@ - Initializes data base
init :: IO ()
init = File.mkDir $ path ""

readJs :: String -> IO (Int, JSValue)
readJs year = do
  let p = path year
  ex <- File.exists $ p
  if ex
  then do
    tx <- File.read (path year)
    let [id, anns] = Js.rList $ Js.fromStr tx
    return (Js.rInt id, anns)
  else return $ (0, Js.fromStr "[]")

read :: String -> IO (Int, [Ann])
read year = do
  (id, anns) <- readJs year
  return (id, map Ann.fromJs $ Js.rList anns)

write :: String-> Int -> [Ann] -> IO ()
write year id anns =
  File.write (path year) $ Js.toStr $ toJs $ Dr id anns

add2 date a date' a' [] = if date >= date' then [a, a'] else [a', a]
add2 date a date' a' (a'':anns) =
  if date >= date' then a:a':a'':anns
                   else a':(add2 date a (Ann.date a'') a'' anns)

add1 id a date anns =
  ad anns
  where
    ad [] = [Ann.setId id a]
    ad (a':anns) = add2 date (Ann.setId id a) (Ann.date a') a' anns

-- |@'add' a@ - Adds an annotation to data base
add :: Ann -> IO ()
add a = do
  let date = Ann.date a
  let year = Data.List.take 4 date
  (id, anns) <- Data.Diary.read year
  let anns' = add1 id a date anns
  write year (id + 1) anns'

remove' _ [] = []
remove' id (a:rest) = if id == (Ann.id a) then rest else a:(remove' id rest)

-- |@'remove' id date@ - Removes an annotation from data base
remove :: Int -> String -> IO ()
remove id date = do
  let year = Data.List.take 4 date
  (idNew, anns) <- Data.Diary.read year
  let anns' = remove' id anns
  write year idNew anns'

-- |@'years'@ - Returns a sorted (before-after) list of diary years
years :: IO [String]
years = do
  r <- File.dir $ path ""
  return $ sort r

take' r n ys = tk r ys
  where
    tk r [] = return r
    tk r (y:ys) = do
      let len = length r
      if len < n
      then do
        (_, anns) <- Data.Diary.read y
        tk (r ++ Prelude.take (n - len) anns) ys
      else
        return r

-- |@'take' n@ - Returns the last /n/ annotations of diary
take :: Int -> IO [Ann]
take n = do
  ys <- years
  take' [] n $ reverse ys

books' (pf, ld) y = do
  (_, anns) <- Data.Diary.read y
  return $ foldl' (\(p, l) a -> Ledger.add a p l) (pf, ld) $ reverse anns

-- |@'books'@ - Returns current books: (portfolio, ledger)
books :: IO (Pf, Ledger)
books = do
  ys <- years
  foldM books' (Pf.new, Ledger.new) ys

-- |@'yearBooks' y@ - Returns books of year /y/: (portfolio, ledger)
yearBooks :: String -> IO (Pf, Ledger)
yearBooks y = do
  ys <- years
  foldM books' (Pf.new, Ledger.new) $ takeWhile (<= y) ys

round2 :: Double -> Double
round2 n = fromIntegral (round (n * 100)) / 100

cash' amm y = do
  (_, anns) <- Data.Diary.read y
  return $ foldl' (\a ann -> a + c ann) amm anns
  where
    c (Sell _ _ _ stocks price) =
      let m = round2 $ fromIntegral stocks * price
          fees = round2 $ Fees.app m
      in  m - fees
    c (Buy _ _ _ stocks price) =
      let m = round2 $ fromIntegral stocks * price
          fees = round2 $ Fees.app m
      in  - m - fees
    c (Withdrawal _ _ m) = - m
    c (Income _ _ m) = m
    c (Profits _ _ m _) = m
    c (Fees _ _ m _) = - m
    c (Pdif _ _ m _) = m
    c (Ndif _ _ m _) = - m
    c (Close _ _) = 0

cash :: IO Double
cash = do
  ys <- years
  foldM cash' 0 ys
