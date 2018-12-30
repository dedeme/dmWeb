-- Copyright 18-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Historic data

module Data.Historic (
  Data.Historic.init,
  write,
  Data.Historic.take,
  removeDuplicate
  ) where

import Data.List
import Control.Monad (foldM)
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Global as G

path :: String -> String
path "" = G.path ["data", "Historic"]
path year = G.path ["data", "Historic", year]

-- |@'init'@ - Initializes data base
init :: IO ()
init = File.mkDir $ path ""

write' :: String-> [(String, JSValue)] -> IO ()
write' year dateProfits =
  File.write (path year) $ Js.toStr $ Js.wList $ map toJs dateProfits
  where
    toJs (d, p) = Js.wList [Js.wString d, p]

-- |@'write' new@(date, profits)@ - Write a new entry in the Historic.
write :: (String, JSValue) -> IO ()
write new@(date, profits) = do
  let year = Prelude.take 4 date
  ls <- Data.Historic.read year
  case ls of
    [] -> write' year [new]
    (d, _):rest ->
      if d == date then write' year (new:rest) else write' year (new:ls)

readJs :: String -> IO JSValue
readJs year = do
  let p = path year
  ex <- File.exists $ p
  if ex
  then File.read (path year) >>= return . Js.fromStr
  else return $ Js.fromStr "[]"

readJs2 :: String -> IO [JSValue]
readJs2 year = do
  r <- readJs year
  return $ Js.rList r

read :: String -> IO [(String, JSValue)]
read year = readJs year >>= return . map fromJs . Js.rList
  where
    fromJs js = let [d, p] = Js.rList js in (Js.rString d, p)

years :: IO [String]
years = do
  r <- File.dir $ path ""
  return $ reverse $ sort r

take' ys = do
  let rys = map readJs2 ys
  lss <- sequence rys
  return $ concat lss

-- |@'take' n@ - Returns the last /n/ years of diary. If n < 1 or n > 2, it
--               returns all of them.
take :: Int -> IO [JSValue]
take 1 = do
  ys <- years
  case ys of
    [] -> return []
    (y:_) -> readJs2 y
take 2 = do
  ys <- years
  case ys of
    (y1:y2:_) -> take' [y1, y2]
    _ -> take' ys
take _ = do
  ys <- years
  take' ys

removeDuplicate :: IO ()
removeDuplicate = do
  (y:_) <- years
  d <- Data.Historic.read y
  case d of
    [] -> File.del $ path y
    (_:rest) -> write' y rest
