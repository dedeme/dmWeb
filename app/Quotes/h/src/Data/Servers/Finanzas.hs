-- Copyright 30-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Server Finanzas

module Data.Servers.Finanzas (Finanzas (..)) where

import Control.Exception
import Data.List
import Text.Printf
import qualified Data.Servers.Reader as Reader
import qualified Dm.Date as Date
import Data.Server (Server)
import qualified Data.Quote as Q
import Data.Quote (Quote (..))
import qualified Data.Server as Sv

data Finanzas = Finanzas

trim = takeWhile (> ' ') . dropWhile (<= ' ')

fdrop :: String -> String -> Maybe String
fdrop "" sub = Nothing
fdrop s@(_:rest) sub = if isPrefixOf sub s then Just s else fdrop rest sub

ftake :: String -> String -> Maybe String
ftake s sub = ftake' "" s
  where
  ftake' _ "" = Nothing
  ftake' r s@(c:cs) = if isPrefixOf sub s
                      then Just $ reverse r
                      else ftake' (c:r) cs

regularizeN = (remComma . remPoint)
  where
  remComma = map (\c -> if c == ',' then '.' else c)
  remPoint = filter (/= '.')

readInt :: String -> IO (Maybe Int)
readInt s = do
  r <- try $ evaluate $ read $ regularizeN s :: IO (Either SomeException Int)
  case r of
    Left _ -> return Nothing
    Right v -> return $ Just v

readDouble :: String -> IO (Maybe Double)
readDouble s = do
  r <- try $ evaluate $ read $ regularizeN s :: IO (Either SomeException Double)
  case r of
    Left _ -> return Nothing
    Right v -> return $ Just v

readDate :: String -> IO (Maybe String)
readDate tx = do
  let [ds, ms, ys] = words $ map (\ch -> if ch == '/' then ' ' else ch) tx
  d' <- readInt ds
  case d' of
    Nothing -> return Nothing
    Just d -> do
      m' <- readInt ms
      case m' of
        Nothing -> return Nothing
        Just m -> do
          y' <- readInt ys
          case y' of
            Nothing -> return Nothing
            Just y -> return $ Just $ Date.toStr $
                               Date.new (toInteger y) m d

readCol r h =
  case fdrop h "<td" of
    Nothing -> Nothing
    Just h1 -> case fdrop h1 ">" of
                  Nothing -> Nothing
                  Just h2 -> case ftake (tail h2) "</td>" of
                                Nothing -> Nothing
                                Just tx -> Just (trim tx, h2)

colDate h = case readCol [] h of
  Nothing -> return Nothing
  Just (tx, rest) -> do
    date <- readDate tx
    case date of
      Nothing -> return Nothing
      Just d -> return $ Just (d, rest)

colDouble h = case readCol [] h of
  Nothing -> return Nothing
  Just (tx, rest) -> do
    n <- readDouble tx
    case n of
      Nothing -> return Nothing
      Just n' -> return $ Just (n', rest)

colInt h = case readCol [] h of
  Nothing -> return Nothing
  Just (tx, rest) -> do
    n <- readInt tx
    case n of
      Nothing -> return Nothing
      Just n' -> return $ Just (n', rest)

rowVol d o c mx mn v = return $ Just $ Quote {
    Q.date = d,
    Q.open = o,
    Q.close = c,
    Q.max = mx,
    Q.min = mn,
    Q.vol = v,
    Q.error = False
  }

rowMin d o c mx mn h = do
  vol <- colInt h
  case vol of
    Nothing -> return Nothing
    Just (v, _) -> rowVol d o c mx mn v

rowMax d o c mx h = do
  mini <- colDouble h
  case mini of
    Nothing -> return Nothing
    Just (mn, rest) -> rowMin d o c mx mn rest

rowClose d o c h = do
  tmp <- colDouble h
  case tmp of
    Nothing -> return Nothing
    Just (_, rest) -> do
      maxi <- colDouble rest
      case maxi of
        Nothing -> return Nothing
        Just (mx, rest') -> rowMax d o c mx rest'

rowOpen d o h = do
  close <- colDouble h
  case close of
    Nothing -> return Nothing
    Just (c, rest) -> rowClose d o c rest

rowDate d h = do
  open <- colDouble h
  case open of
    Nothing -> return Nothing
    Just (o, rest) -> rowOpen d o rest

row h = do
  date <- colDate h
  case date of
    Nothing -> return Nothing
    Just (d, rest) -> rowDate d rest

table :: [Quote] -> String -> IO (Either String [Quote])
table r h = do
  case fdrop h "<tr " of
    Nothing -> return $ Right $ reverse r
    Just h' -> case ftake h' "</tr>" of
                  Nothing -> return $ Left "<tr> not closed"
                  Just h'' -> do
                    row' <- row h''
                    case row' of
                      Nothing -> table r $ tail h'
                      Just q -> table (q:r) $ tail h'

instance Server Finanzas where
  name a = "Finanzas"

  uri a code =
    printf "http://www.finanzas.com/cotizaciones/%s/datos-historicos.html"
           code

  read a code = do
    html <- Reader.read $ Sv.uri a code
    case fdrop html "<h2>Hist" of
      Nothing -> return $ Left "'<h2>Hist' not found"
      Just h'' -> case fdrop h'' "<tbody>" of
        Nothing -> return $ Left "'<tbody>' not found"
        Just h -> case ftake h "</tbody>" of
                    Nothing -> return $ Left "'</tbody>' not found"
                    Just h' -> table [] h'
