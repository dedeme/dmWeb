-- Copyright 20-Jun-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Server Yahoo-Volume

module Data.VServers.Yahoo (readVol) where

import Debug.Trace
import Data.List
import Control.Exception (catch, try, evaluate, SomeException)
import Network.HTTP.Conduit (HttpException)
import qualified Data.Server as Server
import qualified Data.ServersDb as ServersDb
import qualified Data.Servers.Reader as Reader
import Data.Servers.Yahoo (Yahoo)
import qualified Data.Nick as Nick
import Data.Nick (Nick)

failed :: Double
failed = (-2)

extra :: [(String, String)]
extra = [("CMC", "CMC.MC")]

sv :: Yahoo
sv = let (sv, _, _) = ServersDb.list in sv

getCode :: Nick -> IO (Maybe String)
getCode nick = do
  nkCds <- ServersDb.nicks $ Server.name sv
  case lookup (Nick.id nick) nkCds of
    Nothing -> return $ lookup (Nick.name nick) extra
    Just c -> return $ Just c

readPage :: Nick -> IO (Maybe String)
readPage nick = do
  code <- getCode nick
  case code of
    Nothing -> return Nothing
    Just c -> do
      p <- catch (Reader.read $ Server.uri sv c) blank
      case p of
        "" -> return Nothing
        pg -> return $ Just pg
  where
    blank :: HttpException -> IO String
    blank e = return ""

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

readDouble :: String -> IO Double
readDouble s = do
  r <- try $ evaluate $ read $ regularizeN s :: IO (Either SomeException Double)
  case r of
    Left _ -> return (-1)
    Right v -> return v

readCol :: String -> IO Double
readCol tx =
  case fdrop tx ">" of
    Nothing -> return (-1)
    Just nTx ->
      case ftake (tail nTx) "<" of
        Nothing -> return (-1)
        Just n -> readDouble $ trim n

volField :: Double -> String -> IO Double
volField avg tx =
  case fdrop tx "<span " of
    Nothing -> return (-1)
    Just close1Tx ->
      case fdrop (tail close1Tx) "<span " of
        Nothing -> return (-1)
        Just close2Tx ->
          case fdrop (tail close2Tx) "<span " of
            Nothing -> return (1)
            Just volTx -> do
              v <- readCol volTx
              if v <= 0 then return (1)
                        else return $ v * avg

minField :: Double -> String -> IO Double
minField mx tx =
  case fdrop tx "<span " of
    Nothing -> return (-1)
    Just minTx -> do
      mn <- readCol minTx
      let avg = (mx - mn) / 2
      if mn <= 0 || avg <= 0
        then return (-1)
        else volField avg $ tail minTx

maxField :: String -> IO Double
maxField tx =
  case fdrop tx "<span " of
    Nothing -> return (-1)
    Just maxTx -> do
      mx <- readCol maxTx
      if mx <= 0 then return (-1)
                  else minField mx $ tail maxTx

row :: String -> IO Double
row tx =
  case fdrop tx "<span " of
    Nothing -> return (-1)
    Just dateTx ->
      case fdrop (tail dateTx) "<span " of
        Nothing -> return (-1)
        Just openTx -> maxField $ tail openTx

table :: Int -> Double -> String -> IO Double
table c vol tx = do
  if c >= 100
    then return $ vol / (fromIntegral c)
    else
      case fdrop tx "<tr " of
        Nothing -> return $ if c > 0 then vol / (fromIntegral c) else 0
        Just tx' -> case ftake tx' "</tr>" of
                      Nothing -> return failed
                      Just rowTx -> do
                        n <- row rowTx
                        if n <= 0 then table c vol $ tail tx'
                                  else table (c + 1) (vol + n) $ tail tx'

readVolPage :: String -> IO Double
readVolPage page =
  case fdrop page "</thead>" of
    Nothing -> return failed
    Just tx -> case ftake tx "</tbody>" of
      Nothing -> return failed
      Just tableTx -> table 0 0 tableTx

readVol :: Nick -> IO Double
readVol nick = do
  page <- readPage nick
  case page of
    Nothing -> return failed
    Just p -> readVolPage p
