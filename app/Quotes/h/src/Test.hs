-- Copyright 29-Jan-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Test page

module Test (process) where

import Data.List
import Text.Printf
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Dm.Date as Date
import qualified Dm.Cgi as Cgi
import Dm.Cgi (Cgi)
import qualified Data.NicksDb as NicksDb
import Data.NicksDb (NicksDb(..))
import qualified Data.ServersDb as ServersDb
import qualified Data.Server as Server
import qualified Data.Nick as Nick
import Data.Nick (Nick(..))
import qualified Data.Quote as Quote
import Data.Quote (Quote(..))
import qualified Conf as Conf

issue1 = "_1_" -- Ok.
issue2 = "_2_" -- Error reading local quotes.
issue3 = "_3_" -- Bad nick Id in Test/readQuotesSvs
issue4 = "_4_" -- Extra quote
issue5 = "_5_" -- Open
issue6 = "_6_" -- Close
issue7 = "_7_" -- Maximum
issue8 = "_8_" -- Minimum
issue9 = "_9_" -- Volume

lastQs :: [Quote] -> IO [Quote]
lastQs qs = do
  now <- Date.now
  let d = Date.toStr $ Date.add (-30) now
  return $ filter (\(Quote dt _ _ _ _ _ _) -> dt >= d) qs

data Server = Server {
  id :: String,
  quote :: Quote
}

nextNick :: String -> String -> IO String
nextNick tp last = do
  let fm = if tp == "nicks" then fnicks else fextra
  (NicksDb _ _ nicks) <- NicksDb.read
  let nicks' = fm nicks
  case nicks' of
    [] -> return ""
    (nk:nks) -> if last == "" then return nk
                              else findNext nk nks
  where
    fnicks ls = sort $ map
                        (\(Nick _ name _ _ _) -> name)
                        (filter
                          (\(Nick _ _ _ isSel _) -> isSel)
                          ls)
    fextra ls = sort $ map
                        (\(Nick _ name _ _ _) -> name)
                        (filter
                          (\(Nick _ _ _ _ isExtra) -> isExtra)
                          ls)
    findNext _ [] = return ""
    findNext nk (nk':nks) = if nk == last then return nk' else findNext nk' nks

readQuotesMe :: String -> IO (Either String [Quote])
readQuotesMe nkId = do
  qs' <- NicksDb.quotes nkId
  case qs' of
    Right qs -> do
      lqs <- lastQs qs
      return $ Right lqs
    _ -> return qs'

readQuotesSvs :: String -> IO (Either String [[Quote]])
readQuotesSvs nkId = let names = ServersDb.listNames
                     in  reduce (return (Right [])) $ reverse names
  where
    reduce :: IO (Either String [[Quote]]) -> [String] ->
              IO (Either String [[Quote]])
    reduce r [] = r
    reduce r (nm:nms) = do
      nicks <- ServersDb.nicks nm
      case lookup nkId nicks of
        Nothing -> return $ Left issue3
        Just nk -> do
          (Right qqs) <- r
          qs' <- case ServersDb.get nm of
                    ServersDb.S1 sv -> Server.read sv nk
                    ServersDb.S2 sv -> Server.read sv nk
                    ServersDb.S3 sv -> Server.read sv nk
          case qs' of
            Right qs -> do
              lqs <- lastQs qs
              reduce (return (Right (lqs:qqs))) nms
            Left e -> return $ Left e

qextract :: Quote -> [Quote] -> (Maybe Quote, [Quote])
qextract q qs = qext [] q qs
  where
    qext rest _ [] = (Nothing, reverse rest)
    qext rest q1@(Quote d _ _ _ _ _ _) (q2@(Quote d' _ _ _ _ _ _):qs) =
      if (d == d') then (Just q2, (reverse rest) ++ qs)
                   else qext (q2:rest) q1 qs

eqq :: Double -> Double -> Bool
eqq n1 n2 = let df = n1 - n2 in df > -0.0001 && df < 0.0001

eqv :: Int -> Int -> Bool
eqv n1 n2 = let df = n1 - n2; l = (n1 + n2) `div` 10 in df > -l && df < l

issuesf5 :: Quote -> Quote -> Maybe String
issuesf5 (Quote _ o c mx mn v _) (Quote _ o1 c1 mx1 mn1 v1 _) =
  if eqq o o1
  then if eqq c c1
       then if eqq mx mx1
            then if eqq mn mn1
                 then if eqv v v1
                      then Nothing
                      else Just issue9
                 else Just issue8
            else Just issue7
       else Just issue6
  else Just issue5

issuesf4 :: Quote -> Quote -> Quote -> Maybe String
issuesf4 q q1 q2 =
  case issuesf5 q q1 of
    Nothing -> Nothing
    e -> case issuesf5 q q2 of
                Nothing -> Nothing
                _ -> e

issuesf3 :: Quote -> Quote -> Quote -> Quote -> Maybe String
issuesf3 q q1 q2 q3 =
  case issuesf5 q1 q2 of
    Nothing -> issuesf5 q q1
    _ -> case issuesf5 q1 q3 of
      Nothing -> issuesf5 q q1
      _ -> case issuesf5 q2 q3 of
        Nothing -> issuesf5 q q2
        _ -> case issuesf4 q q1 q2 of
          Nothing -> Nothing
          e -> case issuesf5 q q3 of
            Nothing -> Nothing
            _ -> e

issuesf21 :: Quote -> Maybe Quote -> Maybe Quote -> Maybe String
issuesf21 q q1' q2' =
  case q1' of
    Nothing ->
      case q2' of
        Nothing -> Just issue4
        Just q2 -> issuesf5 q q2
    Just q1 ->
      case q2' of
        Nothing -> issuesf5 q q1
        Just q2 -> issuesf4 q q1 q2

issuesf2 :: Quote -> Maybe Quote -> Maybe Quote -> Maybe Quote -> Maybe String
issuesf2 (Quote _ _ _ _ _ _ True) _ _ _ = Nothing
issuesf2 q q1' q2' q3' =
  case q1' of
    Nothing -> issuesf21 q q2' q3'
    Just q1 ->
      case q2' of
        Nothing -> issuesf21 q q1' q3'
        Just q2 ->
          case q3' of
            Nothing -> issuesf21 q q1' q2'
            Just q3 -> issuesf3 q q1 q2 q3

issuesf1 :: [Quote] -> [Quote] -> [Quote] -> [Quote] -> Maybe String
issuesf1 qs qs1 qs2 qs3 = iss Nothing qs qs1 qs2 qs3
  where
    iss r [] qs1 qs2 qs3 = r
    iss r (q:qs) qs1 qs2 qs3 =
      let (q1, qs1') = qextract q qs1
          (q2, qs2') = qextract q qs2
          (q3, qs3') = qextract q qs3
      in  case issuesf2 q q1 q2 q3 of
            Nothing -> iss r qs qs1' qs2' qs3'
            Just e ->
              let svnames = ServersDb.listNames
                  msg = Quote.toStr q ++ " [" ++ e ++ "]\n" ++
                          toStr q1 ++ " - " ++ svnames !! 0 ++ "\n" ++
                          toStr q2 ++ " - " ++ svnames !! 1 ++ "\n" ++
                          toStr q3 ++ " - " ++ svnames !! 2 ++ "\n"
              in  case r of
                    Nothing -> iss (Just msg) qs qs1' qs2' qs3'
                    Just e' -> iss (Just $ e' ++ "\n" ++ msg) qs qs1' qs2' qs3'
    toStr q' = case q' of
                Nothing -> "--------:-1:-1:-1:-1:-1:missing"
                Just q'' -> Quote.toStr q''

issuesf :: String -> IO (Maybe String)
issuesf nk = do
  qsMe' <- readQuotesMe nk
  case qsMe' of
    Left e -> return $ Just e
    Right qsMe -> do
      eqsSvs' <- readQuotesSvs nk
      case eqsSvs' of
        Left e -> return $ Just e
        Right [qs1, qs2, qs3] -> return $ issuesf1 qsMe qs1 qs2 qs3

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "nicks" -> do  ------------------------------------------------------ nicks
      NicksDb _ _ ns <- NicksDb.read
      let idNicks = map
                    (\(Nick id name _ _ _) ->
                      Js.wList [Js.wString id, Js.wString name])
                    (filter
                      (\(Nick _ _ _ _ isExtra) -> not isExtra)
                      ns)
      Cgi.ok cgi [("idNicks", Js.wList idNicks)]
    "extra" -> do  ------------------------------------------------------ extra
      NicksDb _ _ ns <- NicksDb.read
      let idNicks = map
                    (\(Nick id name _ _ _) ->
                      Js.wList [Js.wString id, Js.wString name])
                    (filter
                      (\(Nick _ _ _ _ isExtra) -> isExtra)
                      ns)
      Cgi.ok cgi [("idNicks", Js.wList idNicks)]
    "issues" -> do  ---------------------------------------------------- issues
      let nickId = Cgi.get rq Js.rString "nickId"
      Right nick <- NicksDb.nickName nickId
      iss' <- issuesf nickId
      let issues = case iss' of
                      Nothing -> nick ++ ": " ++ issue1 ++ "\n"
                      Just iss -> nick ++ ":\n" ++ iss ++ "\n"
      Cgi.ok cgi [("issues", Js.wString issues)]

    s -> Prelude.error $ printf "Unknown rq '%s'" s --------------------- Error
