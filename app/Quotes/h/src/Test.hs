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

selOkDb :: [Double] -> (Double, Double)
selOkDb [q, q1, q2, q3] =
  if eqq q1 q2 || eqq q1 q3 then (q, q1)
  else if eqq q2 q3 then (q, q2)
  else if eqq q q1 || eqq q q2 || eqq q q3 then (q, q)
  else (q, q1)
selOkDb [q, q1, q2] =
  if eqq q1 q2 then (q, q1)
  else if eqq q q1 || eqq q q2 then (q, q)
  else (q, q1)
selOkDb [q, q1] = (q, q1)

selOkInt :: [Int] -> (Int, Int)
selOkInt [q, q1, q2, q3] =
  if eqv q1 q2 || eqv q1 q3 then (q, q1)
  else if eqv q2 q3 then (q, q2)
  else if eqv q q1 || eqv q q2 || eqv q q3 then (q, q)
  else (q, q1)
selOkInt [q, q1, q2] =
  if eqv q1 q2 then (q, q1)
  else if eqv q q1 || eqv q q2 then (q, q)
  else (q, q1)
selOkInt [q, q1] = (q, q1)

update :: String -> Quote -> IO ()
update nkId q@(Quote d _ _ _ _ _ _) = do
  Right nk <- NicksDb.nickName nkId
  Right qs <- NicksDb.quotes nkId
  NicksDb.writeQuotes nk $ fix [] qs
  where
    fix r [] = reverse r
    fix r (q'@(Quote d' _ _ _ _ _ _):qs) = if d' == d then fix (q:r) qs
                                                      else fix (q':r) qs

issuesf2 :: String -> Quote -> Maybe Quote -> Maybe Quote -> Maybe Quote ->
            IO (Maybe String)
issuesf2 _ (Quote _ _ _ _ _ _ True) _ _ _ = return Nothing
issuesf2 nkId q q1 q2 q3 = do
  let auto = nkId /= ""
  let qs = q:(ff [] [q1, q2, q3])
  let (o, o') = selOkDb $ map (getDb 'o') qs
  if eqq o o'
  then do
    let (o, o') = selOkDb $ map (getDb 'c') qs
    if eqq o o'
    then do
      let (o, o') = selOkDb $ map (getDb 'x') qs
      if eqq o o'
      then do
        let (o, o') = selOkDb $ map (getDb 'n') qs
        if eqq o o'
        then do
          let (o, o') = selOkInt $ map getVol qs
          if eqv o o'
          then return Nothing
          else  if auto
                then do
                  let q' = q {vol = o'}
                  update nkId q'
                  issuesf2 nkId q' q1 q2 q3
                else return $ Just issue9
        else  if auto
              then do
                let q' = q {Data.Quote.min = o'}
                update nkId q'
                issuesf2 nkId q' q1 q2 q3
              else return $ Just issue8
      else  if auto
            then do
              let q' = q {Data.Quote.max = o'}
              update nkId q'
              issuesf2 nkId q' q1 q2 q3
            else return $ Just issue7
    else  if auto
          then do
            let q' = q {close = o'}
            update nkId q'
            issuesf2 nkId q' q1 q2 q3
          else return $ Just issue6
  else  if auto
        then do
          let q' = q {open = o'}
          update nkId q'
          issuesf2 nkId q' q1 q2 q3
        else return $ Just issue5
  where
    ff r [] = reverse r
    ff r (q:qs) = case q of
                    Nothing -> ff r qs
                    Just q' -> ff (q':r) qs
    getDb tp (Quote _ o c mx mn _ _) = case tp of
                                        'o' -> o
                                        'c' -> c
                                        'x' -> mx
                                        _ -> mn
    getVol (Quote _ _ _ _ _ v _) = v

issuesf1 :: String -> [Quote] -> [Quote] -> [Quote] -> [Quote] ->
            IO (Maybe String)
issuesf1 nkId qs qs1 qs2 qs3 = iss Nothing qs qs1 qs2 qs3
  where
    iss r [] qs1 qs2 qs3 = return r
    iss r (q:qs) qs1 qs2 qs3 = do
      let (q1, qs1') = qextract q qs1
      let (q2, qs2') = qextract q qs2
      let (q3, qs3') = qextract q qs3
      issues <- case (q1, q2, q3) of
                  (Nothing, Nothing, Nothing) ->
                    if Quote.error q then return Nothing
                                     else return $ Just issue4
                  _ -> issuesf2 nkId q q1 q2 q3
      case issues of
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

issuesf :: Bool -> String -> IO (Maybe String)
issuesf auto nk = do
  qsMe' <- readQuotesMe nk
  case qsMe' of
    Left e -> return $ Just e
    Right qsMe -> do
      eqsSvs' <- readQuotesSvs nk
      case eqsSvs' of
        Left e -> return $ Just e
        Right [qs1, qs2, qs3] ->
          issuesf1 (if auto then nk else "") qsMe qs1 qs2 qs3

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
      let autocorrection = Cgi.get rq Js.rBool "autocorrection"
      Right nick <- NicksDb.nickName nickId
      iss' <- issuesf autocorrection nickId
      let issues = case iss' of
                      Nothing -> nick ++ ": " ++ issue1 ++ "\n"
                      Just iss -> nick ++ ":\n" ++ iss ++ "\n"
      Cgi.ok cgi [("issues", Js.wString issues)]

    s -> Prelude.error $ printf "Unknown rq '%s'" s --------------------- Error
