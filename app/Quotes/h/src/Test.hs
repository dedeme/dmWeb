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
import qualified Data.Server as WServer
import qualified Data.Nick as Nick
import Data.Nick (Nick(..))
import qualified Data.Quote as Quote
import Data.Quote (Quote(..))
import qualified Conf as Conf

lastQs :: [Quotes] -> IO [Quotes]
lastQs qs = do
  now <- Date.now
  let d = Date.toStr $ Date.add (-30) now
  return filter (\(Quote dt _ _ _ _ _ _) -> dt >= d) qs

data Quotes = Quotes {
  open :: Double,
  close :: Double,
  max :: Double,
  min :: Double
}

quotesToJs (Quotes o c mx mn) =
  Js.wList [Js.wDouble o, Js.wDouble c, Js.wDouble mx, Js.wDouble mn]

data Server = Server {
  id :: String,
  quotes :: Quotes
}

serverToJs (Server id qs) = Js.wList [Js.wString id, quotesToJs qs]

data Issue = Issue {
  date :: String,
  qsMe :: Quotes,
  sv1 :: Server,
  sv2 :: Server,
  sv3 :: Server
}

issueToJs (Issue d me s1 s2 s3) =
  Js.wList [Js.wString d, quotesToJs me,
            serverToJs s1, serverToJs s2, serverToJs s3]

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

readQuotesMe :: String -> (String, [Quotes])
readQuotesMe nk = do
  qs' <- NicksDb.quotes nk
  case qs' of
    Left e -> return (e, [])
    Right qs -> return $ lastQs qs

readQuotesSvs :: String -> IO (String, [Quotes])
readQuotesSvs nk = let names = ServersDb.listNames
                   in  reduce (return ("", [])) names
  where
    reduce :: IO (String, [Quotes]) -> [String] -> IO (String, [Quotes])
    reduce r [] = r >>= reverse
    reduce r (nm:nms) = do
      (_, qqs) <- r
      qs' <- let sv = ServersDb.get nm in WServer.read sv nk
      case qs' of
        Left e -> return (e, [])
        Right qs -> reduce (return ("", (lastQs qs):qqs)) nms

issuesf :: String -> IO (String, [Issue])
issuesf nk = do
  (error, qsMe) <- readQuotesMe nk
  if error /= ""
  then return (error, [])
  else do
    (error, qsSvs) <- readQuotesSvs nk
    if error /= ""
    then return (error, [])
    else return ("", findIssues qsMe qsSvs)

process :: Cgi -> [(String, JSValue)] -> IO ()
process cgi rq =
  case Cgi.get rq Js.rString "rq" of
    "issues" -> do  ---------------------------------------------------- issues
      let tp = Cgi.get rq Js.rString "type"
      let lastNick = Cgi.get rq Js.rString "lastNick"
      nick <- nextNick tp lastNick
      (error, issues) <- issuesf nick
      Cgi.ok cgi [("nick", Js.wString ""),
                  ("error", Js.wString error),
                  ("issues", Js.wList $ map issueToJs issues)]

    s -> Prelude.error $ printf "Unknown rq '%s'" s --------------------- Error
