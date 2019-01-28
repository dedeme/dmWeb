-- Copyright 28-Jan-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Nick - codes reader

module Data.Servers.Codes (codes) where

import Data.List
import qualified Dm.File as File
import qualified Dm.Js as Js
import qualified Global as G

readNicks :: IO [(String, String)]
readNicks = do
  nicksDb <- File.read (G.quotesBase ++ "/nicks.db")
  let [_, _, nicks] = Js.rList $ Js.fromStr nicksDb
  return $ map fm $ filter ff $ map eFromJs $ Js.rList $ nicks
  where
    eFromJs js = let [id, nick, _, sel, _] = Js.rList js
                 in  (id, nick, sel)
    ff (_, _, sel) = Js.rBool sel
    fm (id, nick, _) = (Js.rString id, Js.rString nick)

-- | @'codes' dir@ - Returns the map (nick-code) of server in
--                  "quotes//dir//codes.db"
codes :: String -> IO [(String, String)]
codes dir = do
  cs <- File.read (G.quotesBase ++ "/" ++ dir ++ "/codes.db")
  let [idsJs, codesJs] = Js.rList $ Js.fromStr cs
  let ids = map (\js -> Js.rString js) $ Js.rList idsJs
  let codes = map (\js -> Js.rString js) $ Js.rList codesJs
  nks <- readNicks
  return $ map (fNkCd ids codes) nks
  where
    fNkCd ids codes (id, nick) =
      case elemIndex id ids of
        Nothing -> ("", "")
        Just ix -> (nick, codes !! ix)
