-- Copyright 03-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Nicks data base

module Data.NicksDb (
  NicksDb (..),
  Data.NicksDb.init,
  readJs,
  Data.NicksDb.read,
  exists,
  add,
  setModel,
  remove,
  setIbex,
  setSel,
  setExtra,
  setName,
  nickName,
  quotes,
  quotesStr,
  writeQuotes,
  updateQuotes
  ) where

import Data.Maybe
import Data.List
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import Data.Nick (Nick(..))
import qualified Data.Nick as Nick
import qualified Data.Quote as Quote
import Data.Quote (Quote(..))
import qualified Global as G

-- | Nicks data base
data NicksDb = NicksDb {
  nextId :: Int,
  model :: String,
  nicks :: [Nick]
}

-- | @'init'@ - Initializes NicksDb
init :: IO ()
init = write $ NicksDb 0 "0" []

-- | @'readJs'@ - Returns NicksDb JSONized
readJs :: IO [JSValue]
readJs = do
  d <- File.read (G.path ["data", "nicks.db"])
  return $ (Js.rList . Js.fromStr) d

-- | @'read'@ - Returns NicksDb
read :: IO NicksDb
read = do
  d <- File.read $ G.path ["data", "nicks.db"]
  let [nextId, model, nicks] = Js.rList $ Js.fromStr d
  return $ NicksDb  (Js.rInt nextId)
                    (Js.rString model)
                    (map Nick.fromJs $ Js.rList nicks)

write :: NicksDb -> IO ()
write (NicksDb nextId model nicks) =
  File.write  (G.path ["data", "nicks.db"])
              $ Js.toStr $ Js.wList [
                  Js.wInt nextId,
                  Js.wString model,
                  Js.wList $ map Nick.toJs nicks
                ]

-- | @'exists' nick@ - Returns true if 'nick' is in data base
exists :: String -> IO Bool
exists nick = do
  NicksDb _ _ nicks <- Data.NicksDb.read
  return $ isJust $ find (\nk -> Nick.name nk == nick) nicks

-- | @'add' nickName isExtra@ - Adds a new nick to NicksDb
add :: String -> Bool -> IO Bool
add nick isExtra = do
  db@(NicksDb id model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.name nk == nick) nicks of
    Nothing -> do
      let idStr = show id
      let model' = if length nicks == 0 then idStr else model
      let new = if isExtra then Nick.newExtra else Nick.new
      write $ NicksDb (id + 1) model' ((new idStr nick):nicks)
      File.write (G.path ["data", "quotes", nick ++ ".db"]) ""
      return True
    Just _ -> return False

-- | @'setModel' nickId@ - Set /nickId/ as model
setModel :: String -> IO ()
setModel id = do
  db@(NicksDb nId model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.id nk == id) nicks of
    Nothing -> return ()
    Just _ -> do
      write $ NicksDb nId id nicks

-- | @'remove' nickId@ - Remove /nickId/ from NicksDb
remove :: String -> IO ()
remove id = do
  db@(NicksDb nId model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.id nk == id) nicks of
    Nothing -> return ()
    Just (Nick _ name _ _ _) -> do
      let model' = if id == model then "0" else model
      write $ NicksDb nId model' (filter (\nk -> Nick.id nk /= id) nicks)
      File.del (G.path ["data", "quotes", name ++ ".db"])

-- | @'setIbex' nickId value@ - Sets field /isIbex/ of /nickId/ as /value/
setIbex :: String -> Bool -> IO ()
setIbex id value = do
  db@(NicksDb nId model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.id nk == id) nicks of
    Nothing -> return ()
    Just (Nick id name _ isSel isExtra) -> do
      let nicks' = filter (\nk -> Nick.id nk /= id) nicks
      write $ NicksDb nId model ((Nick id name value isSel isExtra):nicks')

-- | @'setSel' nickId value@ - Sets field /isSel/ of /nickId/ as /value/
setSel :: String -> Bool -> IO ()
setSel id value = do
  db@(NicksDb nId model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.id nk == id) nicks of
    Nothing -> return ()
    Just (Nick id name isIbex _ isExtra) -> do
      let nicks' = filter (\nk -> Nick.id nk /= id) nicks
      write $ NicksDb nId model ((Nick id name isIbex value isExtra):nicks')

-- | @'setExtra' nickId value@ - Sets field /isExtra/ of /nickId/ as /value/
setExtra :: String -> Bool -> IO ()
setExtra id value = do
  db@(NicksDb nId model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.id nk == id) nicks of
    Nothing -> return ()
    Just (Nick id name isIbex isSel _) -> do
      let nicks' = filter (\nk -> Nick.id nk /= id) nicks
      write $ NicksDb nId model ((Nick id name isIbex isSel value):nicks')

-- | @'setName' nickId newName@ - Set field /name/ of /nickId/ as /newName/ and
--                                rename /name/.db
setName :: String -> String -> IO Bool
setName id newName = do
  db@(NicksDb nId model nicks) <- Data.NicksDb.read
  case find (\nk -> Nick.id nk == id) nicks of
    Nothing -> return False
    Just (Nick id oldName isIbex isSel isExtra) -> do
      let nicks' = filter (\nk -> Nick.id nk /= id) nicks
      case find (\nk -> Nick.name nk == newName) nicks of
        Nothing -> do
          write $ NicksDb nId model
                          ((Nick id newName isIbex isSel isExtra):nicks')
          File.rename (G.path ["data", "quotes", oldName ++ ".db"])
                      (G.path ["data", "quotes", newName ++ ".db"])
          return True
        _ -> return False


-- | @'nickName' nickId@ - Returns the name of /nickId/
nickName :: String -> IO (Either String String)
nickName id = do
  db@(NicksDb _ _ nicks) <- Data.NicksDb.read
  case find (\(Nick id' _ _ _ _) -> id' == id) nicks of
    Nothing -> return $ Left $
                          "Company code '" ++ id ++ "' not found in quotes db"
    Just (Nick _ nick _ _ _) -> return $ Right nick

-- quotes management -------------------------------------------------

quotes' :: String -> IO (Either String [Quote])
quotes' nick = do
  let path = G.path ["data", "quotes", nick ++ ".db"]
  ex <- File.exists path
  if ex
  then do
    qs <- File.read path
    return $ Right $ map Quote.fromStr $ filter (/= "") $ lines qs
  else return $ Left $ "File '" ++ nick ++ ".db' not found"

-- | @'quotes' nickId@ - Returns quotes of /nickId/
quotes :: String -> IO (Either String [Quote])
quotes id = do
  nick' <- nickName id
  case nick' of
    Left e -> return $ Left e
    Right nick -> quotes' nick

-- | @'quotesStr' nickId@ - Returns quotes of /nickId/ or ""
quotesStr :: String -> IO String
quotesStr id = do
  nick' <- nickName id
  case nick' of
    Left e -> return ""
    Right nick -> do
      let path = G.path ["data", "quotes", nick ++ ".db"]
      ex <- File.exists path
      if ex
      then File.read path
      else return ""

-- | @'writeQuotes' nickName qs@ - Writes /qs/
writeQuotes :: String -> [Quote] -> IO ()
writeQuotes nickName qs = do
  let qs' = intercalate "\n" $ map Quote.toStr $ take 610 qs
  File.write (G.path ["data", "quotes", nickName ++ ".db"]) qs'

-- | @'updateQuotes' nickId newQs@ - Updates quotes of /nickId/ with quotes
--                                   from a fresh reading.
--
-- If return is "", operation succedded, otherwise returns an error message
updateQuotes :: String -> [Quote] -> IO String
updateQuotes id newQs = do
  nick' <- nickName id
  case nick' of
    Left e -> return e
    Right nick -> do
      qs' <- quotes' nick
      case qs' of
        Left e -> return e
        Right [] -> writeQuotes nick newQs >> return ""
        Right qs@((Quote date _ _ _ _ _ _):_) -> do
          writeQuotes nick $ add (new [] newQs date) qs
          return ""
        where
          add :: [Quote] -> [Quote] -> [Quote]
          add [] qs = qs
          add (q:qs) qs' = add qs (q:qs')
          new r qs date = new' r qs
            where
              new' r [] = r
              new' r (q@(Quote date' _ _ _ _ _ _):qs) =
                if date' > date then new' (q:r) qs
                              else new' r []
