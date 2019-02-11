-- Copyright 08-Feb-2019 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Servers-Companies data base from daily quotes

module Data.DailyDb (
  Data.DailyDb.init,
  Data.DailyDb.read,
  addServer,
  delServer,
  modServer,
  upServer,
  downServer,
  showServer,
  setCode
) where

import Data.List
import qualified Dm.File as File
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import Data.Nick (Nick(..))
import qualified Data.Nick as Nick
import Data.NicksDb (NicksDb(..))
import qualified Data.NicksDb as NicksDb
import qualified Global as G

-- Daily servers and companies data base
data DailyDb = DailyDb {
  servers :: [(String, (Bool, String))], -- [(name, (select, url))]
  cos :: [(String, (Bool, [String]))] -- [(nickName, (select, [serverCode]))]
}

toJs :: DailyDb -> JSValue
toJs (DailyDb svs cos) = Js.wMap [("servers", svsToJs svs),
                                  ("cos", cosToJs cos)]
  where
  svsToJs svs = Js.wMap $ map svToJs svs
  svToJs (k, (sel, url)) = (k, Js.wList [Js.wBool sel, Js.wString url])
  cosToJs cos = Js.wMap $ map (\(k, v) -> (k, codesToJs v)) cos
  codesToJs (sel, cs) = Js.wList [Js.wBool sel, Js.wList $ map Js.wString cs]

fromJs :: JSValue -> DailyDb
fromJs js =
  let db = Js.rMap js
      Just svs = lookup "servers" db
      Just cos = lookup "cos" db
  in  DailyDb (svsFromJs svs) (cosFromJs cos)
  where
    svsFromJs js =  map svFromJs $ Js.rMap js
    svFromJs (k, js) = let [sel, url] = Js.rList js
                       in  (k, (Js.rBool sel, Js.rString url))
    cosFromJs js =  map (\(k, vjs) -> (k, codesFromJs vjs)) $ Js.rMap js
    codesFromJs js = let [sel, cs] = Js.rList js
                     in  (Js.rBool sel, map Js.rString $ Js.rList cs)

path :: FilePath
path = G.path ["data", "daily.db"]

-- | Initializes data base
init :: IO ()
init = File.write path $ Js.toStr $ toJs $ DailyDb [] []

writeDb :: DailyDb -> IO ()
writeDb db = File.write path $ Js.toStr $ toJs db

readDb :: IO DailyDb
readDb = do
  (NicksDb _ _ nks) <- NicksDb.read
  dataStr <- File.read path
  let DailyDb svs cos = fromJs $ Js.fromStr dataStr
  let newCo = map (\_ -> "") svs
  let cos' = update cos nks newCo
  let db = DailyDb svs cos'
  writeDb db
  return db
  where
  update cos nks newCo = rm [] (add cos nks newCo) nks

  add cos [] _ = cos
  add cos ((Nick _ nm _ sel _):nks) newCo =
    case lookup nm cos of
      Nothing -> add ((nm, (sel, newCo)):cos) nks newCo
      _ -> add cos nks newCo

  rm r [] nks = r
  rm r (co@(k, v):cos) nks =
    case find (\(Nick _ nm _ _ _) -> nm == k) nks of
      Nothing -> rm r cos nks
      _ -> rm (co:r) cos nks

-- | Read data base
read :: IO JSValue
read = readDb >>= (return . toJs)

up :: Int -> [a] -> [a]
up ix ls = let (left, (e1:e2:right)) = splitAt ix ls
           in  left ++ (e2:e1:right)

down :: Int -> [a] -> [a]
down ix = up (ix - 1)

index:: String -> [(String, (Bool, String))] -> Maybe Int
index name svs = elemIndex name $ map (\(k, _) -> k) svs

-- | Adds a server. If server name already exists, it does nothing.
addServer :: String -> String -> IO ()
addServer name url = do
  (DailyDb svs cos) <- readDb
  case lookup name svs of
    Just _ -> return ()
    Nothing -> do
      let svs' = ((name, (False, url)):svs)
      let cos' = map addToCos cos
      writeDb (DailyDb svs' cos')
  where
    addToCos (nk, (sel, cds)) = (nk, (sel, "":cds))

-- | Deletes a server. If server name does not exist, it does nothing.
delServer name = do
  (DailyDb svs cos) <- readDb
  case index name svs of
    Nothing -> return ()
    Just ix -> do
      let svs' = take ix svs ++ drop (ix + 1) svs
      let cos' = map (delCos ix) cos
      writeDb (DailyDb svs' cos')
  where
    delCos ix (nk, (sel, cds)) = (nk, (sel, take ix cds ++ drop (ix + 1) cds))

-- | Changes name and/or url of a server. If name is duplicated, it does nothing
modServer :: String -> String -> String -> IO ()
modServer name newName url = do
  (DailyDb svs cos) <- readDb
  if name == newName
  then mod name name url svs cos
  else case lookup newName svs of
          Nothing -> mod name newName url svs cos
          _ -> return ()
  where
    mod name newName url svs cos = do
      let svs' = map (mod' name newName url) svs
      writeDb (DailyDb svs' cos)
    mod' name newName url sv@(name', (sel, url')) =
      if name == name' then (newName, (sel, url)) else sv

-- | Changes position of a server. If it fails, it does nothing.
upServer :: String -> IO ()
upServer name = do
  (DailyDb svs cos) <- readDb
  case index name svs of
    Nothing -> return ()
    Just ix ->
      if ix == length svs - 1 then return ()
      else do
        let svs' = up ix svs
        let cos' = map (upCo ix) cos
        writeDb (DailyDb svs' cos')
  where
    upCo ix (nk, (sel, cds)) = (nk, (sel, up ix cds))

-- | Changes position of a server. If it fails, it does nothing.
downServer :: String -> IO ()
downServer name = do
  (DailyDb svs cos) <- readDb
  case index name svs of
    Nothing -> return ()
    Just ix ->
      if ix == 0 then return ()
      else do
        let svs' = down ix svs
        let cos' = map (downCo ix) cos
        writeDb (DailyDb svs' cos')
  where
    downCo ix (nk, (sel, cds)) = (nk, (sel, down ix cds))

-- | Sets if a server must be shown
showServer :: String -> Bool -> IO ()
showServer name value = do
  (DailyDb svs cos) <- readDb
  let svs' = map (sh name value) svs
  writeDb (DailyDb svs' cos)
  where
    sh name value sv@(nm, (sel, url)) =
      if name == nm then (nm, (value, url)) else sv

-- | Sets the code of a nick in a server.
setCode :: String -> String -> String -> IO ()
setCode server nickName code = do
  (DailyDb svs cos) <- readDb
  case index server svs of
    Nothing -> return ()
    Just ix -> do
      let cos' = map (set ix ) cos
      writeDb (DailyDb svs cos')
  where
    set ix co@(nk, (sel, cds)) =
      if nk == nickName
      then
        let (left, (_:right)) = splitAt ix cds
        in (nk, (sel, left ++ (code:right)))
      else co
