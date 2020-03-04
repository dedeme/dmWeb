-- Copyright 13-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Data base.

module Db
  ( ini
  , getLpath
  , setLpath
  , getLang
  , setLang
  , getShowAll
  , setShowAll
  , getPaths
  , setPaths
  , newPath
  , setShow
  , delPath
  , modifyPath
  ) where

import qualified Dm.File as File
import qualified Dm.Js as Js
import qualified Dm.Map as Map
import Dm.Either (Result, withFail)
import qualified Dm.Maybe as Maybe
import qualified Com.PathEntry as PathEntry

confPath :: String -> String
confPath home = home ++ "/data/conf.db"

lpathsPath :: String -> String
lpathsPath home = home ++ "/data/lpaths.db"

--- ini home
--- Initializes data base
ini :: String -> IO ()
ini home = do
  let dbDir = home ++ "/data"
  ex <- File.exists dbDir
  if ex then return ()
  else do
    File.mkDir dbDir
    File.write (confPath home) $ Js.toStr $ Js.wo $ []
    File.write (lpathsPath home) $ Js.toStr $ Js.wa $ []

-- CONF

confRead :: String -> IO (Map.T Js.T)
confRead home = (\s -> withFail (Js.fromStr s >>= Js.ro)) <$>
                  File.read (confPath home)

confWrite :: String -> Map.T Js.T -> IO ()
confWrite home cf = File.write (confPath home) $
                               Js.toStr $ Js.wo cf

confGet :: String -> String -> a -> (Js.T -> Result a) -> (a -> Js.T) -> IO a
confGet home key def rfn wfn = do
  cf <- confRead home
  return $ withFail $ rfn $ Maybe.fromMaybe (wfn def) $ Map.get key cf

confSet :: String -> String -> a -> (a -> Js.T) -> IO ()
confSet home key value fn = do
  cf <- confRead home
  confWrite home $ Map.pack $  Map.put key (fn value) cf


--- getLpath home
--- Returns default lpath.
getLpath :: String -> IO [String]
getLpath home = confGet home "lpath" ["@"] (Js.rList Js.rs) (Js.wList Js.ws)

--- setLpath home lpath
--- Sets default lpath.
setLpath :: String -> [String] -> IO ()
setLpath home lpath = confSet home "lpath" lpath (Js.wList Js.ws)

--- getLang home
--- Returns application language ['en' or 'es'] (default 'en').
getLang :: String -> IO String
getLang home = confGet home "lang" "en" Js.rs Js.ws

--- setLang home lang
--- Sets default lpat. 'lpath' must include the symbol '?'.
setLang :: String -> String -> IO ()
setLang home lang = confSet home "lang" lang Js.ws

--- getShowAll home
--- Returns 'True' if every path is shown.
getShowAll :: String -> IO Bool
getShowAll home = confGet home "showAll" True Js.rb Js.wb

--- setShowAll home v
--- Sets if every path must be shown.
setShowAll :: String -> Bool -> IO ()
setShowAll home v = confSet home "showAll" v Js.wb

-- LPATHS

--- getPaths
getPaths :: String -> IO [PathEntry.T]
getPaths home = do
  tx <- File.read $ lpathsPath home
  let ls = withFail $ Js.rList (PathEntry.fromJs) $ withFail $ Js.fromStr tx
  mapM setExists ls
  where
    setExists e = do
      ex <- File.isDirectory (PathEntry.path e)
      return $ e { PathEntry.exists = ex }

--- setPaths
setPaths :: String -> [PathEntry.T] -> IO ()
setPaths home paths =
  File.write (lpathsPath home) $ Js.toStr $ Js.wList (PathEntry.toJs) paths

--- newPath home name path
--- Returns 'False' if name is duplicated
newPath :: String -> String -> String -> IO Bool
newPath home name path = do
  ls <- getPaths home
  if any (\p -> PathEntry.name p == name) ls then return False
  else do
    setPaths home (PathEntry.new name path : ls)
    return True

--- setShow value
setShow :: String -> String -> Bool -> IO ()
setShow home name value = do
  ls <- getPaths home
  setPaths home $ map setValue ls
  where
    setValue e = if (PathEntry.name e) == name
                   then e { PathEntry.selected = value }
                   else e

--- delPath home name
delPath :: String -> String -> IO ()
delPath home name = do
  ls <- getPaths home
  setPaths home $ filter (\e -> (PathEntry.name e) /= name) ls

modifyPath :: String -> String -> String -> String -> IO Bool
modifyPath home oldName newName path = do
  ls <- getPaths home
  if (oldName /= newName) && any (\p -> PathEntry.name p == newName) ls
    then return False
    else do
      setPaths home $ map setValue ls
      return True
  where
    setValue e = if (PathEntry.name e) == oldName
                   then e
                          { PathEntry.name = newName
                          , PathEntry.path = path
                          }
                   else e
