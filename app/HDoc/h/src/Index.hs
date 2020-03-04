-- Copyright 24-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Index source.

module Index where

import qualified Dm.Cgi as Cgi
import qualified Dm.Js as Js
import qualified Dm.Map as Map
import qualified Dm.File as File
import qualified Dm.Str as Str
import qualified Com.STree as STree

process :: Cgi.T -> Map.T Js.T -> IO ()
process cgi rq = do
  let home = Cgi.home cgi
  let rrq = Cgi.rrq "Index.process" rq
  case rrq "rq" Js.rs of
    "tree" -> do
      t <- mkTree $ rrq "path" Js.rs
      Cgi.rp cgi [("tree", Js.wMaybe STree.toJs t)]
    v -> putStrLn $ "Unexpected value for Index.process:rq: " ++ v

mkTree :: String -> IO (Maybe STree.T)
mkTree root = File.isDirectory root >>=
                \v -> if v then (mkTree1 "") >>= (return . Just)
                           else return Nothing
  where
    mkTree1 rpath = do
      let name = File.name rpath
      let dir = root File.</> rpath
      ls <- File.dir dir
      ls' <- add [] dir rpath ls
      return $ STree.SDir name ls'
    add r _ _ [] = return r
    add r dir rpath (fname:fnames) = do
      isDir <- File.isDirectory $ dir File.</> fname
      if isDir
        then do
          e <- mkTree1 $ rpath File.</> fname
          add (e:r) dir rpath fnames
        else if Str.ends ".hs" fname
                then do
                  doc <- readDoc $ dir File.</> fname
                  let e = STree.SFile (Str.left (-3) fname) doc
                  add (e:r) dir rpath fnames
                else add r dir rpath fnames
    readDoc f = do
      tx <- File.read f
      return $ readLDoc $ map (Str.trim) $ lines tx
    readLDoc [] = ""
    readLDoc (l:ls) = if Str.starts "--- " l then readLDoc' l else readLDoc ls
    readLDoc' l = let l' = Str.ltrim $ Str.right 4 l
                      (r, _) = Str.break ". " l'
                  in  r

