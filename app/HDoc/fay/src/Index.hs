-- Copyright 14-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Index page.

module Index (Init(..), T(wg), new) where

import Dm.Dom (Element, Property(..), q, q', qSel)
import qualified Dm.Client as Client
import qualified Dm.Js as Js
import qualified Dm.Str as Str
import qualified Com.STree as STree
import I18n

--- Init

data Init = Init
  { client :: Client.T
  , pathName :: String
  , filePath :: String
  }

--- T
data T = T
          { ini :: Init
          , tree :: STree.T

          , wg :: Element
          }

-- CONSTRUCTOR

--- new ini fn
--- Constructor
new :: Init -> (Maybe T -> Fay ()) -> Fay ()
new ini fn = do
  Client.send (client ini)
    [ ("source", Js.ws "Index")
    , ("rq", Js.ws "tree")
    , ("path", Js.ws $ filePath ini)
    ]
    ( \r -> do
        wg <- q "div" [] []
        let tree = Client.rrp r "tree" (Js.rMaybe STree.fromJs)
        case tree of
          Nothing -> fn Nothing
          Just t -> do
            let this = T { ini, tree = t, wg }
            view this
            fn $ Just this
    )

-- VIEW

view :: T -> Fay ()
view this = do
  _ <- qSel "title" [ Text (pathName (ini this)) ] []
  let STree.SDir _ ls = newSort $ tree this
  let trs = mkTr "" "" ls
  case trs of
    [] -> q' (wg this) []
            [ q "table" [ Att "align" "center" ]
                [ q "tr" []
                    [ q "td" []
                        [ q "div"
                            [ Class "frame"
                            , Html (__ "Library is empty")
                            ] []
                        ]
                    ]
                ]
            ] >> return ()
    trs' -> q' (wg this) []
              [ q "table" [ Class "frame" ] trs' ] >> return ()
  where
    newSort t@(STree.SFile _ _) = t
    newSort (STree.SDir n ls) =
      let ls' = map newSort ls
      in  STree.SDir n $ sortBy tsort ls'
    tsort (STree.SFile _ _) (STree.SDir _ _) = LT
    tsort (STree.SFile n1 _) (STree.SFile n2 _) =
      Str.compare (Str.toUpper n1) (Str.toUpper n2)
    tsort (STree.SDir n1 _) (STree.SDir n2 _) =
      Str.compare (Str.toUpper n1) (Str.toUpper n2)
    tsort _ _ = GT
    mkTr :: String -> String -> [STree.T] -> [Fay Element]
    mkTr _ _ [] = []
    mkTr prefix path (e:es) =
      case e of
        STree.SFile n doc ->
          q "tr" []
            [ q "td" [ Style "white-space:nowrap" ]
                [ q "a"
                  [ Att "href" ("?" ++ (pathName $ ini this) ++ "&" ++
                                (path </> n))
                  , Html (prefix ++ n)
                  , Class "link"
                  ] []
                ]
            , q "td" [ Style "padding-left:8px", Text doc ] []
            ] : mkTr prefix path es
        STree.SDir n ls ->
          q "tr" []
            [ q "td"
              [ Style "width:5px;white-space:nowrap"
              , Html (prefix ++ "<b>" ++ n ++ "</b>")
              ] []
            ] : mkTr ("&nbsp;&nbsp;&nbsp;&nbsp;" ++ prefix)
                     (path </> n) ls ++
                mkTr prefix path es
    (</>) "" p = p
    (</>) p "" = p
    (</>) p1 p2 = p1 ++ "/" ++ p2

