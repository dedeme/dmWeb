-- Copyright 14-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Paths page.

module Paths (Init(..), T(wg, wgNewName), new) where

import Dm.Dom (Element, Property(..), q, q', getValue)
import qualified Dm.Ui as Ui
import qualified Dm.Client as Client
import qualified Dm.Location as Lc
import qualified Dm.Map as Map
import qualified Dm.Js as Js
import qualified Dm.Str as Str
import I18n
import qualified MsgPg
import qualified ChangePass
import qualified Com.PathEntry as PathEntry

--- Init

data Init = Init
  { app :: String
  , client :: Client.T
  , rp :: Map.T Js.T

  , mainWg :: Element
  }

--- T
data T = T
          { ini :: Init

          , lang :: String
          , modifiyng :: Maybe String

          , wg :: Element
          , wgNewName :: Element
          , wgNewPath :: Element
          , wgShowAll :: Element
          , wgMdName :: Element
          , wgMdPath :: Element
          }

data Cmd = New
         | ChangeLang
         | ChangePass
         | ChangeShowAll
         | SetShow String Bool
         | Edit String
         | EditCancel
         | EditAccept String
         | Delete String
         | Error String

validateName :: [String] -> String -> String
validateName _ "" = (__ "Name is missing")
validateName names n =
  if any (== n) names then (__ "Name is duplicate")
  else validateChar " =@/?<&"
  where
    validateChar "" = ""
    validateChar (c:cs) =
      if any (== c) n then i18nFormat (__ "Name contains '%0'") [[c]]
                      else validateChar cs

validatePath :: String -> String
validatePath "" = (__ "Path is missing")
validatePath _ = ""

-- CONSTRUCTOR

--- new ini
--- Constructor
new :: Init -> Fay T
new ini = do
  wg <- q "div" [] []
  wgNewName <- Ui.field "newPath" >>= \w -> q' w [ Att "size" "20" ] []
  wgNewPath <- Ui.field "newBt" >>= \w -> q' w
                                            [ Att "id" "newPath"
                                            , Att "size" "60"
                                            ] []
  wgMdName <- Ui.field "mdPath" >>= \w -> q' w
                                            [ Att "id" "mdPath"
                                            , Att "size" "20"
                                            ] []
  wgMdPath <- Ui.field "mdName" >>= \w -> q' w
                                            [ Att "id" "mdName"
                                            , Att "size" "60"
                                            ] []
  wgShowAll <- q "div" [] []

  let model = T
                { ini

                , lang = Client.rrp (rp ini) "lang" Js.rs
                , modifiyng = Nothing

                , wg
                , wgNewName
                , wgNewPath
                , wgShowAll
                , wgMdName
                , wgMdPath
                }

  view model
  return model

-- VIEW

view :: T -> Fay ()
view model = do
  let r = rp $ ini model
  let md = modifiyng model /= Nothing
  newImg <- Ui.img "new"
  newBt <-  Ui.img "enter" >>=
            \w -> q "button" [ Att "id" "newBt", Style "width:80px" ]
                    [ q' w [ Style "vertical-align:-10%" ] [] ]
  newBt' <- if md then q' newBt [ Disabled True ] []
                  else q' newBt
                    [ Disabled False
                    , On "click" $ update model New
                    ] []
  newName <- if md then q' (wgNewName model) [Disabled True] []
                   else q' (wgNewName model) [Disabled False] []
  newPath <- if md then q' (wgNewPath model) [Disabled True] []
                   else q' (wgNewPath model) [Disabled False] []
  let showAllValue = Client.rrp r "showAll" Js.rb
  let showAllImg = if showAllValue then "out" else "in"
  showAll <- if md then q' (wgShowAll model) [ RemoveAll ]
                          [ Ui.lightImg showAllImg ]
                   else q' (wgShowAll model) [ RemoveAll ]
                          [ Ui.img showAllImg >>=
                              \w -> Ui.eLink (w) (update model ChangeShowAll)
                          ]
  let ls = filterSort (Client.rrp r "paths" (Js.rList PathEntry.fromJs))
                      showAllValue
  let rows = case ls of
                [] -> [ q "tr" []
                          [ q "td"
                            [ Class "frame"
                            , Style "text-align:center"
                            , Att "colspan" "6"
                            , Html (__ "There are no libraries")
                            ] []
                          ]
                      ]
                ls' -> map row ls'
  _ <- q' (wg model) [RemoveAll] $
    [ q "div" [ Class "head", Text (__ "Libraries") ] []
    , q "table"
        [ Class "border"
        , Att "align" "center"
        , Style "background-color: rgb(255, 250, 250)"
        ]$[
          q "tr" []
            [ q "td" []
                [ q' (newImg) [ Style "vertical-align:-15%" ] [] ]
            , q "td" [ Att "colspan" "2" ] [ return newBt' ]
            , q "td" [] [ return newName ]
            , q "td" [] [ return newPath ]
            , q "td" [] []
            ]
          , q "tr" []
            [ q "td" [] [ return showAll ]
            , q "td" [ Att "colspan" "2" ] []
            , q "td" [ Html ("&nbsp;&nbsp;<b>" ++ (__ "Name") ++ "</b>") ] []
            , q "td" [ Html ("&nbsp;&nbsp;<b>" ++ (__ "Path") ++ "</b>") ] []
            , q "td" [] []
            ]
        ] ++ rows
    ] ++
    [ q "p" [ Style "text-align:center" ]
        [ Ui.tLink (i18nFormat (__ "Change Language to %0") $
                     if (lang model) == "es" then ["EN"] else ["ES"]) $
                   update model ChangeLang
        , q "span" [ Html "&nbsp;|&nbsp;" ] []
        , Ui.tLink (__ "Change Password") $ update model ChangePass
        ]
    ]
  _ <- q' (if md then wgMdName model else wgNewName model) [ Focus ] []
  return ()
  where
    filterSort ls True = psort ls
    filterSort ls False = psort $ filter (\e -> PathEntry.selected e) ls
    psort = sortBy (\e1 -> \e2 ->
                     Str.compare (Str.toUpper (PathEntry.name e1))
                                 (Str.toUpper (PathEntry.name e2))
                   )
    row :: PathEntry.T -> Fay Element
    row (PathEntry.T name path sel ex) = do
      let md = modifiyng model
      let showImg = if sel then "out" else "in"
      let exImg = if ex then "well" else "error"
      sh <- case md of
              Just nm | nm /= name -> Ui.lightImg showImg
              Just _ -> Ui.img "blank"
              _ -> Ui.img showImg >>=
                   \w -> Ui.eLink w $ update model $ SetShow name $ not sel
      mf <- case md of
              Just nm | nm /= name -> Ui.lightImg "edit"
              Just _ -> Ui.img "cancel" >>=
                         \w -> Ui.eLink w $ update model $ EditCancel
              _ -> Ui.img "edit" >>=
                   \w -> Ui.eLink w $ update model $ Edit name
      rm <- case md of
              Just nm | nm /= name -> Ui.lightImg "delete"
              Just _ -> Ui.img "enter" >>=
                         \w -> Ui.eLink w $ update model $ EditAccept name
              _ -> Ui.img "delete" >>=
                   \w -> Ui.eLink w $ update model $ Delete name
      nm <- case md of
              Just nm | nm /= name -> q "span"
                                        [ Style "opacity:0.4",
                                          Html name
                                        ] []
              Just _ -> q' (wgMdName model) [ Value name ] []
              _ -> q "span" [ Html name ] []
      pt <- case md of
              Just n | n /= name -> q "span"
                                        [ Style "opacity:0.4",
                                          Html path
                                        ] []
              Just _ -> q' (wgMdPath model) [ Value path ] []
              _ -> q "span" [ Html path ] []
      et <- case md of
              Just n | n /= name -> Ui.lightImg exImg
              _ -> Ui.img exImg
      q "tr" []
        [ q "td" [] [ return sh ]
        , q "td" [ Style "text-align:center" ] [ return mf ]
        , q "td" [ Style "text-align:center" ] [ return rm ]
        , q "td" [ Class "border" ] [ return nm ]
        , q "td" [ Class "border" ] [ return pt ]
        , q "td" [] [ return et ]
        ]
-- UPDATE

update :: T -> Cmd -> Fay ()

update model New = do
  let n = Str.trim $ getValue $ wgNewName model
  let p = Str.trim $ getValue $ wgNewPath model
  let paths = Client.rrp (rp $ ini model) "paths" (Js.rList PathEntry.fromJs)
  let names = map (PathEntry.name) paths
  let vn = validateName names n
  let vp = validatePath p
  if vn == "" && vp == ""
    then
      Client.send (client (ini model))
        [ ("source", Js.ws "Paths")
        , ("rq", Js.ws "new")
        , ("name", Js.ws n)
        , ("path", Js.ws p)
        ]
        ( \r ->
          let err = Client.rrp r "error" Js.rs
          in  if err == "" then Lc.reload
          else update model $ Error err
        )
    else if vn == "" then Ui.alert vp else Ui.alert vn

update model ChangeShowAll = do
  let value = not $ Client.rrp (rp $ ini model) "showAll" Js.rb
  Client.send (client (ini model))
    [ ("source", Js.ws "Paths")
    , ("rq", Js.ws "changeShowAll")
    , ("value", Js.wb value)
    ]
    ( Client.discardRp Lc.reload )

update model (SetShow name value) =
  Client.send (client (ini model))
    [ ("source", Js.ws "Paths")
    , ("rq", Js.ws "setShow")
    , ("name", Js.ws name)
    , ("value", Js.wb value)
    ]
    ( Client.discardRp Lc.reload )

update model (Edit name) = view (model { modifiyng = Just name })

update model EditCancel = view (model { modifiyng = Nothing })

update model (EditAccept name) = do
  let n = Str.trim $ getValue $ wgMdName model
  let p = Str.trim $ getValue $ wgMdPath model
  let vn = if n /= name
            then let paths = Client.rrp (rp $ ini model) "paths"
                                        (Js.rList PathEntry.fromJs)
                     names = map (PathEntry.name) paths
                 in  validateName names n
            else ""
  let vp = validatePath p
  if vn == "" && vp == ""
    then
      Client.send (client (ini model))
        [ ("source", Js.ws "Paths")
        , ("rq", Js.ws "modify")
        , ("oldName", Js.ws name)
        , ("newName", Js.ws n)
        , ("path", Js.ws p)
        ]
        ( \r ->
          let err = Client.rrp r "error" Js.rs
          in  if err == "" then Lc.reload
              else update model $ Error err
        )
    else if vn == "" then Ui.alert vp else Ui.alert vn

update model (Delete name) = do
  confirm <- Ui.confirm $ i18nFormat (__ "Delete '%0'?") [name]
  if confirm then
                  Client.send (client (ini model))
                    [ ("source", Js.ws "Paths")
                    , ("rq", Js.ws "delete")
                    , ("name", Js.ws name)
                    ]
                    ( Client.discardRp Lc.reload )
  else return ()

update model ChangeLang =
  Client.send (client (ini model))
    [ ("source", Js.ws "Paths")
    , ("rq", Js.ws "changeLang")
    , ("lang", Js.ws $ if (lang model) == "es" then "en" else "es")
    ]
    ( Client.discardRp Lc.reload )

update model ChangePass = do
  pg <- ChangePass.new $ ChangePass.Init
    { ChangePass.app = app (ini model)
    , ChangePass.client = client (ini model)
    }
  _ <- q' (mainWg (ini model)) [RemoveAll] [ return (ChangePass.wg pg) ]
  _ <- q' (ChangePass.wgOldPass pg) [ Focus ] []
  return ()

update model (Error e) = do
  pg <- MsgPg.new MsgPg.Init {
    MsgPg.title = app $ ini model
  , MsgPg.msg = e
  , MsgPg.reload = True
  }
  _ <- q' (mainWg (ini model)) [ RemoveAll ] [ return $ MsgPg.wg pg ]
  return ()
