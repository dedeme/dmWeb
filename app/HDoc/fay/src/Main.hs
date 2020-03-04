-- Copyright 22-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

--- Application entry.

{-# LANGUAGE NamedFieldPuns #-}

import qualified Dm.Location as Lc
import qualified Dm.Client as Client
import qualified Dm.Js as Js
import qualified Dm.Map as Map
import qualified Dm.Menu as Menu
import qualified Dm.Ui as Ui
import qualified Dm.Str as Str
import Dm.Dom (Element, Property(..), q, qSel, q')
import I18n
import qualified Authentication as Auth
import qualified Paths
import qualified MsgPg
import qualified Index
import qualified Module
import qualified Com.PathEntry as PathEntry

data T = T
  { client :: Client.T
  , app :: String

  , wg :: Element
  , body :: Element
  }

data Cmd =  Connect
          | ServerPage
          | Main
          | Main2 [(String, String)] (Map.T Js.T)
          | Bye

appName :: String
appName = "HDoc"

version :: String
version = "202001"

--- main
--- Main function
main :: Fay ()
main = do
  m <- new
  _ <- qSel "body" [] [return $ wg m]
  update m Connect

-- CONSTRUCTOR

new :: Fay T
new = do
  wg <- q "div" [] []
  body <- q "div" [] []
  client <- Client.new $ Client.Init
              { Client.url = "http://localhost/cgi-bin/ccgi.sh"
              , Client.app = appName
              , Client.fnExpiration = do
                  pg <- MsgPg.new $ MsgPg.Init
                    { MsgPg.title = appName
                    , MsgPg.msg = (__ "Session is expired.")
                    , MsgPg.reload = True
                    }
                  _ <- q' body [ RemoveAll ] [ return $ MsgPg.wg pg ]
                  return ()
              , Client.fnFail = \e -> do
                  pg <- MsgPg.new $ MsgPg.Init
                    { MsgPg.title = appName
                    , MsgPg.msg = e
                    , MsgPg.reload = False
                    }
                  _ <- q' body [ RemoveAll ] [ return $ MsgPg.wg pg ]
                  return ()
              }

  let this = T
            { client
            , app = appName

            , wg
            , body
            }

  view this
  return this

-- VIEW

view :: T -> Fay ()
view this = do
  _ <- q' (wg this) [RemoveAll]
        [ return (body this)
        , q "hr" [] []
        , q "div"
          [ Style "text-align: right;font-size: 10px;\
                  \color:#808080;font-size:x-small;"
          , Html ("- © ºDeme. " ++ appName ++ " (" ++ version ++ ") -")
          ] []
        ]
  return ()

-- UPDATE

update :: T -> Cmd -> Fay ()
update this cmd =
  case cmd of
    Connect -> do
      let cl = client this
      Client.connect cl (\rp ->
        case rp of
          Nothing -> do
            auth <- Auth.new $ Auth.Init
                      { Auth.app = appName
                      , Auth.client = cl
                      , Auth.fnOk =
                        (\cl' ->
                          update (this {client = cl'}) Main
                        )
                      }
            q' (body this) [ RemoveAll ] [ return $ Auth.wg auth] >>
              q' (Auth.wgUser auth) [ Focus ] [] >> return ()
          Just cl' -> update (this {client = cl'}) Main
        )
    ServerPage -> do
      Client.send (client this)
        [ ("source", Js.ws "Main")
        , ("rq", Js.ws "lcPath")
        ]
        ( \rp -> Lc.replace $ Client.rrp rp "lcPath" Js.rs )
    Main -> do
      lc <- Lc.search
      case lc of
        [] -> update this ServerPage
        _ -> Client.send (client this)
              [ ("source", Js.ws "Main")
              , ("rq", Js.ws "idata")
              ]
              ( \rp -> do
                  let lang = Client.rrp rp "lang" Js.rs
                  i18nInit lang
                  update this $ Main2 lc rp
              )
    Main2 lc rp -> do
      goPathsImg <- (Ui.img "asterisk") >>=
                      \w -> q' w [ Style "vertical-align: middle" ] []
      mnGoPaths <- Menu.imgOption "?@" goPathsImg $ Lc.assign "?@"
      let fpaths =
            sortBy
              (\e1 -> \e2 -> Str.compare (Str.toUpper (PathEntry.name e1))
                                         (Str.toUpper (PathEntry.name e2))
              ) $
              filter (\e -> PathEntry.selected e && PathEntry.exists e) $
                Client.rrp rp "paths" (Js.rList PathEntry.fromJs)
      mnGos <- mapM mkLink fpaths
      mnGos' <- separate (mnGoPaths : mnGos)
      mnClose <- Menu.close $ update this Bye

      wgMenu <- q "div" [] []
      wgBody <- q "div" [] []
      _ <- q' (body this) [ RemoveAll ]
              [ return wgMenu
              , return wgBody
              ]
      case lc of
        [] -> fail "Main:update: Unexpected empty array"
        (_, "@"):[] -> do
          menu <- Menu.new $ Menu.Init
                  { Menu.selected = "?@"
                  , Menu.left = mnGos'
                  , Menu.right = [ mnClose ]
                  , Menu.withSeparator = False
                  }
          _ <- q' wgMenu [ RemoveAll ] [ return (Menu.wg menu) ]
          pg <- Paths.new $ Paths.Init
            { Paths.app = (app this)
            , Paths.client = (client this)
            , Paths.rp = rp
            , Paths.mainWg = (body this)
            }
          q' wgBody [] [ return (Paths.wg pg) ] >>
            q' (Paths.wgNewName pg) [ Focus ] [] >> return ()
        (_, l): rs -> do
          menu <- Menu.new $ Menu.Init
                  { Menu.selected = l
                  , Menu.left = mnGos'
                  , Menu.right = [ mnClose ]
                  , Menu.withSeparator = False
                  }
          _ <- q' wgMenu [ RemoveAll ] [ return (Menu.wg menu) ]
          case rs of
            [] -> goIndex wgBody fpaths l
            (_, m):[] -> goModule wgBody fpaths l m
            (_, m):(_,line):_ -> putStrLn $ "Page " ++ l ++ ":" ++ m ++ line
    Bye ->
      case Client.sessionId (client this) of
        Nothing -> byePage
        Just ssId -> do
          Client.send (client this)
            [ ("source", Js.ws "Main")
            , ("rq", Js.ws "bye")
            , ("sessionId", Js.ws ssId)
            ]
            ( Client.discardRp byePage )
  where
    mkLink e = let name = (PathEntry.name e)
               in  Menu.txLink Nothing name name
    separate :: [Menu.Entry] -> Fay [Menu.Entry]
    separate [] = return []
    separate [e] = return [e]
    separate (e:es) = do
      s <- Menu.separator
      rs <- separate es
      return (e: s : rs)

    goIndex wgBody fpaths l =
      case find (\p -> PathEntry.name p == l) fpaths of
        Nothing -> Lc.replace "?@"
        Just p -> Index.new Index.Init
                          { Index.client = client this
                          , Index.pathName = l
                          , Index.filePath = PathEntry.path p
                          }
                          ( \pg ->
                              case pg of
                                Nothing -> Lc.replace "?@"
                                Just pg' ->
                                  q' wgBody [ RemoveAll ]
                                    [ return (Index.wg pg') ] >> return ()
                          )

    goModule wgBody fpaths l m =
      case find (\p -> PathEntry.name p == l) fpaths of
        Nothing -> Lc.replace "?@"
        Just p -> Module.new Module.Init
                    { Module.client = client this
                    , Module.pathName = l
                    , Module.modulePath = m
                    , Module.filePath = PathEntry.path p ++ "/" ++ m ++ ".hs"
                    }
                    ( \pg ->
                        case pg of
                          Nothing -> Lc.replace "?@"
                          Just pg' ->
                            q' wgBody [ RemoveAll ]
                              [ return (Module.wg pg') ] >> return ()
                    )

    byePage = do
      pg <- MsgPg.new $ MsgPg.Init
        { MsgPg.title = appName
        , MsgPg.msg = i18nFormat (__ "Logout-message") [appName]
        , MsgPg.reload = False
        }
      _ <- q' (body this) [ RemoveAll ] [ return $ MsgPg.wg pg ]
      return ()

