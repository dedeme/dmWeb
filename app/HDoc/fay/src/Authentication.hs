-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Authentication page.

module Authentication (start) where

import qualified Dm.Client as Client
import Dm.Dom (Element, Property(..), q, q')
import qualified Dm.Ui as Ui
import qualified Dm.Storage as Storage
import qualified I18n

data Model = Model
  { __ :: String -> String
  , lang :: String
  , fnOk :: String -> Fay ()

  , wgUser :: Element
  , wgPass :: Element
  , wgPersistent :: Element
  }

data Cmd = OnClick | ChangeLang

getLang :: Client.T -> Fay String
getLang cl = do
  l <- Storage.get $ (Client.app cl) ++ "__language"
  case l of
    Just lg -> return lg
    _ -> return "es"

start :: Client.T -> (String -> Fay ()) -> Fay Element
start client fnOk = do
  lang <- getLang client
  let __ = I18n.trs lang

  wgUser <- Ui.field "pass" >>= \w -> q' w [ Att "autofocus" "true"] []
  wgPass <- Ui.pass "accept" >>= \w -> q' w [ Att "id" "pass"] []
  wgPersistent <- q "input"
    [ Att "type" "checkbox"
    , Style "vertical-align: middle"
    , Checked True
    ] []

  let model = Model
                { __
                , lang
                , fnOk
                , wgUser
                , wgPass
                , wgPersistent
                }

  q "div" []
    [ q "div"
      [ Class "head"
      , Html (Client.app client)
      ][]
    , q "p" [Style "width:30px"] []
    , q "table"
      [ Att "align" "center"
      , Style "background-color: #f8f8f8;\
              \border-collapse: collapse;\
              \padding: 10px; border: 1px solid rgb(110,130,150);"
      ] [
        q "tr" []
          [ q "td"
            [ Att "colspan" "2"
            , Style "background-color:#e8e8e8;\
                    \border-bottom:1px solid #c9c9c9;\
                    \padding: 10px;color:#505050;"
            , Html ("<big><big><b>" ++ (__ "Login") ++ "</b></big></big>")
            ][]]
      , q "tr" []
          [ q "td"
              [ Style "padding: 10px 0px 0px 10px;text-align:right;"
              , Text (__ "User")
              ] []
          , q "td" [Style "padding: 10px 10px 0px 10px;"] [ return wgUser ]]
      , q "tr" []
          [ q "td"
              [ Style "padding: 10px 0px 0px 10px;text-align:right;"
              , Text (__ "Password")
              ] []
          , q "td" [Style "padding: 10px 10px 5px 10px;"] [ return wgPass ]]
      , q "tr" []
          [ q "td"
            [ Att "colspan" "2"
            , Style "border-top:1px solid #c9c9c9;\
                    \padding: 5px 10px 10px;text-align:right;"
            ][
              q "table"
                [ Class "main"]
                [ q "tr" []
                    [ q "td"
                      [ Att "align" "center"
                      , Att "colspan" "2"
                      ][
                        return wgPersistent
                      , q "span"
                        [ Html ("&nbsp;" ++ (__ "Keep connected")) ][]]]
                , q "tr" []
                    [ q "td" []
                      [ Ui.tLink
                          (if lang == "es" then "EN" else "ES")
                          (update model ChangeLang)
                      ]
                    , q "td" [ Att "align" "right" ]
                        [ q "button"
                          [ Att "id" "accept"
                          , Text (__ "Accept")
                          , On "click" (update model OnClick)
                          ] []]]]]]]]

update :: Model -> Cmd -> Fay ()
update model OnClick = do
  (fnOk model) "Ok"

update model ChangeLang = do
  print $ lang model
