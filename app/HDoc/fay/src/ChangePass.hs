-- Copyright 14-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Password change page

module ChangePass (Init(..), T(wg, wgOldPass), new) where

import qualified Dm.Str as Str
import qualified Dm.Js as Js
import qualified Dm.Client as Client
import Dm.Dom (Element, Property(..), q, q', getValue)
import qualified Dm.Ui as Ui
import qualified Dm.Captcha as Captcha
import qualified Dm.Location as Lc
import Dm.Maybe (withFail)
import I18n

--- Init
---  { app :: String
---  , client :: Client.T
---  }
---   app   : Application name. Used to Storage.
---   client: Communication manager.
data Init = Init
  { app :: String
  , client :: Client.T
  }

data T = T
  { ini :: Init
  , captcha :: Captcha.T

  , wg :: Element
  , wgOldPass :: Element
  , wgNewPass :: Element
  , wgNewPass2 :: Element
  }

data Cmd = Accept | Cancel | ResetReload

-- CONSTRUCTOR

--- new ini
--- Constructor
new :: Init -> Fay T
new ini = do
  captcha <- Captcha.new $ Captcha.Init (app ini)
                                        Nothing Nothing Nothing Nothing

  wg <- q "div" [] []
  wgOldPass <- Ui.pass "newPass"
  wgNewPass <- Ui.pass "newPass2" >>= \w -> q' w [ Att "id" "newPass" ] []
  wgNewPass2 <- Ui.pass "accept" >>= \w -> q' w [ Att "id" "newPass2" ] []

  let r = T
            { ini
            , captcha

            , wg
            , wgOldPass
            , wgNewPass
            , wgNewPass2
            }
  view r
  return r

-- VIEW

view :: T -> Fay ()
view model = do
  zeroTries <- Captcha.isZero $ captcha model
  outOfLimit <- Captcha.isOutOfLimit $ captcha model

  table1 <- q "table"
    [ Att "align" "center"
    , Style "background-color: #f8f8f8;\
            \border-collapse: collapse;\
            \padding: 10px; border: 1px solid rgb(110,130,150);"
    ][
      q "tr" []
        [ q "td"
            [ Att "colspan" "2"
            , Style "background-color:#e8e8e8;\
                    \border-bottom:1px solid #c9c9c9;\
                    \padding: 10px;color:#505050;"
            , Html ("<big><big><b>" ++ (__ "Password Change") ++
                    "</b></big></big>")
            ][]
        ]
    , q "tr" []
        [ q "td"
            [ Style "padding: 10px 0px 0px 10px;text-align:right;"
            , Text (__ "Current password")
            ] []
        , q "td"
            [ Style "padding: 10px 10px 0px 10px;"
            ] [ return (wgOldPass model) ]
        ]
    , q "tr" []
        [ q "td"
            [ Style "padding: 5px 0px 0px 10px;text-align:right;"
            , Text (__ "New password")
            ] []
        , q "td"
            [ Style "padding: 5px 10px 0px 10px;"
            ] [ return (wgNewPass model) ]
        ]
    , q "tr" []
        [ q "td"
            [ Style "padding: 5px 0px 10px 10px;text-align:right;"
            , Text (__ "New password")
            ] []
        , q "td"
            [ Style "padding: 5px 10px 10px 10px;"
            ] [ return (wgNewPass2 model) ]
        ]
    , q "tr" []
        [ q "td"
            [ Att "colspan" "2"
            , Style "border-top:1px solid #c9c9c9;\
                    \padding: 10px 10px 10px;text-align:right;"
            ] [
              q "span" []
                [ q "button"
                  [ Att "id" "cancel"
                  , Text (__ "Cancel")
                  , On "click" (update model Cancel)
                  ] []
                , q "span" [ Text "  " ] []
                , q "button"
                  [ Att "id" "accept"
                  , Text (__ "Accept")
                  , On "click" (update model Accept)
                  ] []
                ]
            ]
        ]
    ]

  table2 <-
    if not zeroTries
      then q' table1 []
            [ q "tr" []
                [ q "td"
                    [ Att "colspan" "2"
                    , Style "border-top:1px solid #c9c9c9;\
                            \padding: 10px 10px 10px;text-align:right;"
                    ] [
                      q "table"
                        [ Att "align" "center"
                        , Style "background-color: rgb(250, 250, 250);\
                                \border: 1px solid rgb(110,130,150);\
                                \font-family: sans;font-size: 14px;\
                                \padding: 4px;border-radius: 4px;"
                        ] [
                          q "tr" []
                            [ q "td" [ Html (__ "Wrong password") ] []
                            ]
                        ]
                    ]
                ]
            ]
      else return table1

  table3 <-
    if outOfLimit
      then q' table2 []
            [ q "tr" []
                [ q "td"
                    [ Att "colspan" "2"
                    , Att "align" "center"
                    ] [ return $ Captcha.wg (captcha model) ]
                ]
            , q "tr" []
                [ q "td"
                    [ Att "colspan" "2"
                    , Style "padding: 5px 0px 5px 10px;text-align:center;"
                    , Html (__ "Check gray squares")
                    ] []
                ]
            ]
      else return table2

  q' (wg model) [RemoveAll]
        [ q "div"
          [ Class "head"
          , Html (app $ ini model)
          ][]
        , q "p" [ Style "width:30px" ] []
        , return table3
        ] >> q' (wgOldPass model) [Focus] [] >> return ()

-- UPDATE

update :: T -> Cmd -> Fay ()

update model Accept = do
  let old = Str.trim $ getValue $ wgOldPass model
  let new1 = Str.trim $ getValue $ wgNewPass model
  let new2 = Str.trim $ getValue $ wgNewPass2 model
  case findError old new1 new2 of
    Just e -> Ui.alert e >> update model ResetReload
    Nothing -> do
      outOfLimit <- Captcha.isOutOfLimit $ captcha model
      cpOk <- if outOfLimit then Captcha.check (captcha model) else return True
      if cpOk then do
        let user = withFail (Client.user $ client $ ini model)
                            "ChangePass.update: User is Nothing"
        Client.send (client (ini model))
          [ ("source", Js.ws "ChangePass")
          , ("user", Js.ws user)
          , ("old", Js.ws $ Client.crypPass old)
          , ("new", Js.ws $ Client.crypPass new1)
          ]
          ( \rp ->  if Client.rrp rp "ok" Js.rb
                      then Ui.alert (__ "Password successfully changed") >>
                           Lc.reload
                      else update model ResetReload
          )
      else do
        Ui.alert (__ "Grey squares checks are wrong")
        update model ResetReload

  where
    findError "" _ _ = Just (__ "Current password is missing")
    findError _ "" _ = Just (__ "New password is missing")
    findError _ _ "" = Just (__ "Confirm password is missing")
    findError _ n1 n2
      | n1 /= n2 = Just (__ "New password and confirm password do not match")
      | True = Nothing


update _ Cancel = do
  Lc.reload

update model ResetReload = q' (wgOldPass model) [ Value "" ] [] >>
                           q' (wgNewPass model) [ Value "" ] [] >>
                           q' (wgNewPass2 model) [ Value "" ] [] >>
                           Captcha.inc (captcha model) >>
                           Captcha.newCombination (captcha model) >>
                           view model
