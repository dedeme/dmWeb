-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Authentication page.

module Authentication (Init(..), T(wg, wgUser), new) where

import qualified Dm.Client as Client
import Dm.Dom (Element, Property(..), q, q', getValue, isChecked)
import qualified Dm.Ui as Ui
import qualified Dm.Storage as Storage
import qualified Dm.Captcha as Captcha
import qualified Dm.Location as Lc
import I18n

--- Init
---  { app :: String
---  , client :: Client.T
---  , fnOk :: (Client.T -> Fay ())
---  }
---   app   : Application name. Used to Storage.
---   client: Communication manager.
---   fnOk  : (fnOk cl) Function runned when authentication is Ok. 'cl' is
---           the intial client modified with the 'key' field updated.
data Init = Init
  { app :: String
  , client :: Client.T
  , fnOk :: (Client.T -> Fay ())
  }

--- T
data T = T
  { ini :: Init
  , lang :: String
  , captcha :: Captcha.T

  , wg :: Element
  , wgUser :: Element
  , wgPass :: Element
  , wgPersistent :: Element
  }

data Cmd = OnClick | ChangeLang

getLang :: String -> Fay String
getLang appName = do
  l <- Storage.get $ appName ++ "__language"
  case l of
    Just lg -> return lg
    _ -> return "es"

setLang :: String -> String -> Fay ()
setLang appName lg = Storage.set (appName ++ "__language") lg

-- CONSTRUCTOR

--- new ini
--- Constructor
new :: Init -> Fay T
new ini = do
  lang <- getLang $ app ini
  i18nInit lang
  captcha <- Captcha.new $ Captcha.Init (app ini)
                                        Nothing Nothing Nothing Nothing

  wg <- q "div" [] []
  wgUser <- Ui.field "pass"
  wgPass <- Ui.pass "accept" >>= \w -> q' w [ Att "id" "pass" ] []
  wgPersistent <- q "input"
    [ Att "type" "checkbox"
    , Style "vertical-align: middle"
    , Checked True
    ] []

  let r = T
            { ini
            , lang
            , captcha

            , wg
            , wgUser
            , wgPass
            , wgPersistent
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
          , Html ("<big><big><b>" ++ (__ "Login") ++ "</b></big></big>")
          ][]]
    , q "tr" []
        [ q "td"
            [ Style "padding: 10px 0px 0px 10px;text-align:right;"
            , Text (__ "User")
            ] []
        , q "td" [Style "padding: 10px 10px 0px 10px;"]
            [ return $ wgUser model ]]
    , q "tr" []
        [ q "td"
            [ Style "padding: 10px 0px 0px 10px;text-align:right;"
            , Text (__ "Password")
            ] []
        , q "td" [Style "padding: 10px 10px 5px 10px;"]
            [ return $ wgPass model ]]
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
                        return $ wgPersistent model
                      , q "span"
                        [ Html ("&nbsp;" ++ (__ "Keep connected")) ][]]]
                , q "tr" []
                    [ q "td" []
                      [ Ui.tLink
                          (if (lang model) == "es" then "EN" else "ES")
                          (update model ChangeLang)
                      ]
                    , q "td" [ Att "align" "right" ]
                        [ q "button"
                          [ Att "id" "accept"
                          , Text (__ "Accept")
                          , On "click" (update model OnClick)
                          ] []
                        ]
                    ]
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
        ] >> q' (wgUser model) [Focus] [] >> return ()

-- UPDATE

update :: T -> Cmd -> Fay ()

update model OnClick = do
  case (val $ getValue $ wgUser model, val $ getValue $ wgPass model) of
    (Just user, Just pass) -> do
      outOfLimit <- Captcha.isOutOfLimit $ captcha model
      cpOk <- if outOfLimit then Captcha.check (captcha model) else return True
      if cpOk then do
        let client' = client $ ini model
        let expiration = not $ isChecked $ wgPersistent model
        Client.authentication client' user pass expiration fn
      else do
        Ui.alert (__ "Grey squares checks are wrong")
        _ <- q' (wgUser model) [Value ""] []
        _ <- q' (wgPass model) [Value ""] []
        Captcha.newCombination $ captcha model
        view model
    (Nothing, _) -> Ui.alert (__ "User name is missing")
    (_, Nothing) -> Ui.alert (__ "Password is missing")
  where
    val "" = Nothing
    val s = Just s
    fn False = do
      _ <- q' (wgUser model) [Value ""] []
      _ <- q' (wgPass model) [Value ""] []
      Captcha.inc $ captcha model
      Captcha.newCombination $ captcha model
      view model
    fn True = do
      Captcha.reset $ captcha model
      Lc.reload

update model ChangeLang = do
  let appName = app (ini model)
  lg <- getLang $ appName
  let lg' = if lg == "es" then "en" else "es"
  setLang appName lg'
  Lc.reload
