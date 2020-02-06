-- Copyright 22-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

import Dm.Location as Lc
import qualified Dm.Client as Client
import qualified Dm.Storage as Storage
import Dm.Dom (Element, Property(..), q, qSel, q, addElement)
import qualified Authentication


data Model = Model
  { client :: Client.T
  , app :: String
  , wg :: Element
  }

data Cmd = Main

--- main
--- Main function
main :: Fay ()
main = qSel "body" [] [start] >> return ()

start :: Fay Element
start = do
  let appName = "HDoc"
  let version = "202001"
  key <- Storage.get $ appName ++ "__client"
  w <- q "div" [] []
  let cl = Client.new "http://localhost/cgi-bin/ccgi.sh" appName key
  let model = Model
                { client = cl
                , app = appName
                , wg = w
                }
  Client.connect cl (\rp ->
    case rp of
      Right False -> do
        pg <- Authentication.start cl
                (\k ->
                  let cl' = cl {Client.key = Just k} in
                  update (model {client = cl'}) Main
                )
        addElement w pg
      Right True -> update model Main
      Left e -> fail $ "Main_main: " ++ e
    )

  q "div" []
    [ return (w)
    , q "hr" [] []
    , q "div"
      [ Style "text-align: right;font-size: 10px;\
              \color:#808080;font-size:x-small;"
      , Html ("- © ºDeme. " ++ appName ++ " (" ++ version ++ ") -")
      ] []
    ]

update :: Model -> Cmd -> Fay ()
update _ Main = do
  lc <- Lc.location
  case Lc.search lc of
    [] -> putStrLn "Server page"
    (_, "@"):[] -> putStrLn "Main page"
    (_, l):[] -> putStrLn $ "Page " ++ l
    (_, l):(_, m):[] -> putStrLn $ "Page " ++ l ++ ":" ++ m
    (_, l):(_, m):(_,line):_ -> putStrLn $ "Page " ++ l ++ ":" ++ m ++ line

