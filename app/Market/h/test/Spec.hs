
import qualified Data.Servers.Yahoo as Yahoo
import qualified Data.Servers.Invertia as Invertia
import qualified Data.Servers.Finanzas as Finanzas

main :: IO ()
main = do
  yq <- Yahoo.quote "SAN"
  putStrLn $ show yq
  iq <- Invertia.quote "SAN"
  putStrLn $ show iq
  fq <- Finanzas.quote "SAN"
  putStrLn $ show fq
