module ServerMain where

import Server (runServer)
import System.Environment (getEnv)

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  runServer port
