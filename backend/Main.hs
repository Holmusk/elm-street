module Main where

import Network.Wai.Handler.Warp (run)

import Api (app)


main :: IO ()
main = run 8080 app
