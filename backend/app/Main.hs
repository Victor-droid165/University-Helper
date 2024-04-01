module Main (main) where

import Util.ScreenCleaner (screenCleaner)
import Util.Server.API (serveOn)

main :: IO ()
main = do
  screenCleaner
  serveOn