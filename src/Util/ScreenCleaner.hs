module Util.ScreenCleaner
    ( screenCleaner,
    ) where

import Control.Concurrent ( threadDelay )
import System.Console.ANSI


screenCleaner :: IO ()
screenCleaner = do
    threadDelay 1000000
    clearScreen