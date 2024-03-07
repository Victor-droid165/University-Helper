module ScreenCleaner    
    ( screenCleaner
    ) where

import Control.Concurrent
import System.Console.ANSI

screenCleaner :: IO ()
screenCleaner = do
    clearScreen
    threadDelay 1000000