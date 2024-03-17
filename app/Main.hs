module Main (main) where

import Lib (populateDBIfNotPopulated)

main :: IO ()
main = do
    populateDBIfNotPopulated