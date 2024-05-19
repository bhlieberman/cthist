module Main where

import Scraping.Core (showVersions)

main :: IO ()
main = do
  versions <- showVersions
  mapM_ print versions
