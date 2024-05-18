module Main where

import Scraping.Core (showVersions)

main :: IO ()
main = showVersions >>= print
