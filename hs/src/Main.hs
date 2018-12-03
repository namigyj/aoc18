module Main where

import Aoc18.Day1 (part1 , part2)
import System.IO

main :: IO ()
main = do input <- lines <$> hGetContents stdin
          print . ("p1: " ++) . show . part1 $ input
          print . ("p2: " ++) . show . part2 $ input

