module Aoc18.Day1 where

import Test.Hspec
import Data.Set (member, insert, empty)

tests :: IO ()
tests = hspec $ do
  describe "part1 tests" $ do
    it "" $ do
      part1 ["+1","+1","+1"] `shouldBe` 3
    it "" $ do
      part1 ["+1","+1","-2"] `shouldBe` 0
    it "" $ do
      part1 ["-1","-2","-3"] `shouldBe` (-6)
  describe "part2 tests" $ do
    it "" $ do
      part2 ["+1", "-1"] `shouldBe` 0
    it "" $ do
      part2 ["+3","+3","+4","-2","-4"] `shouldBe` 10
    it "" $ do
      part2 ["-6","+3","+8","+5","-6"] `shouldBe` 5
    it "" $ do
      part2 ["+7","+7","-2","-7","-4"] `shouldBe` 14


withInput :: ([String] -> Integer) -> FilePath -> IO Integer
withInput f path = f . lines <$> readFile path

part1 :: [String] -> Integer
part1 = sum . map parse

part2 :: [String] -> Integer
part2 = ffreq . map parse

parse :: String -> Integer
parse nums@(x:xs) | x == '+' = read xs
                  | otherwise = read nums
parse xs = error $ "invalid input: " ++ xs

ffreq :: [Integer] -> Integer
ffreq changes = go empty 0 (cycle changes)
  where
    go set freq (c:cs) | member freq set = freq
                       | otherwise = go (insert freq set) (freq + c) cs
