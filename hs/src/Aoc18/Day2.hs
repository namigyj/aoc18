{-# language LambdaCase #-}
module Aoc18.Day2 where

import Data.Char (ord)
import Data.Vector (Vector, fromList)
import Test.Hspec (hspec, describe, it, shouldBe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

tests :: IO ()
tests = hspec $ do
  describe "" $ do
    it "part1" $ do
      part1 testinput `shouldBe` 12
    it "part2" $ do
      part2 [ "abcde"
            , "fghij"
            , "klmno"
            , "pqrst"
            , "fguij"
            , "axcye"
            , "wvxyz"
            ] `shouldBe` "fgij"

testinput :: [String]
testinput = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

withInput :: Show a => ([String] -> a) -> FilePath -> IO a
withInput f path = readFile path >>= pure . f . lines

part1 :: [String] -> Int
part1 = uncurry (*)
        . foldr1 tupsum
        . fmap (countOcc . updateOcc)
  where
    tupsum (a,b) (c,d) = (a+c, b+d)

part2 :: [String] -> String
part2 ids = head $ do x <- ids  -- in head we trust :pray:
                      y <- ids
                      case f x y of
                        Just x -> pure x
                        _ -> []
                      where
                        f a b | singleDiff a b = Just $ positionalIntersect a b
                              | otherwise = Nothing

singleDiff :: Eq a => [a] -> [a] -> Bool
singleDiff (a:as) (b:bs) | a /= b = as == bs
                         | otherwise = singleDiff as bs
singleDiff _ _ = False

positionalIntersect :: Eq a => [a] -> [a] -> [a]
positionalIntersect as [] = as
positionalIntersect [] bs = bs
positionalIntersect (a:as) (b:bs) | a /= b = positionalIntersect as bs
                                  | otherwise = a:positionalIntersect as bs

letters :: Vector Int
letters = V.replicate 26 0

tr :: Char -> Int
tr = flip (-) 97 . ord

countOcc :: Vector Int -> (Int, Int)
countOcc vec = foldr f (0, 0) vec
  where
    f x (a, b) | x == 2 = (1, b)
               | x == 3 = (a, 1)
               | otherwise = (a, b)

updateOcc :: String -> Vector Int
updateOcc str = foldl inschar letters str
  where
    inschar counts c = V.modify (\v -> M.modify v (+1) (tr c)) counts
