module Aoc18.Day3 where

import Prelude hiding (filter, readFile)

import Data.List (foldl')
import Data.Maybe (listToMaybe, mapMaybe)
import Data.IntMap.Strict (IntMap, size, filter, empty, insertWith)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text as T

data Claim = Claim { _cid :: Int
                   , _origin :: (Int, Int)
                   , _size :: (Int, Int)
                   } deriving (Eq, Show)

extreme :: Claim -> (Int, Int)
extreme (Claim _ (a,b) (c, d)) = (a+c-1, b+d-1)

testInput :: Text
testInput = "#1 @ 1,3: 4x4\n\
            \#2 @ 3,1: 4x4\n\
            \#3 @ 5,5: 2x2"

main :: IO ()
main = print . part2 $ testInput

withInput :: (Text -> IO ()) -> IO ()
withInput = (readFile "data/day3.txt" >>=)

part1 :: Text -> Int
part1 = size . filter (> 1) . foldl' insert empty . parse

part2 :: Text -> Maybe Int
part2 = fmap _cid . listToMaybe . (\cs -> mapMaybe (find' cs) cs) . parse
  where
    find' xs x = if any (isOverlap x) xs then Nothing else Just x

pointToKey :: (Int, Int) -> Int
pointToKey (a, b) = a * 1000 + b -- max 1k

insert :: IntMap Int -> Claim -> IntMap Int
insert m (Claim _ (x, y) (w, l)) =
  foldl' (\m p -> insertWith (+) p 1 m) m [pointToKey (x', y') | x' <- [x..(x+w-1)]
                                                               , y' <- [y..(y+l-1)]]

isOverlap :: Claim -> Claim -> Bool
isOverlap a@(Claim _ (ax,ay) _) b@(Claim _ (bx,by) _)
  | a == b = False
  | ax <= bv && av >= bx = ay <= bw && aw >= by
  | otherwise = False
  where
    (av, aw) = extreme a
    (bv, bw) = extreme b

parse :: Text -> [Claim]
parse = fromEither . traverse (parseOnly claimP) . T.lines
  where
    fromEither (Right x) = x
    fromEither (Left m) = error m

idP :: Parser Int
idP = do char '#'
         decimal

origP :: Parser (Int, Int)
origP = do x <- decimal
           char ','
           y <- decimal
           pure $ (x, y)

sizeP :: Parser (Int, Int)
sizeP = do x <- decimal
           char 'x'
           y <- decimal
           pure $ (x, y)

claimP :: Parser Claim
claimP = do i <- idP
            ws; char '@'; ws
            o <- origP
            char ':'; ws
            s <- sizeP
            pure $ Claim i o s
              where
                ws = char ' '
