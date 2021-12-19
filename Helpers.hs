module Helpers where

import Data.List
import qualified Data.Map as M

p1 ||| p2 = \x -> p1 x || p2 x

splitWithSet :: Eq a => [a] -> [a] -> [[a]]
splitWithSet = splitWith . foldr1 (|||) . map (==)

splitWith f [] = []
splitWith f xs = a : splitWith f b
    where (a, b) = break f (dropWhile f xs)

takeEvery n [] = []
takeEvery n xs = take n xs : takeEvery n (drop n xs)

tally :: Ord x => [x] -> [(x, Int)]
tally = M.toList . foldl (\m k -> M.insertWith (+) k 1 m) M.empty

count f = length . filter f

big = filter (`notElem` small) [1..10]

small = filter (`notElem` big) [1..10]
