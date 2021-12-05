import Data.List
import qualified Data.Map as M

test = pt2 <$> readFile "test5.txt"
main = pt2 <$> readFile "input5.txt"

pt2 = count (>= 2) . map snd . tally . concat . map points . map parse . lines

parse :: String -> ((Int, Int), (Int, Int))
parse = couple . map read . splitWithChars " ,->"

couple [a, b, c, d] = ((a, b), (c, d))

points ((x1, y1), (x2, y2)) = infinizip (,) [x1, x1 <= x2 ?? x1 .. x2]
                                            [y1, y1 <= y2 ?? y1 .. y2]

-- helpers >:)

infix 1 ??
True  ?? x = x + 1
False ?? x = x - 1

p1 ||| p2 = \x -> p1 x || p2 x

splitWithChars = splitWith . foldr1 (|||) . map (==)

splitWith f [] = []
splitWith f xs = a : splitWith f b
    where (a, b) = break f (dropWhile f xs)

tally = M.toList . foldl (\m k -> M.insertWith (+) k 1 m) M.empty

count f = length . filter f

infinizip f = go
  where
    go [x] [y]      = [f x y]
    go [x] (y:ys)    = f x y : go [x] ys
    go (x:xs) [y]    = f x y : go xs [y]
    go (x:xs) (y:ys) = f x y : go xs ys
