import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import qualified Data.Map as M
import Data.Array

test = pt2 <$> readFile "test9.txt"
main = pt2 <$> readFile "input9.txt"

pt2 = product . take 3 . reverse . sort . basinSizes . toArray . map (map digitToInt) . lines

toArray :: [[a]] -> Array (Int, Int) a
toArray xs = listArray ((0, 0), (length xs - 1, length (head xs) - 1)) $ concat xs

lowPoints :: Array (Int, Int) Int -> [(Int, Int)]
lowPoints xs = map (fst . fst) $ filter (\((_, x), atSurrounding) -> all (> x) atSurrounding) $ withSurrounding xs

withSurrounding :: Array (Int, Int) Int -> [(((Int, Int), Int), [Int])]
withSurrounding xs = map (\((x, y), e) -> (((x, y), e), atSurrounding xs (x, y))) (assocs xs)

up    (x, y) = (x - 1, y)
down  (x, y) = (x + 1, y)
left  (x, y) = (x, y - 1)
right (x, y) = (x, y + 1)

atSurrounding :: Array (Int, Int) Int -> (Int, Int) -> [Int]
atSurrounding xs pos = catMaybes $ map ((!!!) xs) $ surrounding pos

xs !!! (x, y) = if inBounds xs (x, y) then Just $ xs ! (x, y) else Nothing

surrounding pos = [ up pos, down pos, left pos, right pos ]

basinSizes :: Array (Int, Int) Int -> [Int]
--basinSizes xs = [ (basinSize xs) ((lowPoints xs) !! 1) ]
basinSizes xs = map (basinSize xs) (lowPoints xs)

inBounds :: Array (Int, Int) x -> (Int, Int) -> Bool
inBounds xs (x, y) = x >= 0 && x <= width && y >= 0 && y <= height
  where
    ((0, 0), (width, height)) = bounds xs

basinSize :: Array (Int, Int) Int -> (Int, Int) -> Int
basinSize xs low = length $ nub $ helper xs [low] low

helper :: Array (Int, Int) Int -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
helper xs visited pos =
    let
        unvisited :: [(Int, Int)]
        unvisited = filter (inBounds xs) $ filter (not . (flip elem) visited) (surrounding pos)
        inBasin :: [(Int, Int)]
        inBasin = filter (\p -> (xs ! p) > (xs ! pos) && (xs ! p) /= 9) unvisited
        f :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
        f nextPos soFar = helper xs soFar nextPos
    in
        case inBasin of
            [] -> pos:visited
            _  -> foldr f (pos:visited) inBasin
        
