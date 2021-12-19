import Data.Array
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Char
import Debug.Trace
import Data.Function (on)
import qualified Data.Set as S

demo = pt2 . parse . expand . map (map digitToInt) . lines <$> readFile "demo15.txt"
test = pt2 . parse . expand . map (map digitToInt) . lines <$> readFile "test15.txt"
--test = toString . parse . expand . map (map digitToInt) . lines <$> readFile "test15.txt"
main = pt2 . parse . expand . map (map digitToInt) . lines <$> readFile "input15.txt"

parse :: [[Int]] -> Array (Int, Int) Int
parse xs = listArray ((0, 0), (length (head xs) - 1, length xs - 1)) (concat xs)

expand :: [[Int]] -> [[Int]]
expand grid =
  let
    long = map (\row -> concat $ take 5 (iterate (map (+1)) row)) grid
    wrap x = if x > 9 then x - 9 else x
  in
    map (map wrap) $ concat $ take 5 $ iterate (map (map (+1))) long

toString :: Array (Int, Int) Int -> String
toString arr = unlines $ map (concat . intersperse "" . map show) $ chunk (w + 1) $ elems arr
    where (w, h) = snd $ bounds arr
    
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

pt2 :: Array (Int, Int) Int -> Int
pt2 arr = costs M.! (mx, my)
    where
        costs = dijkstra
                    ((!) arr)
                    (\(x, y) -> filter (inBounds (bounds arr)) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)])
                    (0, 0)
                    S.empty
                    S.empty
                    (M.fromList [((0, 0), 0)])
        (_, (mx, my)) = bounds arr

inBounds ((0, 0), (mx, my)) (x, y) = x >= 0 && x <= mx && y >= 0 && y <= my

dijkstra :: Ord a => (a -> Int) -> (a -> [a]) -> a -> S.Set a -> S.Set a -> M.Map a Int -> M.Map a Int
dijkstra edgeCost neighbours current visited unvisited costs =
    let
        currentCost :: Int
        currentCost = costs M.! current

        newCosts = M.fromList $ map (\e -> (e, currentCost + edgeCost e)) (neighbours current)
        updatedCosts = M.unionWith (\a b -> if a < b then a else b) newCosts costs

        updatedUnvisited = (S.fromList $ filter (`S.notMember` visited) (neighbours current)) `S.union` S.delete current unvisited
        next = minimumBy (compare `on` updatedCost) updatedUnvisited

        updatedCost e = updatedCosts M.! e
    in
            if S.null updatedUnvisited then
                updatedCosts
            else
                dijkstra
                    edgeCost
                    neighbours
                    next
                    (S.insert current visited)
                    updatedUnvisited
                    updatedCosts
