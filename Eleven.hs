import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import qualified Data.Map as M
import Data.Array
import Control.Concurrent
import System.IO.Unsafe

c = 100

demo = pt2 <$> readFile "demo10.txt"
test = pt2 <$> readFile "test10.txt"
main = (show . pt2 <$> readFile "input10.txt") >>= putStrLn

data Cell
  = Zero 
  | One 
  | Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | Exploding 
  | Exploded 
    deriving (Eq)

intToCell 0 = Zero 
intToCell 1 = One 
intToCell 2 = Two 
intToCell 3 = Three 
intToCell 4 = Four 
intToCell 5 = Five 
intToCell 6 = Six 
intToCell 7 = Seven 
intToCell 8 = Eight 
intToCell 9 = Nine 

inc Zero      = One 
inc One       = Two 
inc Two       = Three 
inc Three     = Four 
inc Four      = Five 
inc Five      = Six 
inc Six       = Seven 
inc Seven     = Eight 
inc Eight     = Nine 
inc Nine      = Exploding
inc Exploding = Exploding
inc Exploded  = Exploded

toChar Zero  = '◌'
toChar One   = '◜'
toChar Two   = '◝'
toChar Three = '◠' 
toChar Four  = '◯'
toChar Five  = '◷'
toChar Six   = '◔'
toChar Seven = '◑' 
toChar Eight = '◕' 
toChar Nine  = '●'
toChar Exploding  = '?'
toChar Exploded  = '◌'

toString :: [[Cell]] -> String
toString = concatMap (\row -> (intersperse ' ' (map toChar row)) ++ "\n")

pt2 = getIndexWhere allFlashing . iterate step . map (map (intToCell . digitToInt)) . lines

allFlashing xs = all (== Zero) $ concat xs

getIndexWhere f grids = helper f 0 grids

helper f n (grid:more) = if f grid then n else helper f (n + 1) more

step :: [[Cell]] -> [[Cell]]
step grid = cascade $ map (map inc) grid

cascade :: [[Cell]] -> [[Cell]]
cascade grid =
    if any (== Exploding) (concat grid) then
        cascade (cascadeFirst grid)
    else
        log_ $ map (map (\x -> if x == Exploded then Zero else x)) grid

cascadeFirst :: [[Cell]] -> [[Cell]]
cascadeFirst grid =
    let
        pos = coordsOfFirst (== Exploding) grid
        incAt = surrounding pos
        flashed = updateAt grid pos (const Exploded)
        next = foldr (\p myGrid -> updateAt myGrid p inc) flashed incAt
    in
        next

log_ x = thenSleep (trace (toString x ++ "\n") x)

thenSleep a = const a $! unsafePerformIO (threadDelay 70000)

surrounding :: (Int, Int) -> [(Int, Int)]
surrounding (a, b) =
    [ (x, y) | x <- [a - 1 .. a + 1]
             , y <- [b - 1 .. b + 1] 
    ]

withCoords :: [[a]] -> [[((Int, Int), a)]]
withCoords xs =
    zipWith
        (\ns y -> map (\(x, v) -> ((x, y), v)) ns)
        (map indexed xs)
        [0..]

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

coordsOfFirst :: (a -> Bool) -> [[a]] -> (Int, Int)
coordsOfFirst f grid = head $ map fst $ filter (\(_, x) -> f x) $ concat $ withCoords grid

updateAt :: [[a]] -> (Int, Int) -> (a -> a) -> [[a]]
updateAt grid pos f = map (map update) $ withCoords grid
    where update (p, x) = if pos == p then f x else x

