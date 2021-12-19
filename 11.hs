import Data.List
import Data.Char
import Data.Array hiding (index)

-- main is the entry point 
-- we have to use <$> to apply the pt2 function
-- on the contents of the file as haskell keeps IO related code
-- separate from pure functions
main :: IO ()
main = pt2 <$> readFile "input10.txt"

-- This function 'solves the problem' - taking a String (our input grid)
-- and returning the line number. This has to be a Maybe Int as it's hard to
-- convince haskell this program will find an all 0 grid at some point
-- iterate creates and infinite list where we run the simulation
-- zip [0..] pairs every step with a number
-- then find looks for the step where all cells are zero
-- and fmap fst gets the number (first element of the tuple)
pt2 :: String -> Maybe Int
pt2 = fmap fst . find (all (== 0) . snd) . zip [0..] . iterate (cascade . fmap inc) . parse

-- I'm defining a type alias here do I don't repeat myself but
-- Array (Int, Int) Int means a 2d array of ints
type Grid = Array (Int, Int) Int

-- parse is fairly clear... split the string into lines, put it in a 2d array
-- then turn all the characters into ints
parse :: String -> Grid
parse = fmap digitToInt . to2dArray . lines

-- cascade is the core of the program
-- it takes a grid to a grid, basically stepping the simulation. we check if there are no 11s
-- (I'm using 11 to mean exploded cells and 10 to mean exploding cells)
cascade :: Grid -> Grid
cascade grid = maybe (fmap (iff (== 11) 0) grid) (cascade . explode grid) (index (== 10) grid)

explode grid pos = foldr (at inc) (at (const 11) pos grid) (surrounding pos)

inc 10 = 10
inc 11 = 11
inc x = x + 1

-- helpers

at f p xs = if inRange (bounds xs) p then xs // [(p, f (xs ! p))] else xs

index f = fmap fst . find (f . snd) . assocs

to2dArray xs = listArray ((0, 0), (length (head xs) - 1, length xs - 1)) $ concat xs

surrounding (a, b) = [ (x, y) | x <- a ± 1, y <- b ± 1 ]

x ± d = [ x - d .. x + d ]

iff f a x | f x = a
          | otherwise = x
