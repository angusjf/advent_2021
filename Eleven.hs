import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import qualified Data.Map as M
import Data.Array hiding (index)
import Control.Concurrent
import System.IO.Unsafe

main = pt2 <$> readFile "input10.txt"

data Cell
  = Zero | One | Two | Three | Four
  | Five | Six | Seven | Eight | Nine 
  | Exploding | Exploded deriving (Eq)

toCell '0' = Zero 
toCell '1' = One 
toCell '2' = Two 
toCell '3' = Three 
toCell '4' = Four 
toCell '5' = Five 
toCell '6' = Six 
toCell '7' = Seven 
toCell '8' = Eight 
toCell '9' = Nine 

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

type Grid = Array (Int, Int) Cell

pt2 = fmap fst . find (all (== Zero) . snd) . zip [0..] . iterate step . fmap toCell . to2dArray . lines

step :: Grid -> Grid
step = cascade . fmap inc

cascade :: Grid -> Grid
cascade grid =
    case index (== Exploding) grid of
        Just pos ->
            cascade (explode pos grid)
        Nothing ->
            fmap (iff (== Exploded) Zero) grid

explode :: (Int, Int) -> Grid -> Grid
explode pos grid =
    foldr
        (\p acc -> at acc p inc)
        (at grid pos (const Exploded))
        (surrounding pos)

-- helpers

at xs p f =
    if (inRange (bounds xs) p) then
        xs // [(p, f (xs ! p))]
    else
        xs

index f = fmap fst . find (f . snd) . assocs

to2dArray xs = listArray ((0, 0), (length (head xs) - 1, length xs - 1)) $ concat xs

surrounding :: (Int, Int) -> [(Int, Int)]
surrounding (a, b) =
    [ (x, y) | x <- a ± 1, y <- b ± 1 ]

x ± d = [ x - d .. x + d ]

iff f a x | f x = a
          | otherwise = x
