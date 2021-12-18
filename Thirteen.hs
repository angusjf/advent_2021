import Data.List
import Data.Maybe
import Data.Char
import Data.Array
import Data.Map.Internal.Debug
import Debug.Trace

test = (pt2 <$> readFile "test13.txt") >>= putStr
main = (pt2 <$> readFile "input13.txt") >>= putStr

pt2 = pretty . foldAll . parse . lines

data Cmd = X Int | Y Int deriving (Show)

type Man = [(Int, Int)]

parse lines =
    ( map toCell man
    , map toCmd cmds
    )
    where
        (man, _:cmds) = break (== "") lines

toCell :: String -> (Int, Int)
toCell str = (read x, read y)
  where
    (x, _:y) = break (== ',') str

toCmd :: String -> Cmd
toCmd ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':n) = X (read n)
toCmd ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=':n) = Y (read n)

foldAll (man, cmds) = foldr fold man cmds

fold :: Cmd -> Man -> Man
--fold cmd man | trace ("cmd " ++ show cmd ++ " " ++ show man) False = undefined
fold cmd man =
    let 
        (flipped, fn1, fn2) = case cmd of
            X x -> (flippedX, (< x) . fst, (> x) . fst)
            Y y -> (flippedY, (< y) . snd, (> y) . snd)
        (base, overlay) = cut fn1 fn2 man
    in
        base ++ (flipped overlay)

cut :: ((Int, Int) -> Bool) -> ((Int, Int) -> Bool) -> Man -> (Man, Man)
cut fn1 fn2 map = (filter fn1 map, filter fn2 map)

flippedX :: Man -> Man
flippedX man = map (\(x, y) -> (maxX - x, y)) man
  where maxX = maximum $ map fst man

flippedY :: Man -> Man
flippedY man = map (\(x, y) -> (x, maxY - y)) man
  where maxY = maximum $ map snd man

pretty :: Man -> String
pretty = show
--pretty = toString . to2d . sort

toString :: [(Int, [Bool])] -> String
toString [] = ""
toString ((n, x):xs) = show n ++ "\t" ++ ": " ++ (map (\c -> if c then '#' else '.') x) ++ "\n" ++ toString xs

to2d :: Man -> [(Int, [Bool])]
to2d [] = []
to2d l = helper 0 l
  where
    helper n [] = []
    helper n list =
        let 
            x = filter (\(x, y) -> y == n) list
            xs = filter (\(x, y) -> y > n) list
        in
            ( n
            ,   map snd $
                    sortOn fst $
                    unionBy (\(a, _) (b, _) -> a == b)
                        (map (\a -> (a,  True)) (map fst x))
                        (map (\a -> (a, False)) [0..maximum (map fst x)])
            ) : helper (n + 1) xs

