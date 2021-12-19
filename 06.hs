import Data.List
import Data.Maybe
import qualified Data.Map as M

test = pt2 <$> readFile "test6.txt"
main = pt2 <$> readFile "input6.txt"

pt2 = sum . M.elems . (!! 256) . iterate step . tally . parse . head . lines

parse = map read . splitWith (== ',')

step = resetTimers . addNew . M.mapKeys pred

resetTimers xs = M.delete (-1) $ add (M.findWithDefault 0 (-1) xs) 6 xs

addNew xs = add (M.findWithDefault 0 (-1) xs) 8 xs

add n = M.alter (pure . (+ n) . fromMaybe 0)

-- helpers

splitWith f [] = []
splitWith f xs = a : splitWith f b
    where (a, b) = break f (dropWhile f xs)

tally = foldl (\m k -> M.insertWith (+) k 1 m) M.empty
