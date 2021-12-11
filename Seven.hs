import Data.List
import Data.Maybe
import qualified Data.Map as M

test = pt2 <$> readFile "test7.txt"
main = pt2 <$> readFile "input7.txt"

pt2 = getFuel . map read . splitWith (== ',')

getFuel xs = minimum $ map ((flip alignAt) xs) [(minimum xs)..(maximum xs)]

alignAt n = sum . map (fuelToMoveFrom n)

fuelToMoveFrom from to = (n * n + n) `div` 2
  where n = abs (from - to)

-- helpers

splitWith f [] = []
splitWith f xs = a : splitWith f b
    where (a, b) = break f (dropWhile f xs)
