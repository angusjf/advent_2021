import Data.List
import Data.Maybe
import Data.Char
import Data.Tuple
import Data.Array
import qualified Data.Map as M
import Debug.Trace

test = pt2 <$> readFile "test14.txt"
main = pt2 <$> readFile "input14.txt"

pt2 str = number $ (!! 40) $ iterate (grow rules) $ tally $ pairs $ start
    where (start, rules) = parse str

type Rule = ((Char, Char), Char)

parse :: String -> (String, [Rule])
parse str = (start, map toRule rules)
    where
        start:_:rules = lines str

pairs :: String -> [(Char, Char)]
pairs (x:y:rest) = (x, y) : pairs (y:rest)
pairs _ = []

unpairs :: [(Char, Char)] -> String
unpairs [(x, y)] = [x, y]
unpairs ((x, y):rest) = x : unpairs rest

toRule :: String -> Rule
toRule (a:b:' ':'-':'>':' ':c:[]) = ((a, b), c)

grow :: [Rule] -> M.Map (Char, Char) Int -> M.Map (Char, Char) Int
grow rules = M.foldlWithKey f M.empty
  where
        f acc (a, b) n =
            case lookup (a, b) rules of
                Just x ->
                    M.insertWith (+) (a, x) n $
                    M.insertWith (+) (x, b) n $
                    acc
                Nothing ->
                    M.insertWith (+) (a, b) n $
                    acc

number dict = (max_, min_)
    where
        max_ = M.findMax $ invert $ collapse dict
        min_ = M.findMin $ invert $ collapse dict

invert = M.fromList . map swap . M.toList

--collapse :: M.Map (Char, Char) Int -> String
collapse = M.foldlWithKey f M.empty
  where f acc (a, b) n = M.insertWith (+) a n acc

tally :: Ord x => [x] -> M.Map x Int
tally = foldl (\m k -> M.insertWith (+) k 1 m) M.empty
