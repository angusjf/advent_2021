import Data.List
import Data.Maybe

type Card = [[Maybe Int]]

test = uncurry bingo . parse . lines <$> readFile "test4.txt"
main = uncurry bingo . parse . lines <$> readFile "input4.txt"

toCard :: [String] -> Card
toCard = map (map (Just . read)) . map (splitWith (== ' ')) . init

parse :: [String] -> ([Int], [Card])
parse (numbers:_:rest) = (map read splitNumbers, map toCard cards)
    where cards = chunksOf 6 rest
          splitNumbers = splitWith (== ',') numbers

bingo :: [Int] -> [Card] -> Int
bingo (called:rest) cards =
    case filter isWinner strikedCards of
        winner:_ ->
            case strikedCards of
                [lastCard] -> called * sumCard lastCard
                _ -> bingo rest (filter (not . isWinner) strikedCards)
        [] -> bingo rest strikedCards
    where strikedCards = map (strike called) cards

isWinner :: Card -> Bool
isWinner card = any (all id) bools || any (all id) (transpose bools)
    where bools = map (map isNothing) card

strike :: Int -> Card -> Card
strike called = map (map check)
    where check Nothing = Nothing
          check (Just n) = if n == called then Nothing else Just n

sumCard :: Card -> Int
sumCard = sum . concatMap catMaybes

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs = a : splitWith f b
    where (a, b) = break f (dropWhile f xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
