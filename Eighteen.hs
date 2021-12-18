import Data.Char

test = pt2 <$> readFile "test18.txt"
main = pt2 <$> readFile "input18.txt"

pt2 = findSmallestSum . map (fst . parse) . lines

data SFNum = N Int | P SFNum SFNum deriving (Eq)

parse :: String -> (SFNum, String)
parse ('[':more) =
    let (a, ',':moremore) = parse more
        (b, ']':moremoremore) = parse moremore
    in  (P a b, moremoremore)
parse (n:more) | isDigit n = (N $ digitToInt n, more)

sfAdd :: SFNum -> SFNum -> SFNum
sfAdd a b = reduce (P a b)

reduce :: SFNum -> SFNum
reduce x =
    case ex 4 x of
        Just (new, _) -> reduce new
        Nothing -> maybe x reduce (sp x)

ex :: Int -> SFNum -> Maybe (SFNum, (Int, Int))
ex _ (N n) = Nothing
ex 0 (P (N x) (N y)) = Just (N 0, (x, y))
ex d (P l r) | d > 0 =
    case ex (d - 1) l of
        Just (newL, (x, y)) -> Just (P newL (addToL r y), (x, 0))
        Nothing ->
            case ex (d - 1) r of
                Just (newR, (x, y)) -> Just (P (addToR l x) newR, (0, y))
                Nothing -> Nothing

sp :: SFNum -> Maybe SFNum
sp (N n) =
    if n >= 10 then
        let x = fromIntegral n / 2
        in Just $ P (N $ floor x) (N $ ceiling x)
    else
        Nothing
sp (P a b) =
    case sp a of
        Just new -> Just $ P new b
        Nothing ->
            case sp b of
                Just new -> Just $ P a new
                Nothing -> Nothing

magnitude :: SFNum -> Int
magnitude (N n) = n
magnitude (P a b) = 3 * magnitude a + 2 * magnitude b

addToL :: SFNum -> Int -> SFNum
addToL (N n) x = N $ x + n
addToL (P a b) x = P (addToL a x) b

addToR :: SFNum -> Int -> SFNum
addToR (N n) x = N $ x + n
addToR (P a b) x = P a (addToR b x)

pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [ (a, b) | (i, a) <- zip [1..] xs, (j, b) <- zip [1..] xs, i /= j ]

findSmallestSum nums =
    maximum $
    map (magnitude . uncurry sfAdd) $
    pairs nums
