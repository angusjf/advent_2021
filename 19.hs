import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Map as M

test = pt2 <$> readFile "test19.txt" >>= putStrLn . unlines
main = pt2 <$> readFile "input19.txt" >>= putStrLn . unlines

pt2 = numBeacons . orient . parse

type BeaconId = Int
type Beacon = (Int, Int, Int)
type RotationId = Integer
data Scanner = S Int [Beacon] deriving (Show, Eq)
data ScannerWithData = SData BeaconId [Beacon] BeaconId RotationId (Int, Int, Int) deriving (Show, Eq)

parse :: String -> [Scanner]
parse = map toScanner . splitWith (== "") . lines

toScanner :: [String] -> Scanner
toScanner (h:t) = S (read id) (map (read . bracketize) t)
  where bracketize x = '(':x ++ ")"
        [_, _, id, _] = splitWith (== ' ') h

splitWith f [] = []
splitWith f xs = a : splitWith f b
    where (a, b) = break f (dropWhile f xs)

beacons (S _ bs) = bs

beaconsD (SData _ bs _ _ _) = bs

old :: ScannerWithData -> Scanner
old (SData id bs _ _ _) = S id bs

orient :: [Scanner] -> [ScannerWithData]
orient scanners = helper [] scanners

helper db [] = db
helper db (x:xs) =
    let
        S index bs = x
        --validCandidate :: (RotationId, Scanner) -> (RotationId,     (  ((Int, Int, Int), BeaconId)  ,  Scanner )    )
        validCandidate (rid, c) = ((,) rid) <$> matches12 c
        valid =
            case firstThat validCandidate (candidates x) of
                Just (rid, ((off, relTo), S _ c)) ->
                    trace (show index) $ SData index c relTo rid off
                Nothing ->
                    error $ "no valid orientation for scanner " ++ show index
        matches12 c = (, c) <$> firstThat (shares12 c) (if db /= [] then (map old db) else xs)
    in
        helper (valid:db) xs

-- find a mapping if it exists
-- such that 12 points from as == 12 points from bs
shares12 :: Scanner -> Scanner -> Maybe ((Int, Int, Int), BeaconId)
shares12 (S aid as) (S bid bs) =
    let
        mappings = concatMap (\b -> map (\a -> sub a b) as) bs
    in
        (, bid) <$> find (isValidMapping as bs) mappings

isValidMapping :: [Beacon] -> [Beacon] -> (Int, Int, Int) -> Bool
isValidMapping as bs mapping = length (intersect (map (add mapping) as) bs) >= 12

add (a, b, c) (d, e, f) = (a + d, b + e, c + f)
sub (a, b, c) (d, e, f) = (a - d, b - e, c - f)

firstThat :: Eq b => (a -> Maybe b) -> [a] -> Maybe b
firstThat f = listToMaybe . catMaybes . map f

candidates :: Scanner -> [(RotationId, Scanner)]
candidates (S id beacons) = 
    map
        (\(i, f) -> (i, (S id (map f beacons))))
        $ zip [0..]
        [ (\(x, y, z) -> ( x,  y,  z))
        , (\(x, y, z) -> ( x,  y, -z))
        , (\(x, y, z) -> ( x, -y,  z))
        , (\(x, y, z) -> ( x, -y, -z))
        , (\(x, y, z) -> (-x,  y,  z))
        , (\(x, y, z) -> (-x,  y, -z))
        , (\(x, y, z) -> (-x, -y,  z))
        , (\(x, y, z) -> (-x, -y, -z))

        , (\(x, y, z) -> ( y,  x,  z))
        , (\(x, y, z) -> ( y,  x, -z))
        , (\(x, y, z) -> (-y,  x,  z))
        , (\(x, y, z) -> (-y,  x, -z))
        , (\(x, y, z) -> ( y, -x,  z))
        , (\(x, y, z) -> ( y, -x, -z))
        , (\(x, y, z) -> (-y, -x,  z))
        , (\(x, y, z) -> (-y, -x, -z))

        , (\(x, y, z) -> ( z,  y,  x))
        , (\(x, y, z) -> (-z,  y,  x))
        , (\(x, y, z) -> ( z, -y,  x))
        , (\(x, y, z) -> (-z, -y,  x))
        , (\(x, y, z) -> ( z,  y, -x))
        , (\(x, y, z) -> (-z,  y, -x))
        , (\(x, y, z) -> ( z, -y, -x))
        , (\(x, y, z) -> (-z, -y, -x))

        , (\(x, y, z) -> ( x,  z,  y))
        , (\(x, y, z) -> ( x, -z,  y))
        , (\(x, y, z) -> ( x,  z, -y))
        , (\(x, y, z) -> ( x, -z, -y))
        , (\(x, y, z) -> (-x,  z,  y))
        , (\(x, y, z) -> (-x, -z,  y))
        , (\(x, y, z) -> (-x,  z, -y))
        , (\(x, y, z) -> (-x, -z, -y))

        , (\(x, y, z) -> ( y,  z,  x))
        , (\(x, y, z) -> ( y, -z,  x))
        , (\(x, y, z) -> (-y,  z,  x))
        , (\(x, y, z) -> (-y, -z,  x))
        , (\(x, y, z) -> ( y,  z, -x))
        , (\(x, y, z) -> ( y, -z, -x))
        , (\(x, y, z) -> (-y,  z, -x))
        , (\(x, y, z) -> (-y, -z, -x))

        , (\(x, y, z) -> ( z,  x,  y))
        , (\(x, y, z) -> (-z,  x,  y))
        , (\(x, y, z) -> ( z,  x, -y))
        , (\(x, y, z) -> (-z,  x, -y))
        , (\(x, y, z) -> ( z, -x,  y))
        , (\(x, y, z) -> (-z, -x,  y))
        , (\(x, y, z) -> ( z, -x, -y))
        , (\(x, y, z) -> (-z, -x, -y))
        ]

numBeacons = map f
    where 
        f (SData id beaconList relTo rId trans) = "id: " ++ show id ++ "\t" ++ "relTo: " ++ show relTo ++ "\t" ++ "rId: " ++ show rId ++ "\t" ++ "trans: " ++ show trans ++ "\t"
