import Data.List

main = interact $ show . life . map (map b) . lines
  where b '0' = False
        b '1' = True

power :: [[Bool]] -> Int
power xs = fromBin bools * fromBin (map not bools)
  where bools = map mostFreq $ transpose xs

mostFreq :: [Bool] -> Bool
mostFreq xs = nF <= nT
  where [(nF, _), (nT, _)] = sortOn snd $ map (\xs -> (length xs, head xs)) . group . sort $ True:False:xs

fromBin :: [Bool] -> Int
fromBin xs = sum $ zipWith (\x -> (* 2 ^ x)) [0..] (reverse (map n xs))
  where n True = 1
        n False = 0

--life :: [[Bool]] -> Int
life xs = o2 * co2
  where o2 = fromBin $ f xs
        co2 = fromBin $ g $ xs

f [[]] = []
f xs = 
    let
        heads = map head xs
        mf = mostFreq heads
        new = map tail $ filter (\x -> head x == mf) xs
    in
        mf : f new

g [[]] = []
g [x] = x
g xs = 
    let
        heads = map head $ xs
        mf = not $ mostFreq heads
        new = map tail $ filter (\x -> head x == mf) xs
    in
        mf : g new
