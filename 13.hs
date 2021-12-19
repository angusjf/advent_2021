import qualified Data.Set as S
import Data.Bifunctor

main = pt2 <$> readFile "input13.txt" >>= putStr

pt2 = pretty . uncurry (foldr fold) . parse . lines

data Cmd = X Int | Y Int

parse lines = (S.fromList $ map toCell man, reverse $ map toCmd cmds)
  where (man, _:cmds) = break null lines

toCell str = (read x, read y)
  where (x, _:y) = break (== ',') str

toCmd (_:_:_:_:_:_:_:_:_:_:_:'x':_:n) = X (read n)
toCmd (_:_:_:_:_:_:_:_:_:_:_:'y':_:n) = Y (read n)

chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

fold (X x) = S.filter ((< x) . fst) >|< flipX x . S.filter ((> x) . fst)
fold (Y y) = S.filter ((< y) . snd) >|< flipY y . S.filter ((> y) . snd)

infixr 0 >|<
a >|< b = \man -> S.union (a man) (b man)

flipX line = S.map $ first  ((-) (line * 2))
flipY line = S.map $ second ((-) (line * 2))

toChar True  = '#'
toChar False = '.'

pretty man = unlines $ map (map toChar) bools
    where
        maxX = S.findMax $ S.map fst man
        maxY = S.findMax $ S.map snd man
        bools = chunk (maxX + 1) [ (x, y) `S.member` man
                                 | y <- [0..maxY], x <- [0..maxX]
                                 ]
