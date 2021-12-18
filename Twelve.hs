import Data.List
import Data.Maybe
import Data.Char

main = pt2 <$> readFile "input12.txt"

pt2 = length . paths ["start"] . map parse . lines

parse line = (from, to)
    where
        (from, _:to) = break (== '-') line

paths path graph =
    case head path of
        "end" ->
            [reverse path]
        current ->
            let
                nextNodes = filter (visitable path) (links current graph)
            in
                concatMap (\next -> paths (next:path) graph) nextNodes

links node = mapMaybe $
    \(a, b) ->
        if      a == node then Just b
        else if b == node then Just a
        else                   Nothing
        

visitable _ "start" = False
visitable path cave = big cave || cave `notElem` path || smalls == nub smalls
    where
        smalls = filter (not . big) path

big = isUpper . head
