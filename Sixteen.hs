import Control.Applicative
import Data.Maybe
import Data.List
import Debug.Trace

hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

main = pt2 . head . lines  <$> readFile "input16.txt"

pt2 =  value . fst . parse . concatMap hexToBin

data Packet
  = Packet Int Int PacketData
  deriving (Show)

data PacketData
  = Literal Int
  | Operator [Packet]
  deriving (Show)

parse :: String -> (Packet, String)
parse (v1:v2:v3:t1:t2:t3:more) =
    case (btoi [v1, v2, v3], btoi [t1, t2, t3]) of
        (v, 4) ->
            let
                (data_, moremore) = literal more
            in
                (Packet v 4 data_, moremore)
        (v, not4) ->
            let
                (data_, moremore) = operator more
            in
                (Packet v not4 data_, moremore)

parse str = error str

literal :: String -> (PacketData, String)
literal str =
    let
        (a, b:c) = span ((== '1') . head) $ every 5 str
        whole = a ++ [b]
        n = btoi $ concat $ map tail whole
    in
        ( Literal n
        , concat c
        )

operator :: String -> (PacketData, String)
operator (i:rest) =
    let 
        (packets, more) =
            case i of
                '0' ->
                    let
                        len = btoi $ take 15 rest
                    in
                        ( parseAll (take len $ drop 15 rest)
                        , drop (len + 15) rest
                        )
                '1' ->
                    let
                        nSubPackets = btoi $ take 11 rest
                    in
                        parseN nSubPackets $ drop 11 $ rest
    in
        (Operator packets, more)

parseAll :: String -> [Packet]
parseAll "" = []
parseAll str =
    let 
        (packet, more) = parse str
        packets = parseAll more
    in
        packet:packets

parseN :: Int -> String -> ([Packet], String)
parseN 0 str = ([], str)
parseN n str =
    let
        (packet, more) = parse str 
        (packets, moremore) = parseN (n - 1) more
    in
        (packet:packets, moremore)

btoi :: String -> Int
btoi = foldr (+) 0 . zipWith (\n x -> (bin x) * (2 ^ n)) [0..] . reverse

bin '0' = 0
bin '1' = 1

value :: Packet -> Int
value (Packet _ id data_) =
    case data_ of
        Literal n -> n
        Operator packets ->
            ( case id of
                0 -> sum
                1 -> product
                2 -> minimum
                3 -> maximum
                5 -> \[first, second] -> if first >  second then 1 else 0
                6 -> \[first, second] -> if first <  second then 1 else 0
                7 -> \[first, second] -> if first == second then 1 else 0
            ) (map value packets)
        

every n xs =
    case (take n xs, drop n xs) of
        ([], _) -> []
        (e, []) -> [e]
        (e, z) -> e : every n z
        
