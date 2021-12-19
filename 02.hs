main = interact $ show . two . map cmd . lines

data Command = Forward Int | Up Int | Down Int

cmd ('f':'o':'r':'w':'a':'r':d:' ':n) = Forward $ read n
cmd ('d':'o':'w':'n'          :' ':n) = Down    $ read n
cmd ('u':'p'                  :' ':n) = Up      $ read n

two xs = h * d
    where (_, h, d) = foldl f (0, 0, 0) xs
          f (aim, x, y) cmd = case cmd of 
                                Forward n -> (aim    , x + n, y + aim * n)
                                Up n      -> (aim - n, x    , y          )
                                Down n    -> (aim + n, x    , y          )

one xs = h * d
    where (h, d) = foldl f (0, 0) xs
          f (x, y) cmd = case cmd of 
                                Forward n -> (x + n, y)
                                Up n      -> (x, y - n)
                                Down n    -> (x, y + n)
