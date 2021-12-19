minTargetX = 277
maxTargetX = 318
targetX = [minTargetX .. maxTargetX]

minTargetY = -92
maxTargetY = -53
targetY = [minTargetY .. minTargetY]

main = getBestVelocity (1, 1)

getBestVelocity (xvel, yvel) =
    case simulate (0, 0) (xvel, yvel) of
        TooEarly -> getBestVelocity 


data SimulationResult
    = TooFar
    | Hit
    | TooClose

simulate (x, y) (xvel, yvel) =
    let
        newX = x + xvel
        newY = y + yvel
    in
        if newX > maxTargetX then
            TooFar
        else if newX < minTargetX then
            if newY > maxTargetY then
                simulate (newX, newY) (moveTo0 xvel, yvel - 1)
            else if newY < minTargetY then
                undefined
        else
            if newY > maxTargetY then
                TooClose
            else if newY < minTargetY then
                simulate (newX, newY) (moveTo0 xvel, yvel - 1)
            else
                Hit
    
moveTo0 :: Int -> Int
moveTo0 x =
    if x == 0
        then 0
        else if x > 0
            then x - 1
            else x + 1
