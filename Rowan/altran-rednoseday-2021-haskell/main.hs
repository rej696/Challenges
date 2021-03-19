-- list to represent the keypad
keypad :: [[Int]]
keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- move function for right and down increments
moveRD :: Int -> Bool -> Int
moveRD coord partflag =
    if coord == 2
        then if partflag then coord - 1 else coord 
    else coord + 1

-- move function for left and up increments
moveLU :: Int -> Bool -> Int
moveLU coord partflag =
    if coord == 0
        then if partflag then coord + 1 else coord
    else coord - 1

-- Determines the final coordinate from a line
-- and start coordinate recursively
move :: [Int] -> String -> Bool -> [Int]
move coord line partflag =
    if line /= [] 
        then
            let command = head line
                rest = tail line
            in move (case command of
                'U' -> [head coord, moveLU (last coord) partflag]
                'D' -> [head coord, moveRD (last coord) partflag]
                'L' -> [moveLU (head coord) partflag, last coord]
                'R' -> [moveRD (head coord) partflag, last coord]
            ) rest partflag
    else coord

-- Builds output from start coordinate and list of strings (lines)
getResult :: [Int] -> [[Char]] -> Bool -> [Int]
getResult startcoord input partflag =
    let newcoord = move startcoord (head input) partflag
        number = (keypad !! last newcoord) !! head newcoord
        rest =
            if (tail input) == [] 
                then [] 
             else getResult newcoord (tail input) partflag
    in number : rest

-- read standard input and convert to list of strings, output result for each part
main = do
    input <- getContents
    putStr "Part1: "
    print (getResult [1, 1] (lines input) False)
    putStr "Part2: "
    print (getResult [1, 1] (lines input) True)
