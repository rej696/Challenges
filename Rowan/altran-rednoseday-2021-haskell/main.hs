-- list to represent the keypad
keypad :: [[Int]]
keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- move function for right and down increments
moveRD coord =
    if coord == 2
        then coord
    else coord + 1

-- move function for left and up increments
moveLU coord =
    if coord == 0
        then coord
    else coord - 1

-- Determines the final coordinate from a line
-- and start coordinate recursively
move :: [Int] -> String -> [Int]
move coord line =
    if line /= [] 
        then
            let command = head line
                rest = tail line
            in move (case command of
                'U' -> [head coord, moveLU (last coord)]
                'D' -> [head coord, moveRD (last coord)]
                'L' -> [moveLU (head coord), last coord]
                'R' -> [moveRD (head coord), last coord]
            ) rest
    else coord

-- Builds output from start coordinate and list of strings (lines)
getResult :: [Int] -> [[Char]] -> [Int]
getResult startcoord input =
    let newcoord = move startcoord (head input)
        number = (keypad !! last newcoord) !! head newcoord
        rest =
            if (tail input) == [] 
                then [] 
             else getResult newcoord (tail input)
    in number : rest

-- read standard input and convert to list of strings, output result
main = do
    input <- getContents
    print (getResult [1, 1] (lines input))
