import Data.List.Split (splitOn)

data Dir = North | East | South | West deriving (Show)

rotate_lr :: Dir -> LR -> Dir
rotate_lr dir L = rotate_left dir
rotate_lr dir R = rotate_right dir

rotate_left :: Dir -> Dir
rotate_left North = West
rotate_left West  = South
rotate_left South = East
rotate_left East  = North

rotate_right :: Dir -> Dir
rotate_right dir = rotate_left $ rotate_left $ rotate_left dir

go :: Dir -> (Int, Int) -> Int -> (Int, Int)
go North (x, y) steps = (x         , y + steps) 
go East  (x, y) steps = (x + steps , y        ) 
go South (x, y) steps = (x         , y - steps) 
go West  (x, y) steps = (x - steps , y        ) 

parse_input :: String -> [Instruction]
parse_input input = map parse_instruction $ splitOn ", " input

data Instruction = Instruction LR Int deriving (Show)

parse_instruction :: String -> Instruction
parse_instruction (lr : steps) =
    let
        lr'    = parse_left_right lr
        steps' = read steps
    in
        Instruction lr' steps'

data LR = L | R deriving (Show)

parse_left_right :: Char -> LR
parse_left_right 'L' = L
parse_left_right 'R' = R

problem :: (Int, Int) -> Dir -> [Instruction] -> Int
problem (x, y) _ [] = (abs x) + (abs y)
problem pos dir (Instruction lr steps : rest) =
    let
        dir' = rotate_lr dir lr
        pos' = go dir' pos steps
    in
        problem pos' dir' rest

main = do
    input <- readFile "input.txt"
    let solution = problem (0, 0) North (parse_input input)
    putStrLn $ show solution
