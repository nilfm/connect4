import Text.Read
import System.Random
import Debug.Trace

-- Random number generation

-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt :: Int -> Int -> IO Int
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

randChoice :: [a] -> IO a
randChoice options = do
    index <- randInt 0 ((length options) - 1)
    return (options !! index)

-- Type aliases
type Board = Int -> Int -> Piece

-- Data types
data Game = Game Board Int Int
data Piece = Cross | Circle | None
    deriving (Eq)
data Strategy = Random | Greedy | Smart
    deriving (Show, Eq)
data Turn = Player | Computer
    deriving (Show, Eq)
data Outcome = Win | Lose | Tie
    deriving (Show, Eq)

-- Show instances
instance Show Piece where
    show Cross = "X"
    show Circle = "O"
    show None = "."

instance Show Game where
    show (Game board w h) = concat (map (showLine board w) [h, h-1..1])

-- Auxiliary functions for Show instances
showLine :: Board -> Int -> Int -> String
showLine board w row = show (board row 1) ++ concat [" " ++ show (board row col) | col <- [2..w]] ++ "\n"


-- Creators
createBoard :: Int -> Int -> Board
createBoard w h row col = None

createGame :: Int -> Int -> Game
createGame w h = Game (createBoard w h) w h


-- Functions to get piece at position

-- Pre: 1 <= row <= height, 1 <= col <= width
getPiece :: Board -> Int -> Int -> Piece
getPiece board row col = board row col

-- Auxiliary functions to get piece at position

-- (Empty)


-- Functions to set piece at position

-- Pre: the given column has space left over at the top
setPiece :: Turn -> Game -> Int -> Game
setPiece turn (Game board w h) col = Game newBoard w h
    where 
        row = firstEmptyRow board col
        newBoard a b
            | a == row && b == col = getTurnPiece turn
            | otherwise = board a b


-- Auxiliary functions to set piece at position
firstEmptyRow :: Board -> Int -> Int
firstEmptyRow board col = head $ dropWhile isFull [1..]
    where 
        isFull row = (getPiece board row col) /= None

getTurnPiece :: Turn -> Piece
getTurnPiece Player = Circle
getTurnPiece Computer = Cross

isValidColumn :: Game -> Int -> Bool
isValidColumn (Game board w h) col = (firstEmptyRow board col) <= h

-- Reads an int from stdin until it receives one that fulfills the condition
readInt :: String -> (Int -> Bool) -> IO Int
readInt s cond = do
    putStrLn s
    line <- getLine
    putStrLn ""
    let num = readMaybe line :: Maybe Int
    case num of
        Nothing -> do
            putStrLn "That is not an integer number"
            readInt s cond
        (Just n) -> if cond n 
                    then return n 
                    else do
                        putStrLn "This number is invalid"
                        readInt s cond

-- Returns the Strategy associated with a number 
getStrategy :: Int -> Strategy
getStrategy 1 = Random
getStrategy 2 = Greedy
getStrategy 3 = Smart

getPlayRandom :: Game -> IO Int
getPlayRandom game@(Game board w h) = randChoice [col | col <- [1..w], isValidColumn game col]

-- Returns the first element of the first pair which has a maximal second element
firstEqualToMax :: [(Int, Int)] -> Int
firstEqualToMax xs
    | y == maxim = x
    | otherwise = firstEqualToMax (tail xs)
    where
        (x, y) = head xs
        maxim = maximum (map snd xs)

getPlayGreedy :: Game -> Int
getPlayGreedy game@(Game board w h) 
    = firstEqualToMax . filter (isValidColumn game . fst)
        $ [(i, getMaxConsecutiveCol (setPiece Computer game i) i) | i <- [1..w]]

getScores :: Turn -> Game -> [Int]
getScores turn game@(Game board w h)
    = [getMaxConsecutivePos game row col | 
             row <- [1..h], 
             col <- [1..w], 
             getPiece board row col == getTurnPiece turn]

evaluateBoard :: Game -> Int
evaluateBoard game@(Game board w h) =
    if myMaximum opp_scores >= 4
        then -10000
    else if myMaximum own_scores >= 4
        then 10000
    else (sum own_scores) - (sum opp_scores)
    where 
        own_scores = getScores Computer game
        opp_scores = getScores Player game
        myMaximum [] = 0
        myMaximum xs = maximum xs
        
simulateNmoves :: Turn -> Game -> Int -> Int
simulateNmoves _ game 0 = evaluateBoard game
simulateNmoves turn game@(Game board w h) n
    | currentEval ==  10000 =  10000
    | currentEval ==  10000 = -10000
    | otherwise = 
        if turn == Computer then maximum simulatedMoves else minimum simulatedMoves
    where
        currentEval = evaluateBoard game
        possibleMoves = filter (isValidColumn game) [1..w]
        simulatedMoves = [simulateNmoves (switchTurn turn) (setPiece turn game col) newN |
                          col <- possibleMoves,
                          isValidColumn game col]
        newN = if turn == Player then (n-1) else n

getPlaySmart :: Int -> Game -> Int
getPlaySmart turnNum game@(Game board w h) = bestMove
    where 
        turnsAhead = min 3 (quot (w*h - turnNum + 1) 2)
        possibleMoves = filter (isValidColumn game) [1..w]
        moves = [(col, simulateNmoves Player (setPiece Computer game col) turnsAhead)| 
                 col <- possibleMoves]
        maxim = maximum $ map snd moves
        bestMoves = filter (\x -> snd x == maxim) moves
        bestMove = fst . head $ bestMoves

getComputerChoice :: Strategy -> Int -> Game -> IO Int
getComputerChoice Random _ = getPlayRandom
getComputerChoice Greedy _ = return . getPlayGreedy
getComputerChoice Smart turnNum = return . (getPlaySmart turnNum)

getPlayerChoice :: Game -> IO Int
getPlayerChoice game@(Game board w h) = readInt "Enter column: " (isValidColumn game)
    
getChoice :: Turn -> Strategy -> Int -> Game -> IO Int
getChoice Player   _     _       game = getPlayerChoice game
getChoice Computer strat turnNum game = getComputerChoice strat turnNum game 

getTurn :: Int -> Turn
getTurn 0 = Player
getTurn 1 = Computer

switchTurn :: Turn -> Turn
switchTurn Player = Computer
switchTurn Computer = Player

getMaxDiagonal1 :: Game -> Int -> Int -> Int
getMaxDiagonal1 game@(Game board w h) row col
    = fst (until isDifferent add1 (row, col)) - fst (until isDifferent sub1 (row, col)) - 1
        where
            current = getPiece board row col
            isDifferent (r, c) = getPiece board r c /= current
            add1 (x, y) = (x+1, y-1)
            sub1 (x, y) = (x-1, y+1)

getMaxDiagonal2 :: Game -> Int -> Int -> Int
getMaxDiagonal2 game@(Game board w h) row col
    = fst (until isDifferent add1 (row, col)) - fst (until isDifferent sub1 (row, col)) - 1
        where
            current = getPiece board row col
            isDifferent (r, c) = getPiece board r c /= current
            add1 (x, y) = (x+1, y+1)
            sub1 (x, y) = (x-1, y-1)

getMaxHorizontal :: Game -> Int -> Int -> Int
getMaxHorizontal game@(Game board w h) row col
    = (until isDifferent (+1) col) - (until isDifferent (+ (-1)) col) - 1
        where
            current = getPiece board row col
            isDifferent c = getPiece board row c /= current

getMaxVertical :: Game -> Int -> Int -> Int
getMaxVertical game@(Game board w h) row col
    = (until isDifferent (+1) row) - (until isDifferent (+ (-1)) row) - 1
        where
            current = getPiece board row col
            isDifferent r = getPiece board r col /= current

getMaxConsecutivePos :: Game -> Int -> Int -> Int
getMaxConsecutivePos game@(Game board w h) row col
    = maximum [getMaxVertical game row col,
               getMaxHorizontal game row col,
               getMaxDiagonal1 game row col,
               getMaxDiagonal2 game row col]

getMaxConsecutiveCol :: Game -> Int -> Int
getMaxConsecutiveCol game@(Game board w h) col
    = getMaxConsecutivePos game row col
    where
        row = (firstEmptyRow board col) - 1 

playRound :: Game -> Strategy -> Turn -> Int -> IO()
playRound game@(Game board w h) strat turn turnNum = do
    putStrLn $ "Turn " ++ (show turnNum) ++ " - " ++ (show turn) ++ " plays"
    putStrLn $ show game
    choice <- getChoice turn strat turnNum game 
    let updatedGame = setPiece turn game choice
    let maxConsecutive = getMaxConsecutiveCol updatedGame choice
    if maxConsecutive >= 4
        then do
            putStrLn $ (show turn) ++ " wins!\n"
            putStrLn $ "Final board:"
            putStrLn $ show updatedGame
    else if turnNum >= w*h
        then do
            putStrLn $ "Tie\n"
            putStrLn $ "Final board:"
            putStrLn $ show updatedGame
    else playRound updatedGame strat (switchTurn turn) (turnNum+1)

playGame :: Int -> Int -> Strategy -> IO()
playGame w h strat = do
    putStrLn $ "Width: " ++ show w
    putStrLn $ "Height: " ++ show h
    putStrLn $ "Strategy: " ++ show strat ++ "\n"
    let game = createGame w h
    starter <- randInt 0 1
    let turn = getTurn starter
    playRound game strat turn 1

main :: IO()
main = do
    putStrLn "Welcome to Connect 4!"
    width <- readInt "Enter the width of the board" (> 0)
    height <- readInt "Enter the height of the board" (> 0)
    strategyChoice <- readInt "1 - Random\n2 - Greedy\n3 - Smart" (\x -> x >= 1 && x <= 3)
    let strategy = getStrategy strategyChoice
    playGame width height strategy
