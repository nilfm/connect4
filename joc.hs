import Text.Read
import System.Random
import Debug.Trace
import Data.List
import Data.Maybe

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
data Piece = Cross | Circle | None | OutOfBoard
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
    show OutOfBoard = "#"

instance Show Game where
    show (Game board w h) = concat (map (showLine board w) [h, h-1..1])

-- Auxiliary functions for Show instances
showLine :: Board -> Int -> Int -> String
showLine board w row = show (board row 1) ++ concat [" " ++ show (board row col) | col <- [2..w]] ++ "\n"


-- Creators
createBoard :: Int -> Int -> Board
createBoard w h row col
    | 1 <= row && row <= h && 1 <= col && col <= w = None
    | otherwise = OutOfBoard

createGame :: Int -> Int -> Game
createGame w h = Game (createBoard w h) w h


-- Functions to get piece at position

-- Pre: 1 <= row <= height, 1 <= col <= width
getPiece :: Board -> Int -> Int -> Piece
getPiece board row col = board row col


-- Functions to set piece at position

-- Pre: the given column has space left over at the top
setPiece :: Turn -> Game -> Int -> Game
setPiece turn (Game board w h) col = Game newBoard w h
    where 
        row = firstEmptyRow board col
        newBoard a b
            | a == row && b == col = getTurnPiece turn
            | otherwise = board a b


firstEmptyRow :: Board -> Int -> Int
firstEmptyRow board col = head $ dropWhile isFull [1..]
    where 
        isFull row = (getPiece board row col /= None) && (getPiece board row col /= OutOfBoard)

getTurnPiece :: Turn -> Piece
getTurnPiece Player = Circle
getTurnPiece Computer = Cross

isValidColumn :: Game -> Int -> Bool
isValidColumn (Game board w h) col = (firstEmptyRow board col) <= h && 1 <= col && col <= w

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

argMax :: [(Int, Int)] -> Int
argMax xs
    = fst . fromJust $ find (\(x, y) -> y == maxim) xs
        where
            maxim = maximum $ map snd xs
            
argMin :: [(Int, Int)] -> Int
argMin xs
    = fst . fromJust $ find (\(x, y) -> y == minim) xs
        where
            minim = minimum $ map snd xs

getPlayGreedy :: Game -> Int
getPlayGreedy game@(Game board w h) 
    = argMax scores
        where
            scores = [(i, getMaxConsecutiveCol (setPiece Computer game i) i) | i <- filter (isValidColumn game) [1..w]]

getScores :: Turn -> Game -> [Int]
getScores turn game@(Game board w h)
    = [getMaxConsecutivePos game row col | 
             row <- [1..h], 
             col <- [1..w], 
             getPiece board row col == getTurnPiece turn]

simulateNmoves :: Game -> Turn -> Int -> Int
simulateNmoves game@(Game board w h) turn 1
    = minimum [evaluateBoard (setPiece Player game i) | i <- [1..w], isValidColumn game i]
simulateNmoves game@(Game board w h) turn n
    | turn == Player = if score < -4 then -5000 else minimum simulated
    | otherwise = maximum simulated
        where
            score = evaluateBoard game
            newN = if turn == Computer then (n-1) else n
            newGames = [setPiece turn game i | i <- [1..w], isValidColumn game i]
            simulated = [simulateNmoves newGame (switchTurn turn) newN | newGame <- newGames]

evaluateBoard :: Game -> Int
evaluateBoard game@(Game board w h) =
    if myMaximum opp_scores >= 4
        then -10000
    else if myMaximum own_scores >= 4
        then 10000
    else difference
    where 
        own_scores = getScores Computer game
        opp_scores = getScores Player game
        difference = (sum own_scores) - (sum opp_scores)
        myMaximum [] = 0
        myMaximum xs = maximum xs

getPlaySmart :: Int -> Game -> Int
getPlaySmart turnNum game@(Game board w h) =
    trace(show scores) $
    argMax scores
        where 
            turnsAhead = min 2 (quot (w*h - turnNum + 1) 2)
            games = [(i, setPiece Computer game i) | i <- [1..w], isValidColumn game i]
            scores = [(i, simulateNmoves newGame Player turnsAhead) | (i, newGame) <- games]

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

getMaxConsecutiveDirection :: Game -> Int -> Int -> ((Int, Int) -> (Int, Int)) -> ((Int, Int) -> (Int, Int)) -> Int
getMaxConsecutiveDirection game@(Game board w h) row col add1 sub1 
    = if isPossible then maxim else 0
        where
            current = getPiece board row col
            isDifferent (r, c) = getPiece board r c /= current
            isBlocked (r, c) = (getPiece board r c /= current) && (getPiece board r c /= None)
            distance (r1, c1) (r2, c2) = max (abs (r1 - r2)) (abs (c1 - c2))
            maxim = distance (until isDifferent add1 (row, col)) (until isDifferent sub1 (row, col)) - 1
            maximPossible = distance (until isBlocked add1 (row, col)) (until isBlocked sub1 (row, col)) - 1
            isPossible = maximPossible >= 4

getMaxConsecutivePos :: Game -> Int -> Int -> Int
getMaxConsecutivePos game@(Game board w h) row col
    = maximum scores
        where
            scores = [getMaxConsecutiveDirection game row col add sub | (add, sub) <- moves]
            moves = [(\(x, y) -> (x+1, y),   \(x, y) -> (x-1, y)),    --vertical
                     (\(x, y) -> (x, y+1),   \(x, y) -> (x, y-1)),    --horizontal
                     (\(x, y) -> (x+1, y-1), \(x, y) -> (x-1, y+1)),  --diagonal (top right) -> (bottom left) 
                     (\(x, y) -> (x+1, y+1), \(x, y) -> (x-1, y-1))]  --diagonal (top left) -> (bottom right)

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
