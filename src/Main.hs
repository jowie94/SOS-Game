module Main where
import Data.Matrix
import System.Random

-- Not score but board
type Score = (Int, Int)
type Move = (Int, Int, Char)
type GameBoard = Matrix Char

data GameState = Game GameBoard Score

-- Auxiliar functions

getLetter :: Int -> Char
getLetter i
    | i == 0 = 'S'
    | otherwise = 'O'

freeRowPositions :: [Char] -> Int -> [Int]
freeRowPositions [] _ = []
freeRowPositions (x:xs) n
    | x == '_' = n:rest
    | otherwise = rest
    where rest = freeRowPositions xs (n+1)

freePositions :: [[Char]] -> Int -> [(Int,Int)]
freePositions [] _ = []
freePositions (x:xs) n = (Prelude.concatMap (\y -> [(n, y)]) (freeRowPositions x 1)) ++ (freePositions xs (n+1))

generateMoves :: [(Int, Int)] -> [Move]
generateMoves [] = []
generateMoves ((r,c):xs) = (r,c,'S'):(r,c,'O'):generateMoves xs

bestScore :: GameBoard -> [Move] -> [(Int, Move)]
bestScore board (mov@(r,c,l):movs)
    | null movs || sc < score = [(score, mov)]
    | sc == score = (score, mov):nxt
    | otherwise = nxt
    where
        nxt@((sc, _):_) = bestScore board movs
        score = doScore mov (setElem l (r,c) board)

printScore :: Score -> IO ()
printScore sc = putStr "Score: " >> print sc

-- Decisional functions

randomDecision :: GameBoard -> Score -> IO Move
randomDecision board _ = do
    i <- getStdRandom (randomR (0::Int,1))
    let l = getLetter i
    let fpos = freePositions (toLists board) 1
    el <- getStdRandom (randomR (0, (length fpos) - 1))
    let (r,c) = fpos !! el
    return (r, c, l)

-- TODO: MinMax IA
bestScoreDecision :: GameBoard -> Score -> IO Move
bestScoreDecision board _ = do
    let moves = generateMoves $ freePositions (toLists board) 1
    let m = bestScore board moves
    n <- getStdRandom (randomR (0, (length m) - 1))
    let (_, mov) = m !! n
    return mov

userDecision :: GameBoard -> Score -> IO Move
userDecision board sc = do
    putStrLn $ prettyMatrix board
    printScore sc
    putStrLn "Please, enter the row: "
    s <- getLine
    let r = (read s)::Int
    putStrLn "Please, enter the column: "
    s <- getLine
    let c = (read s)::Int
    putStrLn "Please, enter the letter: "
    s <- getChar
    _ <- getLine
    let l = s::Char
    -- Maybe control incorrect input?
    return (r,c,l)

-- Game checking functions

check :: GameBoard -> Char -> (Int, Int) -> (Int, Int) -> Int
check board 'O' pos@(r,c) (mx,my)
    | r >= 1 && c >= 1 && r <= rows && c <= cols && (board ! pos) == 'O'
        && mr >= 1 && mc >= 1 && mr <= rows && mc <= cols &&
        pr >= 1 && pc >= 1 && pr <= rows && pc <= cols &&
        (board ! m) == 'S' && (board ! p) == 'S' = 1
    | otherwise = 0
    where
        m@(mr, mc) = (r - mx, c - my)
        p@(pr, pc) = (r + mx, c + my)
        cols = ncols board
        rows = nrows board
check board 'S' pos@(r, c) mov@(mx, my)
    | (board ! pos) == 'S' = (check board 'O' m mov) + (check board 'O' p mov)
    | otherwise = 0
    where
        m = (r - mx, c - my)
        p = (r + mx, c + my)
check _ _ _ _ = 0

doScore :: Move -> GameBoard -> Int
doScore (r, c, l) board = (check board l mov (0,1)) + (check board l mov (1,0)) + (check board l mov (1,1)) + (check board l mov (-1,1))
    where mov = (r,c)

isFull :: GameBoard -> Bool
isFull board = notElem '_' (toList board)

increaseScore :: Score -> Int -> Int -> Score
increaseScore (p1,p2) score player
    | player == 0 = (p1 + score, p2)
    | otherwise = (p1, p2 + score)

-- Play functions

play :: GameState -> [(GameBoard -> Score -> IO Move)] -> Int -> IO GameState
play (Game board score) players player = do
    dec@(r,c,e) <- ((players !! player) board score)
    let mat2 = setElem e (r,c) board
    let sc = doScore dec mat2
    let newScore = increaseScore score sc player
    if isFull mat2 then return (Game mat2 newScore)
    else
        play (Game (setElem e (r, c) mat2) newScore) players
            (if sc == 0 then mod (player + 1) 2 else player)

selectIA :: Int -> IO (GameBoard -> Score -> IO Move)
selectIA player = do
    putStr "Select the IA level for player "
    print player
    putStrLn "1. Easy\n2. Hard"
    s <- getLine
    let level = (read s)::Int
    return (if level == 1 then (randomDecision) else (bestScoreDecision))

main :: IO ()
main = do
    putStrLn "Please, enter the number of rows: "
    s <- getLine
    let r = (read s)::Int
    putStrLn "Please, enter the number of columns: "
    s <- getLine
    let c = (read s)::Int
    let mat = matrix r c (const '_')
    print mat
    putStrLn "Please, select the game mode:\n1. Human vs Machine\n2. Machine vs Machine (Simulation)"
    s <- getLine
    let mode = (read s)::Int
    game <- if mode == 1 then return userDecision else selectIA 1
    game2 <- selectIA 2
    (Game res score) <- play (Game mat (0,0)) [game,game2] 0
    print res
    printScore score
