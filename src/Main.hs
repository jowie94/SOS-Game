-- | Main entry point to the application.
module Main where
import Data.Matrix
import System.Random

-- Not score but board
type Score = (Int, Int)
type Move = (Int, Int, Char)
type GameBoard = Matrix Char

data GameState = Game GameBoard Score

getLetter :: Int -> Char
getLetter i
    | i == 0 = 'S'
    | otherwise = 'O'

-- TODO Function to get free spaces

freeRowPositions :: [Char] -> Int -> [Int]
freeRowPositions [] _ = []
freeRowPositions (x:xs) n
    | x == '_' = n:rest
    | otherwise = rest
    where rest = freeRowPositions xs (n+1)


freePositions :: [[Char]] -> Int -> [(Int,Int)]
freePositions [] _ = []
freePositions (x:xs) n = (Prelude.concatMap (\y -> [(n, y)]) (freeRowPositions x 1)) ++ (freePositions xs $ n+1)

randomDecision :: GameBoard -> IO Move
randomDecision mat = do
    i <- getStdRandom (randomR (0::Int,1))
    let l = getLetter i
    --r <- getStdRandom (randomR (1, nrows mat))
    --c <- getStdRandom (randomR (1, ncols mat))
    let poss = freePositions (toLists mat) 1
    el <- getStdRandom (randomR (0, (length poss) - 1))
    let (r,c) = poss !! el
    return (r, c, l)

check :: GameBoard -> Char -> (Int, Int) -> (Int, Int) -> Int
check mat 'O' pos@(r,c) (mx,my)
    | r >= 1 && c >= 1 && r <= rows && c <= cols && (mat ! pos) == 'O'
        && mr >= 1 && mc >= 1 && mr <= rows && mc <= cols &&
        pr >= 1 && pc >= 1 && pr <= rows && pc <= cols &&
        (mat ! m) == 'S' && (mat ! p) == 'S' = 1
    | otherwise = 0
    where
        m@(mr, mc) = (r - mx, c - my)
        p@(pr, pc) = (r + mx, c + my)
        cols = ncols mat
        rows = nrows mat
check mat 'S' pos@(r, c) mov@(mx, my)
    | (mat ! pos) == 'S' = (check mat 'O' m mov) + (check mat 'O' p mov)
    | otherwise = 0
    where
        m@(mr, mc) = (r - mx, c - my)
        p@(pr, pc) = (r + mx, c + my)

doScore :: Move -> GameBoard -> Int
doScore (r, c, l) mat = (check mat l mov (0,1)) + (check mat l mov (1,0)) + (check mat l mov (1,1))
    where mov = (r,c)

isFull :: GameBoard -> Bool
isFull m = all (/= '_') (toList m)

increaseScore :: Score -> Int -> Int -> Score
increaseScore (p1,p2) score player
    | player == 0 = (p1 + score, p2)
    | otherwise = (p1, p2 + score)

play :: GameState -> [(GameBoard -> IO Move)] -> Int -> IO GameState
play sc@(Game mat score) players player = do
    dec@(r,c,e) <- ((players !! player) $ mat)
    let mat2 = setElem e (r,c) mat
    let sc = doScore dec mat2
    let newScore = increaseScore score sc player
    if isFull mat2 then
        return (Game mat2 newScore)
    else if sc == 0 then
        play (Game (setElem e (r,c) mat2) newScore) players (mod (player + 1) 2)
    else
        play (Game (setElem e (r,c) mat2) newScore) players player

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Please, introduce the number of rows: "
    s <- getLine
    let r = (read s)::Int
    putStrLn "Please, introduce the number of columns: "
    s <- getLine
    let c = (read s)::Int
    let mat = matrix r c (\_ -> '_')
    print mat
    --(Game res score) <- randomMode (Game mat (0,0)) 0
    (Game res score) <- play (Game mat (0,0)) [randomDecision,randomDecision] 0
    print res
    print score