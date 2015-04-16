-- | Main entry point to the application.
module Main where
import Data.Matrix
import System.Random

-- Not score but board
type Score = (Int, Int)
type Move = (Int, Int, Char)
type GameBoard = Matrix Char

data Game = Game GameBoard Score

getLetter :: Int -> Char
getLetter i
    | i == 0 = 'S'
    | otherwise = 'O'

-- TODO Function to get free spaces

randomDecision :: GameBoard -> IO Move
randomDecision mat = do
    i <- getStdRandom (randomR (0::Int,1))
    let l = getLetter i
    r <- getStdRandom (randomR (1, nrows mat))
    c <- getStdRandom (randomR (1, ncols mat))
    if (mat ! (r,c)) /= '_' then
        randomDecision mat
    else return (r, c, l)

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

-- TODO extract to a general function

randomMode ::Game -> Int -> IO Game
randomMode sc@(Game mat score) player = do
    dec@(r,c,e) <- randomDecision mat
    let mat2 = setElem e (r,c) mat
    let sc = doScore dec mat2
    let newScore = increaseScore score sc player
    if isFull mat2 then
        return (Game mat2 newScore)
    else if sc == 0 then
        randomMode (Game (setElem e (r,c) mat2) newScore) (mod (player + 1) 2)
    else
        randomMode (Game (setElem e (r,c) mat2) newScore) player

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
    (Game res score) <- randomMode (Game mat (0,0)) 0
    print res
    print score