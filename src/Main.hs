module Main where

import State (State(..), initialState, Position, Piece)
import Rules (applyMove, availableMoves)
import Move (Move(..), parseMoves)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

main :: IO ()
main = gameLoop initialState

gameLoop :: State -> IO ()
gameLoop state = do
    printBoard state
    if null (availableMoves state) then
        putStrLn $ "Game Over. " ++ show (winner state) ++ " wins!"
    else if player state == Player1 then do
        move <- getMove
        let newState = applyMove state move
        case newState of
            Just s -> gameLoop s
            Nothing -> do
                putStrLn "Invalid move. Please try again."
                gameLoop state
    else do
        randomIndex <- randomRIO (0, length (availableMoves state) - 1)
        let move = availableMoves state !! randomIndex
        let newState = applyMove state move
        case newState of
            Just s -> gameLoop s
            Nothing -> error "Computer made an invalid move."

printBoard :: State -> IO ()
printBoard state = do
    putStrLn $ unlines $ map printRow [0..3]
    putStrLn $ unlines $ map printStack (zip (stacks state) ['A', 'B'])
  where
    printRow y = concat [printPosition (x, y) | x <- [0..3]]
    printPosition pos = case M.lookup pos (board state) of
        Just [] -> "__ "
        Just ((color, size):_) -> [color, size] ++ " "
        Nothing -> "?? "
    printStack (stack, label) = label : " " ++ concatMap printPiece stack
    printPiece (color, size) = [color, size] ++ " "




getMove :: IO Move
getMove = do
    putStr "Enter your move: "
    hFlush stdout  -- Ensure the prompt is printed immediately.
    input <- getLine
    let moves = parseMoves [input]
    case moves of
        [move] -> return move
        _      -> do
            putStrLn "Invalid input. Please enter a valid move."
            getMove

