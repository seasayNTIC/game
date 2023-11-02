module State (State(..), Position, Piece, Board, Stacks, Player, initialState) where

import qualified Data.Map as M
import Types (Position, Piece, Player(..))

type Board = M.Map Position [Piece]
type Stacks = [Piece]

data State = State { board :: Board
                   , stacks :: (Stacks, Stacks)
                   , player :: Player
                   , winner :: Maybe Player
                   } deriving (Show)

initialState :: State
initialState = State { board = M.fromList [((x, y), []) | x <- [0..3], y <- [0..3]]
                     , stacks = (replicate 4 (Player1, '1') ++ replicate 4 (Player1, '2') ++ replicate 4 (Player1, '3'),
                                 replicate 4 (Player2, '1') ++ replicate 4 (Player2, '2') ++ replicate 4 (Player2, '3'))
                     , player = Player1
                     , winner = Nothing
                     }
