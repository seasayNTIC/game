module Rules (availableMoves, applyMoves) where

import qualified Data.Set as S
import Move (Move(..))
import State (State(..), Position, Piece)

-- | Essaye d'appliquer plusieurs mouvements consécutifs à partir de l'état initial du jeu.
applyMoves :: [Move] -> Maybe State
applyMoves moves = foldl (>>=) (Just initialState) (map applyMove moves)

-- | Applique un mouvement à un état de jeu.
applyMove :: State -> Move -> Maybe State
applyMove state (Drop piece position) = dropPiece state piece position
applyMove state (Move from to) = movePiece state from to

-- | Met en jeu un nouveau Gobblet.
dropPiece :: State -> Piece -> Position -> Maybe State
dropPiece state piece position = 
  if position `M.member` board state && snd (board state M.! position) /= ' '
  then Nothing  -- La position est déjà occupée.
  else if piece `notElem` stacks state
       then Nothing  -- La pièce n'est pas dans les piles du joueur.
       else Just state { board = M.insert position piece (board state)
                       , stacks = removePiece piece (stacks state)
                       }
  where
    -- | Enlève une pièce des piles d'un joueur.
    removePiece :: Piece -> Stacks -> Stacks
    removePiece piece (stack1, stack2) =
      if piece `elem` stack1
      then (delete piece stack1, stack2)
      else (stack1, delete piece stack2)

-- | Déplace un Gobblet déjà en jeu.
movePiece :: State -> Position -> Position -> Maybe State
movePiece state from to = 
  if not (from `M.member` board state) || to `M.member` board state && snd (board state M.! to) /= ' '
  then Nothing  -- La position de départ est vide ou la position d'arrivée est déjà occupée.
  else Just state { board = M.insert to piece (M.delete from (board state)) }
  where
    piece = board state M.! from

-- | Renvoie l'ensemble des mouvements possibles à partir d'un état de jeu.
availableMoves :: State -> S.Set Move
availableMoves state = S.fromList (concatMap (movesFromPosition state) (M.keys (board state)))

-- | Renvoie la liste des mouvements possibles à partir d'une position donnée.
movesFromPosition :: State -> Position -> [Move]
movesFromPosition state position =
  case M.lookup position (board state) of
    Just piece | fst piece == player state && snd piece == ' ' -> dropMoves ++ moveMoves
    _ -> []
  where
    dropMoves = [Drop piece position | piece <- stacks state]
    moveMoves = [Move position to | to <- M.keys (board state), to /= position]
