module Types
  ( Position
  , Player(..)
  , Piece
  , Move(..)
  ) where

type Position = (Int, Int)
type Piece = (Player, Char)  -- Le premier élément représente le joueur (Player1 ou Player2) et le deuxième élément représente la taille ('1' pour petit, '2' pour moyen, '3' pour grand)
data Move = Drop Piece Position | Move Position Position deriving (Show, Read)
data Player = Player1 | Player2 deriving (Eq, Show, Read)
