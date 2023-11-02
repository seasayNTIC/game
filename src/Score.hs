module Score where

import Types (Position, Player(..), Piece)
import State (State(..), Board, Stacks)

import qualified Data.Map as M
import Data.List (subsequences, sort)


-- | Renvoie le score pour un état 'state'.
score :: State -> Int
score state = scorePlayer (concat $ M.elems (board state)) (player state) - scorePlayer (concat $ M.elems (board state)) (opponent (player state))

-- | Calcule le score d'un joueur en fonction de ses pièces sur le plateau.
scorePlayer :: [Piece] -> Player -> Int
scorePlayer pieces player = alignment2 + 10 * alignment3
  where
    playerPieces = filter ((== player) . fst) pieces
    alignment2 = countAlignments playerPieces pieces 2
    alignment3 = countAlignments playerPieces pieces 3

-- | Compte le nombre d'alignements potentiels de longueur n pour un joueur.
countAlignments :: [Piece] -> [Piece] -> Int -> Int
countAlignments pieces playerPieces n = countHorizontalAlignments pieces playerPieces n
                                      + countVerticalAlignments pieces playerPieces n
                                      + countDiagonalAlignments pieces playerPieces n

-- | Compte le nombre d'alignements horizontaux potentiels de longueur n pour un joueur.
countHorizontalAlignments :: [Piece] -> [Piece] -> Int -> Int
countHorizontalAlignments pieces playerPieces n = sum $ map (\row -> length $ filter (\positions -> isAlignment pieces playerPieces positions) $ subsequencesOfSize n $ rowPositions row) [0..3]

-- | Compte le nombre d'alignements verticaux potentiels de longueur n pour un joueur.
countVerticalAlignments :: [Piece] -> [Piece] -> Int -> Int
countVerticalAlignments pieces playerPieces n = sum $ map (\col -> length $ filter (\positions -> isAlignment pieces playerPieces positions) $ subsequencesOfSize n $ colPositions col) [0..3]

-- | Compte le nombre d'alignements diagonaux potentiels de longueur n pour un joueur.
countDiagonalAlignments :: [Piece] -> [Piece] -> Int -> Int
countDiagonalAlignments pieces playerPieces n = sum $ map (\diagonal -> length $ filter (\positions -> isAlignment pieces playerPieces positions) $ subsequencesOfSize n diagonal) diagonals

-- | Renvoie l'adversaire d'un joueur.
opponent :: Player -> Player
opponent Player1 = Player2
opponent Player2 = Player1


-- | Détermine si une liste de positions forme un alignement potentiel pour un joueur.
isAlignment :: [Piece] -> [Piece] -> [Position] -> Bool
isAlignment pieces playerPieces positions =
  let playerPositions = map snd playerPieces
  in not (null positions) && isConsecutive (sort positions) && all (`elem` playerPositions) positions




-- | Vérifie si une liste de positions est consécutive.
isConsecutive :: [Position] -> Bool
isConsecutive [] = True
isConsecutive [_] = True
isConsecutive ((x1,y1):(x2,y2):xs) = (abs(x1-x2) <= 1 && abs(y1-y2) <= 1) && isConsecutive ((x2,y2):xs)



-- | Renvoie la liste des positions sur une ligne donnée.
rowPositions :: Int -> [Position]
rowPositions row = [(row, col) | col <- [0..3]]

-- | Renvoie la liste des positions sur une colonne donnée.
colPositions :: Int -> [Position]
colPositions col = [(row, col) | row <- [0..3]]

-- | Renvoie la liste des positions sur une diagonale donnée.
diagonals :: [[(Int, Int)]]
diagonals = [ [(i, i) | i <- [0..3]]
            , [(i, 3 - i) | i <- [0..3]]
            ]

-- | Renvoie la liste des sous-listes de taille k d'une liste donnée.
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize k xs = filter ((== k) . length) $ subsequences xs
