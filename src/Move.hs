module Move (parseMoves) where

import Types (Move(..), Position, Piece)

-- | Lit une liste de mouvements depuis une liste.
-- Chaque élément de la liste représente un unique mouvement.
-- On fait l'hypothèse que les mouvements sont correctement représentés.
-- Si ce n'est pas le cas, cette fonction peut émettre une exception.
parseMoves :: [String] -> [Move]
parseMoves = map read
