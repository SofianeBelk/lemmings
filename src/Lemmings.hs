module Lemmings where

import  Coord
import Niveau
import qualified Data.Map as Map

data Direction = L | R
                deriving(Eq, Show)

data Lemming =  Mort Coord
              | Marcheur Direction Coord 
              | Tombeur Direction Int Coord
              deriving (Eq)
            
instance Show Lemming where 
    show (Mort _) = "+"
    show (Marcheur R _) = ">"
    show (Marcheur L _) = "<"
    show Tombeur {} = "V"

hauteurMax :: Int
hauteurMax = 8

coordLemming :: Lemming -> Coord
coordLemming (Mort c) = c
coordLemming (Marcheur _ c ) = c
coordLemming (Tombeur _ _ c ) = c

bougeLemming :: Lemming -> Deplacement  -> Lemming
bougeLemming (Mort c) _ = Mort c
bougeLemming (Marcheur di c ) d
    |d == G || d == GH = Marcheur L (bougeCoord d c)
    |d == D || d == DH = Marcheur R (bougeCoord d c)
bougeLemming (Tombeur di n c) _ = Tombeur di (n-1) (bougeCoord B c)

deplaceLemming :: Lemming -> Coord -> Lemming
deplaceLemming (Mort c) _ = Mort c
deplaceLemming (Marcheur d _ ) co = Marcheur d co
deplaceLemming (Tombeur d n _ ) co = Tombeur d n co

instance Placable Lemming where 
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming

tourLemming :: Lemming -> Niveau -> Lemming
tourLemming (Mort c) niv = Mort c
tourLemming lem@(Marcheur di (C x y) ) niv = if dure (C x (y-1)) niv then
                                                case di of
                                                    L ->if passable (C (x-1) y) niv && passable (C (x-1) (y+1)) niv then
                                                            bougeP lem G
                                                        else 
                                                            if dure (C (x-1) y) niv && passable (C (x-1) (y+1)) niv && passable (C (x-1) (y+2)) niv
                                                                then bougeP lem GH
                                                            else 
                                                                Marcheur R (C x y)
                                                    R ->if passable (C (x+1) y) niv && passable (C (x+1) (y+1)) niv then
                                                            bougeP lem D
                                                        else 
                                                            if dure (C (x+1) y) niv && passable (C (x+1) (y+1)) niv && passable (C (x+1) (y+2)) niv
                                                                then bougeP lem DH
                                                            else 
                                                                Marcheur L (C x y)
                                            else
                                                Tombeur di hauteurMax (C x y)

tourLemming (Tombeur di n (C x y)) niv =if dure (C x (y-1)) niv then
                                            if n<=0 then
                                                Mort (C x y)
                                            else 
                                                Marcheur di (C x y)
                                        else
                                            bougeP (Tombeur di (n-1) (C x y)) B

-- >>> tourLemming (Marcheur L (C 1 4)) exempleNiveau
-- V
-- >>> tourLemming (Tombeur L 0 (C 1 1)) exempleNiveau
-- +
-- >>> tourLemming (Tombeur R 3 (C 1 1)) exempleNiveau
-- >
-- >>> tourLemming (Marcheur R (C 9 4) ) exempleNiveau
-- <




