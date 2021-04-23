module Lemmings where

import  Coord
import Niveau
import qualified Data.Map as Map

data Direction = L | R
                deriving(Eq, Show)


data Lemming = Mort Coord
              | Marcheur Direction Coord 
              | Tombeur Direction Int Coord
              deriving (Eq)
            
instance Show Lemming where 
    show (Mort _) = "+"
    show (Marcheur R _) = ">"
    show (Marcheur L _) = "<"
    show Tombeur {} = "V"


{-class Placable a where
        coordP :: a -> Coord
        bougeP :: a -> Deplacement -> a
        deplaceP :: a -> Coord -> a -}

coordLemming :: Lemming -> Coord
coordLemming (Mort c) = c
coordLemming (Marcheur _ c ) = c
coordLemming (Tombeur _ _ c ) = c

bougeLemming :: Lemming -> Deplacement  -> Lemming
bougeLemming (Mort c) _ = Mort c
bougeLemming (Marcheur di c ) d
    |d == G = Marcheur L (bougeCoord d c)
    |d == D = Marcheur R (bougeCoord d c)
bougeLemming (Tombeur di n c) d = Tombeur di (n-1) (bougeCoord B c)

deplaceLemming :: Lemming -> Coord -> Lemming
deplaceLemming (Mort c) _ = Mort c
deplaceLemming (Marcheur d _ ) co = Marcheur d co
deplaceLemming (Tombeur d n _ ) co = Tombeur d n co

instance Placable Lemming where 
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming


tourLemming :: Lemming -> Niveau -> Lemming
tourLemming (Mort c) Niveau{} =  Mort c
tourLemming (Marcheur d (C x y) ) niv =  if dure (C x (y-1)) niv then 
                                                            case d of
                                                                L -> if passable (C (x-1) y) niv && passable (C (x-1) (y+1)) niv then
                                                                   bougeP (Marcheur d (C x y) ) G
                                                                        else 
                                                                            if passable (C (x-1) (y+1)) niv && passable (C (x-1) (y+2)) niv
                                                                                then bougeP (Marcheur d (C x y)) GH
                                                                            else 
                                                                                Marcheur R (C x y)
                                                                R -> if passable (C (x+1) y) niv && passable (C (x+1) (y+1)) niv then
                                                                   bougeP (Marcheur d (C x y) ) D
                                                                        else 
                                                                            if passable (C (x+1) (y+1)) niv && passable (C (x+1) (y+2)) niv
                                                                                then bougeP (Marcheur d (C x y)) DH
                                                                            else 
                                                                                Marcheur L (C x y)
                                                       else
                                                           Tombeur d 8 (C x y)

tourLemming (Tombeur d n (C x y) ) niv = if dure (C x (y-1)) niv then
                                                            if n<=0 then
                                                                Mort (C x y)
                                                            else 
                                                                Marcheur d (C x y)
                                                     else
                                                        bougeP (Tombeur d n (C x y)) B

