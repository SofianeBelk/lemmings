module Lemmings where

import  Coord
import Niveau
import qualified Data.Map as Map

data Direction = Gauche | Droite
                deriving (Show, Eq)

data Lemming =  Mort Coord
              | Marcheur Direction Coord
              | Creuseur Direction Coord
              | Poseur Direction Coord
              | Tombeur Direction Int Coord
              deriving Eq
-- >>> Marcheur Gauche (C 3 2) == Marcheur Gauche (C 3 2)
-- True

makeMort :: Coord -> Lemming
makeMort = Mort

makeMarcheur :: Direction -> Coord -> Lemming
makeMarcheur = Marcheur

makeCreuseur :: Direction -> Coord -> Lemming
makeCreuseur = Creuseur

makePoseur :: Direction -> Coord -> Lemming
makePoseur = Poseur

makeTombeur :: Direction -> Int -> Coord -> Lemming
makeTombeur = Tombeur

prop_lemmingInv :: Lemming -> Bool
prop_lemmingInv (Mort c) = prop_coordInv c
prop_lemmingInv (Marcheur _ c) = prop_coordInv c
prop_lemmingInv (Creuseur _ c) = prop_coordInv c
prop_lemmingInv (Poseur _ c) = prop_coordInv c
prop_lemmingInv (Tombeur _ n c) = prop_coordInv c && n > 0

instance Show Lemming where 
    show (Mort _) = "+"
    show (Marcheur Droite _) = ">"
    show (Marcheur Gauche _) = "<"
    show Creuseur {} = "X"
    show Poseur {} = "P"
    show Tombeur {} = "V"

coordLemming :: Lemming -> Coord
coordLemming (Mort c) = c
coordLemming (Marcheur _ c ) = c
coordLemming (Creuseur _ c ) = c
coordLemming (Poseur _ c ) = c
coordLemming (Tombeur _ _ c ) = c

bougeLemming :: Deplacement -> Lemming -> Lemming
bougeLemming _ (Mort c) = Mort c
bougeLemming d (Marcheur di c )
    |d == G || d == GH = Marcheur Gauche (bougeCoord d c)
    |d == D || d == DH = Marcheur Droite (bougeCoord d c)
bougeLemming _ (Creuseur di c) = Creuseur di (bougeCoord B c)
bougeLemming _(Poseur di c) = Poseur di c
bougeLemming _ (Tombeur di n c) = Tombeur di (n-1) (bougeCoord B c)

deplaceLemming :: Coord -> Lemming -> Lemming
deplaceLemming _ (Mort c) = Mort c
deplaceLemming co (Marcheur d _ ) = Marcheur d co
deplaceLemming co (Creuseur d _ ) = Creuseur d co
deplaceLemming co (Poseur d _ ) = Poseur d co
deplaceLemming co (Tombeur d n _ ) = Tombeur d n co

instance Placable Lemming where 
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming




