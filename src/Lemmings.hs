module Lemmings where

import  Coord
import Niveau
import qualified Data.Map as Map

data Direction = Gauche | Droite
                deriving (Show, Eq)

data Lemming =  Mort Coord Bool
              | Marcheur Direction Coord Bool
              | Creuseur Direction Int Coord Bool
              | Poseur Direction Coord Bool
              | Tombeur Direction Int Coord Bool
              deriving Eq

-- constructeur lemming

makeMort :: Coord -> Lemming
makeMort c = Mort c False

makeMarcheur :: Direction -> Coord -> Lemming
makeMarcheur d c = Marcheur d c False

makeCreuseur :: Direction -> Int -> Coord -> Lemming
makeCreuseur d n c = Creuseur d n c False

makePoseur :: Direction -> Coord -> Lemming
makePoseur d c = Poseur d c False

makeTombeur :: Direction -> Int -> Coord -> Lemming
makeTombeur d i c = Tombeur d i c False

-- Invariant lemming

prop_lemmingInv :: Lemming -> Bool
prop_lemmingInv (Mort c _) = prop_coordInv c
prop_lemmingInv (Marcheur _ c _) = prop_coordInv c
prop_lemmingInv (Creuseur _ _ c _) = prop_coordInv c
prop_lemmingInv (Poseur _ c _) = prop_coordInv c
prop_lemmingInv (Tombeur _ n c _) = prop_coordInv c && n > 0

-- Instanciation show

instance Show Lemming where 
    show (Mort _ False) = "+"
    show (Mort _ True) = "+'"

    show (Marcheur Gauche _ False) = "<"
    show (Marcheur Droite _ False) = ">"

    show (Marcheur Droite _ True) = ">'"
    show (Marcheur Gauche _ True) = "<'"


    show (Creuseur Gauche _ _ False) = "C"
    show (Creuseur Droite _ _ False) = "c"

    show (Creuseur Gauche _ _ True) = "C'"
    show (Creuseur Droite _ _ True) = "c'"

    show Poseur {} = "P"

    show (Tombeur Gauche _ _ False) = "V"
    show (Tombeur Droite _ _ False) = "v"

    show (Tombeur Gauche _ _ True) = "V'"
    show (Tombeur Droite _ _ True) = "v'"

-- pour les propriétes ces fonctions doivent vérifier la loi de placable : prop_placableLaw

coordLemming :: Lemming -> Coord
coordLemming (Mort c _) = c
coordLemming (Marcheur _ c _) = c
coordLemming (Creuseur _ _ c _) = c
coordLemming (Poseur _ c _) = c
coordLemming (Tombeur _ _ c _) = c

bougeLemming :: Deplacement -> Lemming -> Lemming
bougeLemming _ (Mort c s) = Mort c s
bougeLemming d (Marcheur di c s)
    |d == G || d == GH = Marcheur Gauche (bougeCoord d c) s
    |d == D || d == DH = Marcheur Droite (bougeCoord d c) s
bougeLemming _ c@Creuseur {} = c
bougeLemming _(Poseur di c s) = Poseur di c s
bougeLemming _ (Tombeur di n c s) = Tombeur di (n-1) (bougeCoord B c) s

deplaceLemming :: Coord -> Lemming -> Lemming
deplaceLemming _ (Mort c s) = Mort c s
deplaceLemming co (Marcheur d _ s) = Marcheur d co s
deplaceLemming co (Creuseur d n _ s) = Creuseur d n co s
deplaceLemming co (Poseur d _ s) = Poseur d co s
deplaceLemming co (Tombeur d n _ s) = Tombeur d n co s

instance Placable Lemming where 
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming




