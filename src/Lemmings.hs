module Lemmings where

import  Coord
import Niveau
import qualified Data.Map as Map

data Direction = Gauche | Droite
                deriving (Show, Eq)

data Lemming =  Mort Coord
              | Marcheur Direction Coord Bool
              | Creuseur Direction Int Coord
              | Constructeur Direction Int Coord
              | Exploseur Int Coord
              | Bloqueur Direction Int Coord
              | Boucheur Direction Int Coord
              | Tombeur Direction Int Coord
              deriving Eq

-- constructeurs des Lemmings
makeMort :: Coord -> Lemming
makeMort = Mort

makeMarcheur :: Direction -> Coord -> Lemming
makeMarcheur d c = Marcheur d c False

makeCreuseur :: Direction -> Int -> Coord -> Lemming
makeCreuseur = Creuseur

makeConstructeur :: Direction -> Int -> Coord -> Lemming
makeConstructeur = Creuseur

makeExploseur ::  Int -> Coord -> Lemming
makeExploseur = Exploseur

makeBloqueur ::  Direction -> Int -> Coord -> Lemming
makeBloqueur = Bloqueur

makeBoucheur ::  Direction -> Int -> Coord -> Lemming
makeBoucheur = Bloqueur

makeTombeur :: Direction -> Int -> Coord -> Lemming
makeTombeur = Tombeur

-- Invariant lemming
prop_lemmingInv :: Lemming -> Bool
prop_lemmingInv (Mort c) = prop_coordInv c
prop_lemmingInv (Marcheur _ c _) = prop_coordInv c
prop_lemmingInv (Creuseur _ _ c) = prop_coordInv c
prop_lemmingInv (Constructeur _ _ c) = prop_coordInv c
prop_lemmingInv (Exploseur _ c) = prop_coordInv c
prop_lemmingInv (Bloqueur _ _ c) = prop_coordInv c
prop_lemmingInv (Boucheur _ _ c) = prop_coordInv c
prop_lemmingInv (Tombeur _ n c) = prop_coordInv c && n > 0

-- Instanciation show
instance Show Lemming where 
    show (Mort _) = "+"

    show (Marcheur Gauche _ False) = "<"
    show (Marcheur Droite _ False) = ">"

    show (Marcheur Droite _ True) = ">'"
    show (Marcheur Gauche _ True) = "<'"

    show (Creuseur Gauche _ _) = "C"
    show (Creuseur Droite _ _) = "c"

    show (Constructeur Gauche _ _) = "B"
    show (Constructeur Droite _ _) = "b"

    show Exploseur {} = "Ex"

    show Bloqueur {} = "Q"

    show (Boucheur Gauche _ _ ) = "P"
    show (Boucheur Droite _ _ ) = "p"

    show (Tombeur Gauche _ _) = "V"
    show (Tombeur Droite _ _) = "v"

-- pour les propriétes ces fonctions doivent vérifier la loi de placable : prop_placableLaw
coordLemming :: Lemming -> Coord
coordLemming (Mort c) = c
coordLemming (Marcheur _ c _) = c
coordLemming (Creuseur _ _ c) = c
coordLemming (Constructeur _ _ c) = c
coordLemming (Exploseur _ c) = c
coordLemming (Bloqueur _ _ c) = c
coordLemming (Boucheur _ _ c) = c
coordLemming (Tombeur _ _ c) = c

bougeLemming :: Deplacement -> Lemming -> Lemming
bougeLemming _ (Mort c) = Mort c
bougeLemming d (Marcheur di c s)
    |d == G || d == GH = Marcheur Gauche (bougeCoord d c) s
    |d == D || d == DH = Marcheur Droite (bougeCoord d c) s
bougeLemming _ c@Creuseur {} = c
bougeLemming _ c@Constructeur {} = c
bougeLemming _ b@Bloqueur {} = b
bougeLemming _ b@Boucheur {} = b
bougeLemming _ e@(Exploseur _ _) = e
bougeLemming _ (Tombeur di n c) = Tombeur di (n-1) (bougeCoord B c)

deplaceLemming :: Coord -> Lemming -> Lemming
deplaceLemming _ (Mort c) = Mort c
deplaceLemming co (Marcheur d _ s) = Marcheur d co s
deplaceLemming co (Creuseur d n _) = Creuseur d n co
deplaceLemming co (Constructeur d n _) = Constructeur d n co
deplaceLemming co (Bloqueur d n _) = Bloqueur d n co
deplaceLemming co (Boucheur d n _) = Boucheur d n co
deplaceLemming co (Exploseur n _) = Exploseur n co
deplaceLemming co (Tombeur d n _) = Tombeur d n co

instance Placable Lemming where 
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming




