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
              | Demineur Direction Coord
              | Brule Int Coord
              deriving Eq

-- constructeurs des Lemmings
prop_pre_makeMort :: Coord -> Bool
prop_pre_makeMort = prop_coord_inv

makeMort :: Coord -> Lemming
makeMort = Mort

prop_pre_makeMarcheur :: Direction -> Coord -> Bool
prop_pre_makeMarcheur _ = prop_coord_inv

makeMarcheur :: Direction -> Coord -> Lemming
makeMarcheur d c = Marcheur d c False

prop_pre_makeCreuseur :: Direction -> Int -> Coord -> Bool
prop_pre_makeCreuseur _  i c =  i >= 0 && prop_coord_inv c

makeCreuseur :: Direction -> Int -> Coord -> Lemming
makeCreuseur = Creuseur

prop_pre_makeConstructeur :: Direction -> Int -> Coord -> Bool
prop_pre_makeConstructeur _  i c =  i >= 0 && prop_coord_inv c

makeConstructeur :: Direction -> Int -> Coord -> Lemming
makeConstructeur = Creuseur

prop_pre_makeExploseur ::  Int -> Coord -> Bool
prop_pre_makeExploseur i c =  i >= 0 && prop_coord_inv c

makeExploseur ::  Int -> Coord -> Lemming
makeExploseur = Exploseur

prop_pre_makeBloqueur :: Direction -> Int -> Coord -> Bool
prop_pre_makeBloqueur _ i c =  i >= 0 && prop_coord_inv c

makeBloqueur ::  Direction -> Int -> Coord -> Lemming
makeBloqueur = Bloqueur

prop_pre_makeBoucheur :: Direction -> Int -> Coord -> Bool
prop_pre_makeBoucheur _ i c =  i >= 0 && prop_coord_inv c

makeBoucheur ::  Direction -> Int -> Coord -> Lemming
makeBoucheur = Bloqueur

prop_pre_makeTombeur :: Direction -> Int -> Coord -> Bool
prop_pre_makeTombeur _ i c =  i >= 0 && prop_coord_inv c

makeTombeur :: Direction -> Int -> Coord -> Lemming
makeTombeur = Tombeur

prop_pre_makeBrule ::  Int -> Coord -> Bool
prop_pre_makeBrule i c =  i >= 0 && prop_coord_inv c

makeBrule :: Int -> Coord -> Lemming
makeBrule = Brule

prop_pre_makeDemineur :: Direction -> Coord -> Bool
prop_pre_makeDemineur _ = prop_coord_inv

makeDemineur :: Direction -> Coord -> Lemming
makeDemineur = Demineur

-- Invariant lemming
prop_lemming_inv :: Lemming -> Bool
prop_lemming_inv (Mort c) = prop_coord_inv c
prop_lemming_inv (Marcheur _ c _) = prop_coord_inv c
prop_lemming_inv (Creuseur _ i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Constructeur _ i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Exploseur i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Bloqueur _ i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Boucheur _ i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Tombeur _ i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Brule i c) = prop_coord_inv c && i >= 0
prop_lemming_inv (Demineur _ c ) = prop_coord_inv c

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

    show (Brule _ _) = "A"

    show (Demineur Gauche _) = "D"
    show (Demineur Droite _) = "d"

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
coordLemming (Brule _ c) = c
coordLemming (Demineur _ c) = c

bougeLemming :: Deplacement -> Lemming -> Lemming
bougeLemming _ (Mort c) = Mort c
bougeLemming d (Marcheur di c s)
    |d == G || d == GH = Marcheur Gauche (bougeCoord d c) s
    |d == D || d == DH = Marcheur Droite (bougeCoord d c) s
bougeLemming _ c@Creuseur {} = c
bougeLemming _ c@Constructeur {} = c
bougeLemming _ b@Bloqueur {} = b
bougeLemming d b@(Boucheur di i c)
    |d == G || d == GH = Boucheur Gauche i (bougeCoord d c)
    |d == D || d == DH = Boucheur Droite i (bougeCoord d c)
bougeLemming _ e@(Exploseur _ _) = e
bougeLemming _ (Tombeur di n c) = Tombeur di (n-1) (bougeCoord B c)
bougeLemming _ (Brule n c) = Brule n c
bougeLemming d b@(Demineur di c)
    |d == G || d == GH = Demineur Gauche (bougeCoord d c)
    |d == D || d == DH = Demineur Droite (bougeCoord d c)

deplaceLemming :: Coord -> Lemming -> Lemming
deplaceLemming _ (Mort c) = Mort c
deplaceLemming co (Marcheur d _ s) = Marcheur d co s
deplaceLemming co (Creuseur d n _) = Creuseur d n co
deplaceLemming co (Constructeur d n _) = Constructeur d n co
deplaceLemming co (Bloqueur d n _) = Bloqueur d n co
deplaceLemming co (Boucheur d n _) = Boucheur d n co
deplaceLemming co (Exploseur n _) = Exploseur n co
deplaceLemming co (Tombeur d n _) = Tombeur d n co
deplaceLemming co (Brule n _) = Brule n co
deplaceLemming co (Demineur d _) = Demineur d co

instance Placable Lemming where
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming




