module Lemmings where

import  Coord
import Niveau
import qualified Data.Map as Map

data Direction = L | R
                deriving (Show, Eq)

data Lemming =  Mort Coord
              | Marcheur Direction Coord
              | Creuseur Direction Coord
              | Poseur Direction Coord
              | Tombeur Direction Int Coord
              deriving Eq
-- >>> Marcheur L (C 3 2) == Marcheur L (C 3 2)
-- True

prop_lemmingInv :: Lemming -> Bool
prop_lemmingInv (Mort c) = prop_coordInv c
prop_lemmingInv (Marcheur _ c) = prop_coordInv c
prop_lemmingInv (Creuseur _ c) = prop_coordInv c
prop_lemmingInv (Poseur _ c) = prop_coordInv c
prop_lemmingInv (Tombeur _ n c) = prop_coordInv c && n > 0

instance Show Lemming where 
    show (Mort _) = "+"
    show (Marcheur R _) = ">"
    show (Marcheur L _) = "<"
    show Creuseur {} = "X"
    show Poseur {} = "P"
    show Tombeur {} = "V"

hauteurMax :: Int
hauteurMax = 8

coordLemming :: Lemming -> Coord
coordLemming (Mort c) = c
coordLemming (Marcheur _ c ) = c
coordLemming (Creuseur _ c ) = c
coordLemming (Poseur _ c ) = c
coordLemming (Tombeur _ _ c ) = c

bougeLemming :: Lemming -> Deplacement  -> Lemming
bougeLemming (Mort c) _ = Mort c
bougeLemming (Marcheur di c ) d
    |d == G || d == GH = Marcheur L (bougeCoord d c)
    |d == D || d == DH = Marcheur R (bougeCoord d c)
bougeLemming (Creuseur di c) _ = Creuseur di (bougeCoord B c)
bougeLemming (Poseur di c) _ = Poseur di c
bougeLemming (Tombeur di n c) _ = Tombeur di (n-1) (bougeCoord B c)

deplaceLemming :: Lemming -> Coord -> Lemming
deplaceLemming (Mort c) _ = Mort c
deplaceLemming (Marcheur d _ ) co = Marcheur d co
deplaceLemming (Creuseur d _ ) co = Creuseur d co
deplaceLemming (Poseur d _ ) co = Poseur d co
deplaceLemming (Tombeur d n _ ) co = Tombeur d n co

instance Placable Lemming where 
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming

tourLemming :: Lemming -> Niveau -> (Lemming, Niveau)
tourLemming (Mort c) niv = (Mort c,niv)
tourLemming lem@(Marcheur di (C x y) ) niv = if dure (C x (y-1)) niv then
                                                case di of
                                                    L ->if passable (C (x-1) y) niv && passable (C (x-1) (y+1)) niv then
                                                            (bougeP lem G,niv)
                                                        else 
                                                            if dure (C (x-1) y) niv && passable (C (x-1) (y+1)) niv && passable (C (x-1) (y+2)) niv
                                                                then (bougeP lem GH,niv)
                                                            else 
                                                                (Marcheur R (C x y),niv)
                                                    R ->if passable (C (x+1) y) niv && passable (C (x+1) (y+1)) niv then
                                                            (bougeP lem D,niv)
                                                        else 
                                                            if dure (C (x+1) y) niv && passable (C (x+1) (y+1)) niv && passable (C (x+1) (y+2)) niv
                                                                then (bougeP lem DH,niv)
                                                            else 
                                                                (Marcheur L (C x y),niv)
                                            else
                                                (Tombeur di hauteurMax (C x y),niv)
tourLemming lem@(Creuseur di (C x y)) niv@(Niveau h l cns) = if Map.lookup (C x (y-1)) cns == Just Terre then
                                                                let niv'@(Niveau h' l' cns') = Niveau h l (Map.insert (C x (y-1)) Vide cns) in
                                                                    if Map.lookup (C (x-1) (y-1)) cns' == Just Terre then
                                                                        let niv'@(Niveau h' l' cns') = Niveau h' l' (Map.insert (C (x-1) (y-1)) Vide cns') in
                                                                        if Map.lookup (C (x+1) (y-1)) cns' == Just Terre then
                                                                            let niv'@(Niveau h' l' cns') = Niveau h' l' (Map.insert (C (x-1) (y-1)) Vide cns') in
                                                                                (bougeP lem B, niv')
                                                                        else
                                                                            (bougeP lem B, niv')
                                                                    else
                                                                        if Map.lookup (C (x+1) (y-1)) cns' == Just Terre then
                                                                            let niv'@(Niveau h' l' cns') = Niveau h' l' (Map.insert (C (x-1) (y-1)) Vide cns') in
                                                                                (bougeP lem B, niv')
                                                                        else
                                                                            (bougeP lem B, niv')
                                                            else
                                                                (lem,niv)
tourLemming lem@(Poseur di (C x y)) niv@(Niveau h l cns) = case di of
                                                        L -> case Map.lookup (C (x-1) y) cns of
                                                                Just Vide -> (lem,Niveau h l (Map.insert (C (x-1) y) Terre cns))
                                                                _ -> (lem,niv)
                                                        R -> case Map.lookup (C (x+1) y) cns of
                                                                Just Vide -> (lem,Niveau h l (Map.insert (C (x+1) y) Terre cns))
                                                                _ -> (lem,niv)
tourLemming (Tombeur di n (C x y)) niv =if dure (C x (y-1)) niv then
                                            if n<=0 then
                                                (Mort (C x y),niv)
                                            else 
                                                (Marcheur di (C x y),niv)
                                        else
                                            (bougeP (Tombeur di (n-1) (C x y)) B,niv)




