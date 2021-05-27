module Niveau where

import qualified Data.Map as Map
import qualified Data.List as List

import Coord

-- Case 
data Case = Metal | Terre | Entree | Sortie | Vide | Mine | MineActive |Blocked
    deriving Eq

-- Instanciation show
instance Show Case where
    show Vide   =  " "
    show Metal  =  "X"
    show Terre  =  "0"
    show Entree =  "E"
    show Sortie =  "S"
    show Mine = "M"
    show MineActive = "W"
    show Blocked =  " "

-- Instanciation Read
instance Read Case where
    readsPrec _ x = [(lectureCase x ,"")]

lectureCase :: String -> Case
lectureCase " "= Vide
lectureCase "X"= Metal
lectureCase "0"= Terre
lectureCase "E"= Entree
lectureCase "S"= Sortie
lectureCase "M"= Mine
lectureCase _= Vide

-- Niveau
data Niveau = Niveau {
    hNiveau :: Int,
    lNiveau :: Int,
    casesNiveau :: Map.Map Coord Case}
    deriving Eq

-- Constructeur Niveau
makeNiveau :: Int -> Int -> Map.Map Coord Case -> Niveau
makeNiveau = Niveau

-- Invariant Niveau
prop_niveau_inv :: Niveau -> Bool
prop_niveau_inv n@(Niveau h l cases) = prop_niveauEntreeSortie n && prop_niveauMetal n
                && prop_niveauEntreeSortie2 n && prop_niveauCase n &&h > 0 && l > 0 && not(Map.null cases)

-- Propriétés 1
prop_niveauEntreeSortie :: Niveau -> Bool
prop_niveauEntreeSortie (Niveau _ _ cns)  =  Map.foldrWithKey (\ _ y acc -> if y == Entree then acc + 1 else acc)  0 cns == 1
                                            &&
                                             Map.foldrWithKey (\ _ y acc -> if y == Sortie then acc + 1 else acc)  0 cns == 1

-- Propriétés 2
prop_niveauMetal :: Niveau -> Bool
prop_niveauMetal (Niveau h l cns) = Map.foldrWithKey (\(C x y) c acc -> if x == 0 || y == 0 || x == l - 1 || y == h - 1 then acc && c == Metal else acc) True cns

-- Propriétés 3
prop_niveauEntreeSortie2 :: Niveau -> Bool
prop_niveauEntreeSortie2 (Niveau h l cns) = let (C xe ye) = Map.foldrWithKey (\k c acc -> if c == Entree then k else acc) (C 0 0) cns
                                            in let (C xs ys) = Map.foldrWithKey (\k c acc -> if c == Sortie then k else acc) (C 0 0) cns
                                            in Map.lookup (C xe (ye - 1)) cns == Just Vide && Map.lookup (C xs (ys - 1)) cns == Just Metal
-- Propriétés 4
prop_niveauCase :: Niveau -> Bool
prop_niveauCase (Niveau h l cns) =  let maliste = [C x y | x <- [0..(l-1)], y <- [0..(h-1)]] in
                                        List.foldl (\b  a  ->  case Map.lookup a cns of
                                                                     Just _ -> True
                                                                     Nothing -> False
                                                                     ) True maliste
                                        && Map.foldrWithKey (\ x y acc -> List.foldl (\ b  a -> (a == x) || b) False maliste && acc) True cns

showNiveau :: Niveau -> String
showNiveau (Niveau h l cns) = let (s, _, _) = Map.foldl' aux ("", 0, 0) cns in s
                            where aux (s,x,y) v = if x == (l-1)
                                            then (if y == h -1 then (s ++ show v, 0, 0) else (s ++ show v ++ "\n", 0, y+1))
                                            else (s ++ show v, x+1, y)
-- Instanciation show
instance Show Niveau where 
  show = showNiveau

-- Précondition ReadNiveau
prop_PreReadNiveau :: String -> Bool
prop_PreReadNiveau "" = False
prop_PreReadNiveau _ = True

readNiveau :: String -> Niveau
readNiveau = (\(cases, l, h) -> Niveau (h + 1) l cases) . List.foldl' aux (Map.empty, 0, 0) where
                    aux (cases, x, y) '\n' = (cases, 0, y+1)
                    aux (cases, x, y) c = (Map.insert (C x y) (read [c]) cases, x + 1, y)

-- Post-Condition ReadNiveau
prop_post_readNiveau :: String -> Bool
prop_post_readNiveau nivChaine = prop_niveau_inv (readNiveau nivChaine)

inverseNiveau :: Niveau -> Niveau
inverseNiveau (Niveau h l cns) = Niveau h l $ Map.foldrWithKey etape Map.empty cns
                                 where etape (C x y) c = Map.insert (C x (h-1-y)) c

-- Instanciation read
instance Read Niveau where
    readsPrec _ x = [(inverseNiveau(readNiveau x) ,"")]

-- Exemple d'un niveau 
exempleNiveau :: Niveau
exempleNiveau = read "XXXXXXXXXX\nX E      X\nX        X\nX0000    X\nX        X\nX        X\nX   00000X\nX        X\nX 0000000X\nX        X\nX       SX\nXXXXXXXXXX"


-- pas de Précondition ni de Post-condition "ce sont juste des getteur"
coordEntree :: Niveau -> Maybe Coord
coordEntree (Niveau _ _ cns)  =  let c = Map.foldrWithKey (\ k y acc -> if y == Entree then k else acc)  (C (-1) (-1)) cns in
                                    case c of
                                    C (-1) (-1) -> Nothing
                                    _ -> Just c

coordSortie :: Niveau -> Maybe Coord
coordSortie (Niveau _ _ cns)  =  let c = Map.foldrWithKey (\ k y acc -> if y == Sortie then k else acc)  (C (-1) (-1)) cns in
                                    case c of
                                    C (-1) (-1) -> Nothing
                                    _ -> Just c

passable :: Coord -> Niveau -> Bool
passable c (Niveau _ _ cns) = Map.lookup c cns == Just Vide || Map.lookup c cns == Just Entree || Map.lookup c cns == Just Sortie

vide :: Coord -> Niveau -> Bool
vide c (Niveau _ _ cns) = Map.lookup c cns == Just Vide

dure :: Coord -> Niveau -> Bool
dure c (Niveau _ _ cns) = Map.lookup c cns == Just Metal || Map.lookup c cns == Just Terre ||
                            Map.lookup c cns == Just Blocked || Map.lookup c cns == Just Mine 
                            || Map.lookup c cns == Just MineActive

terre :: Coord -> Niveau -> Bool 
terre c (Niveau _ _ cns) = Map.lookup c cns == Just Terre

supprimerCase :: Coord -> Niveau -> Niveau
supprimerCase c (Niveau h l casesNiveau) = Niveau h l (Map.insert c Vide casesNiveau)

poserCase :: Coord -> Niveau -> Niveau
poserCase co (Niveau h l casesNiveau) = Niveau h l (Map.insert co Terre casesNiveau)

activerMine :: Coord -> Niveau -> Niveau
activerMine co niv@(Niveau h l casesNiveau) = case Map.lookup co casesNiveau of
                                            Just Mine -> Niveau h l (Map.insert co MineActive casesNiveau)
                                            _ -> niv

desactiverMine :: Coord -> Niveau -> Niveau
desactiverMine co niv@(Niveau h l casesNiveau) = case Map.lookup co casesNiveau of
                                            Just Mine -> Niveau h l (Map.insert co Terre casesNiveau)
                                            _ -> niv

exploserCase :: Coord -> Niveau -> Niveau
exploserCase c (Niveau h l casesNiveau) = let m = (case Map.lookup (haut c) casesNiveau of
                                                        Just Terre -> Map.insert (haut c) Vide casesNiveau
                                                        _ -> casesNiveau )in
                                            let m2 = (case Map.lookup (gauche c) m of
                                                        Just Terre -> Map.insert (gauche c) Vide m
                                                        _ -> m )in
                                                let m3 = (case Map.lookup (bas c) m of
                                                        Just Terre -> Map.insert (bas c) Vide m2
                                                        _ -> m2 )in
                                                    let m4 = (case Map.lookup (droite c) m3 of
                                                            Just Terre -> Map.insert (droite c) Vide m3
                                                            _ -> m3 )in
                                                        let m5 = (case Map.lookup (gauche (bas c)) m of
                                                                Just Terre -> Map.insert (gauche (bas c)) Vide m4
                                                                _ -> m4 )in
                                                            let m6 = (case Map.lookup (gauche (haut c)) m5 of
                                                                    Just Terre -> Map.insert (gauche (haut c)) Vide m5
                                                                    _ -> m5 )in
                                                                let m7 = (case Map.lookup (droite (bas c)) m6 of
                                                                        Just Terre -> Map.insert (droite (bas c)) Vide m6
                                                                        _ -> m6 )in
                                                                    let m8 = (case Map.lookup (droite (haut c)) m7 of
                                                                            Just Terre -> Map.insert (droite (haut c)) Vide m7
                                                                            _ -> m7 )in
                                                                        let m9 = (case Map.lookup c m8 of
                                                                                Just Terre -> Map.insert (gauche c) Vide m8
                                                                                _ -> m8 )in
                                                                            Niveau h l m9

bloquer :: Coord -> Niveau -> Niveau
bloquer co (Niveau h l casesNiveau) = Niveau h l (Map.insert co Blocked casesNiveau)

debloquer :: Coord -> Niveau -> Niveau
debloquer co (Niveau h l casesNiveau) = Niveau h l (Map.insert co Vide casesNiveau)