module Niveau where

import qualified Data.Map as Map
import qualified Data.List as List

import Coord

-- Case 

data Case = Metal | Terre | Entree | Sortie | Vide
    deriving Eq

-- Instanciation show

instance Show Case where
    show Vide   =  " "
    show Metal  =  "X"
    show Terre  =  "0"
    show Entree =  "E"
    show Sortie =  "S"

-- Instanciation Read

instance Read Case where
    readsPrec _ x = [(lectureCase x ,"")]

lectureCase :: String -> Case
lectureCase " "= Vide
lectureCase "X"= Metal
lectureCase "0"= Terre
lectureCase "E"= Entree
lectureCase "S"= Sortie
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

prop_niveauInv :: Niveau -> Bool
prop_niveauInv (Niveau h l cases) = h > 0 && l > 0 && not(Map.null cases)

-- Propriétés Niveau

prop_niveau :: Niveau -> Bool
prop_niveau n = prop_niveauEntreeSortie n && prop_niveauMetal n
                && prop_niveauEntreeSortie2 n && prop_niveauCase n

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

prop_PostReadNiveau :: String -> Bool
prop_PostReadNiveau nivChaine = case readNiveau nivChaine of
                                     Niveau h l cns -> True

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


dure :: Coord -> Niveau -> Bool
dure c (Niveau _ _ cns) = Map.lookup c cns == Just Metal || Map.lookup c cns == Just Terre

terre :: Coord -> Niveau -> Bool 
terre c (Niveau _ _ cns) = Map.lookup c cns == Just Terre


supprimerCase :: Coord -> Niveau -> Niveau
supprimerCase c (Niveau h l casesNiveau) = Niveau h l (Map.insert (bas c) Vide casesNiveau)
