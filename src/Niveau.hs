module Niveau where

import qualified Data.Map as Map
import qualified Data.List as List

import Coord

data Case = Metal | Terre | Entree | Sortie | Vide
    deriving Eq

instance Show Case where
    show Vide   =  " "
    show Metal  =  "X"
    show Terre  =  "0"
    show Entree =  "E"
    show Sortie =  "S"

instance Read Case where
    readsPrec _ x = [(lectureCase x ,"")]

lectureCase :: String -> Case
lectureCase " "= Vide
lectureCase "X"= Metal
lectureCase "0"= Terre
lectureCase "E"= Entree
lectureCase "S"= Sortie
lectureCase _= Vide

data Niveau = Niveau {
    hNiveau :: Int,
    lNiveau :: Int,
    casesNiveau :: Map.Map Coord Case}
    deriving Eq

prop_niveau :: Niveau -> Bool
prop_niveau n = prop_niveauEntreeSortie n && prop_niveauMetal n
                && prop_niveauEntreeSortie2 n && prop_niveauCase n

prop_niveauEntreeSortie :: Niveau -> Bool
prop_niveauEntreeSortie (Niveau _ _ cns)  =  Map.foldrWithKey (\ _ y acc -> if y == Entree then acc + 1 else acc)  0 cns == 1
                                            &&
                                             Map.foldrWithKey (\ _ y acc -> if y == Sortie then acc + 1 else acc)  0 cns == 1

prop_niveauMetal :: Niveau -> Bool
prop_niveauMetal (Niveau h l cns) = Map.foldrWithKey (\(C x y) c acc -> if x == h - 1 || y == l - 1 then acc && c == Metal else acc) True cns

prop_niveauEntreeSortie2 :: Niveau -> Bool
prop_niveauEntreeSortie2 (Niveau h l cns) = let (C xe ye) = Map.foldrWithKey (\x y acc -> if y == Entree then x else acc) (C 0 0) cns
                                            in let (C xs ys) = Map.foldrWithKey (\x y acc -> if y == Sortie then x else acc) (C 0 0) cns
                                            in Map.lookup (C xe (ye - 1)) cns == Just Vide && Map.lookup (C xs (ys - 1)) cns == Just Metal


prop_niveauCase :: Niveau -> Bool
prop_niveauCase (Niveau h l cns) =  let maliste = [C x y | x <- [0..(h-1)], y <- [0..(l-1)]] in
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

instance Show Niveau where 
  show = showNiveau

readNiveau :: String -> Niveau
readNiveau = (\(cases, l, h) -> Niveau (h + 1) l cases) . List.foldl' aux (Map.empty, 0, 0) where
                    aux (cases, x, y) '\n' = (cases, 0, y+1)
                    aux (cases, x, y) c = (Map.insert (C x y) (read [c]) cases, x + 1, y)

inverseNiveau :: Niveau -> Niveau
inverseNiveau (Niveau h l cns) = Niveau h l $ Map.foldrWithKey etape Map.empty cns
                                 where etape (C x y) c = Map.insert (C x (h-1-y)) c

instance Read Niveau where
    readsPrec _ x = [(inverseNiveau(readNiveau x) ,"")]

exempleNiveau :: Niveau
exempleNiveau = read "XXXXXX\nX   SX\nX 000X\nX    X\nX000 X\nXE   X\nX    X\nXXXXXX"

-- >>> prop_niveauEntreeSortie exempleNiveau
-- True

-- >>> prop_niveauMetal exempleNiveau
-- True

-- >>> prop_niveauEntreeSortie2 exempleNiveau
-- True

-- >>> prop_niveauCase exempleNiveau
-- True


-- >>> prop_niveau exempleNiveau
-- True

-- >>> show exempleNiveau
-- "XXXXX\nXS  X\nX   X\nX  EX\nXXXXX"


passable :: Coord -> Niveau -> Bool
passable c (Niveau _ _ cns) = Map.lookup c cns == Just Vide || Map.lookup c cns == Just Entree

dure :: Coord -> Niveau -> Bool
dure c (Niveau _ _ cns) = Map.lookup c cns == Just Metal || Map.lookup c cns == Just Terre

getCoordEntree :: Niveau -> Coord
getCoordEntree (Niveau _ _ cns) = Map.foldrWithKey (\c y acc -> if y == Entree then c else acc)  (C 0 0) cns