module Environnement where

import Lemmings
import Coord
import Data.Sequence as S
import Data.Map as M
import Data.Maybe as Y

data Entite = Lem Int Lemming
    deriving Eq

data Envi = Envi{ hEnvi :: Int,
                                    lEnvi :: Int,
                                    entitesEnvi :: S.Seq Entite,
                                    casesEnvi :: M.Map Coord (S.Seq Entite)}

instance Show Entite where
    show (Lem _ l) = show l

instance Placable Entite where
    coordP (Lem _ l) = coordP l
    bougeP d (Lem i l) = Lem i $ bougeP d l
    deplaceP d (Lem i l) = Lem i $ deplaceP d l

idEnt :: Entite -> Int
idEnt (Lem i _) = i

envide :: Int -> Int -> Envi
envide h l = Envi h l S.empty M.empty

entitesEnvi2 :: Envi -> S.Seq Entite
entitesEnvi2 (Envi h l _ cases) = M.foldl' etape S.Empty cases
    where etape acc s = s <> acc


trouveIdEnvi :: Int -> Envi -> Maybe Entite
trouveIdEnvi n = trouveIdSeq n . entitesEnvi

trouveIdSeq :: Int -> S.Seq Entite -> Maybe Entite
trouveIdSeq n = Prelude.foldr etape Nothing
    where etape e acc = if idEnt e == n then Just e else acc

trouveIdMap :: Int -> M.Map Coord (S.Seq Entite) -> Maybe Coord
trouveIdMap n = M.foldrWithKey etape Nothing
    where etape c s acc = case trouveIdSeq n s of
                        Nothing -> acc
                        Just _ -> Just c

prop_enviInclusion1 :: Envi -> Bool
prop_enviInclusion1 (Envi _ _ ents cases) = Prelude.foldr etape True ents
    where etape e acc = case trouveIdMap (idEnt e) cases of
                        Nothing -> False 
                        Just c -> c == coordP e

prop_enviInclusion2 :: Envi -> Bool
prop_enviInclusion2 (Envi _ _ ents cases) = M.foldrWithKey etape True cases
    where etape c s acc = Prelude.foldr (etape2 c) acc s
            where etape2 c e acc = case trouveIdSeq (idEnt e) ents of
                            Nothing -> False
                            Just e2 -> acc && coordP e2 == c && coordP e2 == coordP e

prop_envi_inv :: Envi -> Bool
prop_envi_inv envi = prop_enviInclusion1 envi && prop_enviInclusion2 envi

instance Show Envi where
    show = showEnvi

showEnvi :: Envi -> String 
showEnvi (Envi h l _ cases) = let s = aux 0 (h-1) in s
    where aux x y = if x == (l - 1)
                        then (if y == 0 then lacase x y else lacase x y ++ "\n" ++ aux 0 (y-1))
                        else lacase x y ++ aux (x+1) y
                    where lacase x y = case M.lookup (C x y) cases of
                            Nothing -> " "
                            Just S.Empty -> " "
                            Just (e S.:<| es) -> show e

caseVide :: Coord -> Envi -> Bool
caseVide (C x y) (Envi h l _ cases) = (x < l) && (x >= 0) && (y < h) && (y >= 0)

appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> S.Seq Entite
appliqueIdSeq i f = Prelude.foldr etape S.Empty 
    where etape n acc
            |idEnt n == i = f n S.:<| acc
            |otherwise = n S.:<| acc

appliqueIdEnv :: Int -> (Entite -> Entite) -> Envi -> Envi
appliqueIdEnv n f envi@(Envi h l ents cases) = case trouveIdSeq n ents of
                                                Nothing -> error $ "appliqueIdEnv : Pas trouvé l'entité" ++ show n ++ " dans la Seq"
                                                Just e -> Envi h l nents ncases
                                                    where   nents = appliqueIdSeq n f ents
                                                            ncases = case trouveIdMap n cases of
                                                                        Nothing -> error $ "appliqueIdEnv : Pas trouvé l'entité" ++ show n ++ " dans la Map"
                                                                        Just endroit -> M.foldrWithKey etape M.empty cases
                                                                            where etape co s
                                                                                    | co == endroit = M.insert co (appliqueIdSeq n f s)
                                                                                    | otherwise = M.insert co s
                                                                            
enleveId :: Int -> S.Seq Entite -> S.Seq Entite
enleveId i = Prelude.foldr etape S.empty 
    where etape e acc
            | idEnt e == i = acc
            | otherwise  = e S.:<|acc

enleveEnvi :: Int -> Envi -> Envi
enleveEnvi n (Envi h l ents cases) = Envi h l nents ncases
                                            where   nents = enleveId n ents
                                                    ncases = case trouveIdMap n cases of
                                                            Nothing -> cases
                                                            Just endroit -> case M.lookup endroit cases of
                                                                                    Nothing  -> undefined 
                                                                                    Just s -> M.insert endroit (enleveId n s) cases
                                                                                    
deplaceDansEnvi :: Int -> Coord -> Envi -> Envi
deplaceDansEnvi n dest (Envi h l ents cases) = case trouveIdSeq n ents of
                                                    Nothing -> error $ "deplaceDansEnvi : Pas trouvé l'entité" ++ show n ++ " dans la Seq"
                                                    Just e -> Envi h l nents ncases
                                                        where nents = appliqueIdSeq n (deplaceP dest) ents
                                                              ncases = case trouveIdMap n cases of
                                                                        Nothing ->  error $ "deplaceDansEnvi : Pas trouvé l'entité" ++ show n ++ " dans la Map"
                                                                        Just source -> let dents = Y.fromMaybe S.empty $ M.lookup dest cases in
                                                                                        let sents = Y.fromMaybe S.empty $ M.lookup source cases in
                                                                                        let ncases = M.insert source (enleveId n sents) cases in
                                                                                            M.insert dest (deplaceP dest e S.:<| dents) ncases

idFrais :: Envi -> Int
idFrais (Envi h l ents cases) = 1 + Prelude.foldr etape 0 ents
    where etape ent = max (idEnt ent)

addEntite :: Entite -> Envi -> Envi
addEntite ent (Envi h l ents cases) = Envi h l nents ncases
                                        where nents = ent S.:<| ents
                                              ncases = M.insert (coordP ent) (ent S.:<| cents) cases
                                                where cents = Y.fromMaybe S.empty $ M.lookup (coordP ent) cases