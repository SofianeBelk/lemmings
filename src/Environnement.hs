module Environnement where

import Lemmings
import Coord
import Data.Sequence as S
import Data.Map as M
import Data.Maybe as Y

data Entite = Lem Int Lemming
    deriving Eq

data Environnement = Environnement{ hEnvironnement :: Int,
                                    lEnvironnement :: Int,
                                    entitesEnvironnement :: S.Seq Entite,
                                    casesEnvironnement :: M.Map Coord (S.Seq Entite) }

instance Show Entite where
    show (Lem _ l) = show l

instance Placable Entite where
    coordP (Lem _ l) = coordP l
    bougeP d (Lem i l) = Lem i $ bougeP d l
    deplaceP d (Lem i l) = Lem i $ deplaceP d l

idEntite :: Entite -> Int
idEntite (Lem i _) = i

makeEnvironnement :: Int -> Int -> Environnement
makeEnvironnement h l = Environnement h l S.empty M.empty

trouveIdEnvi :: Int -> Environnement -> Maybe Entite
trouveIdEnvi n = trouveIdSeq n . entitesEnvironnement

trouveIdSeq :: Int -> S.Seq Entite -> Maybe Entite
trouveIdSeq n = Prelude.foldr etape Nothing
    where etape e acc = if idEntite e == n then Just e else acc

trouveIdMap :: Int -> M.Map Coord (S.Seq Entite) -> Maybe Coord
trouveIdMap n = M.foldrWithKey etape Nothing
    where etape c s acc = case trouveIdSeq n s of
                        Nothing -> acc
                        Just _ -> Just c

prop_enviInclusion1 :: Environnement -> Bool
prop_enviInclusion1 (Environnement _ _ ents cases) = Prelude.foldr etape True ents
    where etape e acc = case trouveIdMap (idEntite e) cases of
                        Nothing -> False 
                        Just c -> c == coordP e

prop_enviInclusion2 :: Environnement -> Bool
prop_enviInclusion2 (Environnement _ _ ents cases) = M.foldrWithKey etape True cases
    where etape c s acc = Prelude.foldr (etape2 c) acc s
            where etape2 c e acc = case trouveIdSeq (idEntite e) ents of
                            Nothing -> False
                            Just e2 -> acc && coordP e2 == c && coordP e2 == coordP e

prop_envi_inv :: Environnement -> Bool
prop_envi_inv envi = prop_enviInclusion1 envi && prop_enviInclusion2 envi

instance Show Environnement where
    show = showEnvironnement

showEnvironnement :: Environnement -> String 
showEnvironnement (Environnement h l _ cases) = let s = aux 0 (h-1) in s
    where aux x y = if x == (l - 1)
                        then (if y == 0 then lacase x y else lacase x y ++ "\n" ++ aux 0 (y-1))
                        else lacase x y ++ aux (x+1) y
                    where lacase x y = case M.lookup (C x y) cases of
                            Nothing -> " "
                            Just S.Empty -> " "
                            Just (e S.:<| es) -> show e

appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> S.Seq Entite
appliqueIdSeq i f = Prelude.foldr etape S.Empty 
    where etape n acc
            |idEntite n == i = f n S.:<| acc
            |otherwise = n S.:<| acc

data IdError = IdError

appliqueIdEnv :: Int -> (Entite -> Entite) -> Environnement -> Either IdError Environnement
appliqueIdEnv n f envi@(Environnement h l ents cases) = case trouveIdSeq n ents of
                                                Nothing -> Left IdError
                                                Just e -> Right (Environnement h l nents ncases)
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
            | idEntite e == i = acc
            | otherwise  = e S.:<|acc

enleveEnvi :: Int -> Environnement -> Environnement
enleveEnvi n (Environnement h l ents cases) = Environnement h l nents ncases
                                            where   nents = enleveId n ents
                                                    ncases = case trouveIdMap n cases of
                                                            Nothing -> cases
                                                            Just endroit -> case M.lookup endroit cases of
                                                                                    Nothing  -> undefined 
                                                                                    Just s -> M.insert endroit (enleveId n s) cases
                                                                                    
deplaceDansEnvironnement :: Int -> Coord -> Environnement -> Environnement
deplaceDansEnvironnement n dest (Environnement h l ents cases) = case trouveIdSeq n ents of
                                                    Nothing -> error $ "deplaceDansEnvi : Pas trouvé l'entité " ++ show n ++ " dans la Seq"
                                                    Just e -> Environnement h l nents ncases
                                                        where nents = appliqueIdSeq n (deplaceP dest) ents
                                                              ncases = case trouveIdMap n cases of
                                                                        Nothing ->  error $ "deplaceDansEnvi : Pas trouvé l'entité " ++ show n ++ " dans la Map"
                                                                        Just source -> let dents = Y.fromMaybe S.empty $ M.lookup dest cases in
                                                                                        let sents = Y.fromMaybe S.empty $ M.lookup source cases in
                                                                                        let ncases = M.insert source (enleveId n sents) cases in
                                                                                            M.insert dest (deplaceP dest e S.:<| dents) ncases

nouveauId :: Environnement -> Int
nouveauId (Environnement h l ents cases) = 1 + Prelude.foldr etape 0 ents
    where etape ent = max (idEntite ent)

ajouteEntite :: Entite -> Environnement -> Environnement
ajouteEntite ent (Environnement h l ents cases) = Environnement h l nents ncases
                                        where nents = ent S.:<| ents
                                              ncases = M.insert (coordP ent) (ent S.:<| cents) cases
                                                where cents = Y.fromMaybe S.empty $ M.lookup (coordP ent) cases

tuerEntiteCase :: Coord -> Int -> Environnement -> (Environnement, Int)
tuerEntiteCase co i e@(Environnement h l ents cases) = case M.lookup co cases of
                        Just seq -> S.foldlWithIndex (\(envi,acc) _ (Lem id _) -> case appliqueIdEnv id (const (Lem id (Mort co))) envi of
                                                                                Right envi' -> (envi', acc + 1)
                                                                                Left _ -> (envi, acc)) (e,i) seq
                        _ -> (e, i)

explosion :: Coord -> Environnement -> (Environnement, Int)
explosion co envi = let (envi1, i1) = tuerEntiteCase co 0 envi in
                            let (envi2, i2) = tuerEntiteCase (bas co) i1 envi1 in
                                let (envi3, i3) = tuerEntiteCase (haut co) i2 envi2 in
                                    let (envi4,i4) = tuerEntiteCase (gauche co) i3 envi3 in
                                        let (envi5,i5) = tuerEntiteCase (droite co) i4 envi4 in
                                            let (envi6, i6) = tuerEntiteCase (droite (bas co)) i5 envi5 in
                                                let (envi7,i7) = tuerEntiteCase (gauche (bas co)) i6 envi6 in
                                                    let (envi8,i8) = tuerEntiteCase (droite (haut co)) i7 envi7 in
                                                        tuerEntiteCase (gauche (haut co)) i8 envi8
                            