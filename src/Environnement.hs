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

prop_entite_inv :: Entite -> Bool
prop_entite_inv (Lem n l) = n >= 0 && prop_lemming_inv l

idEntite :: Entite -> Int
idEntite (Lem i _) = i

prop_pre_makeEnvironnement :: Int -> Int -> Bool 
prop_pre_makeEnvironnement h l = h > 0 && l > 0

makeEnvironnement :: Int -> Int -> Environnement
makeEnvironnement h l = Environnement h l S.empty M.empty

prop_post_makeEnvironnement :: Int -> Int -> Bool 
prop_post_makeEnvironnement h l = prop_envi_inv (makeEnvironnement h l)

prop_pre_trouveIdEnvi :: Int -> Bool
prop_pre_trouveIdEnvi n = n >= 0

trouveIdEnvi :: Int -> Environnement -> Maybe Entite
trouveIdEnvi n = trouveIdSeq n . entitesEnvironnement

prop_pre_trouveIdSeq :: Int -> Bool
prop_pre_trouveIdSeq n = n >= 0

trouveIdSeq :: Int -> S.Seq Entite -> Maybe Entite
trouveIdSeq n = Prelude.foldr (\e acc -> if idEntite e == n then Just e else acc) Nothing

prop_pre_trouveIdMap :: Int -> Bool
prop_pre_trouveIdMap n = n >= 0

trouveIdMap :: Int -> M.Map Coord (S.Seq Entite) -> Maybe Coord
trouveIdMap n = M.foldrWithKey (\c s acc -> case trouveIdSeq n s of
                                                Nothing -> acc
                                                Just _ -> Just c) Nothing

prop_enviInclusion1 :: Environnement -> Bool
prop_enviInclusion1 (Environnement _ _ ents cases) = Prelude.foldr (\e acc -> case trouveIdMap (idEntite e) cases of
                                                                                    Nothing -> False 
                                                                                    Just c -> c == coordP e ) True ents

prop_enviInclusion2 :: Environnement -> Bool
prop_enviInclusion2 (Environnement _ _ ents cases) = M.foldrWithKey (\c s acc -> Prelude.foldr (\e acc -> case trouveIdSeq (idEntite e) ents of
                            Nothing -> False
                            Just e2 -> acc && coordP e2 == c && coordP e2 == coordP e) acc s) True cases

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

prop_pre_appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> Bool
prop_pre_appliqueIdSeq n _ _ = n >= 0

appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> S.Seq Entite
appliqueIdSeq i f = Prelude.foldr aux S.Empty 
    where aux n acc
            |idEntite n == i = f n S.:<| acc
            |otherwise = n S.:<| acc

data IdError = IdError

prop_pre_appliqueIdEnv :: Int -> (Entite -> Entite) -> Environnement -> Bool
prop_pre_appliqueIdEnv n _ envi = n >= 0 && prop_envi_inv envi

appliqueIdEnv :: Int -> (Entite -> Entite) -> Environnement -> Either IdError Environnement
appliqueIdEnv n f envi@(Environnement h l ents cases) = case trouveIdSeq n ents of
                                                Nothing -> Left IdError
                                                Just e -> Right (Environnement h l nents ncases)
                                                    where   nents = appliqueIdSeq n f ents
                                                            ncases = case trouveIdMap n cases of
                                                                        Nothing -> error $ "appliqueIdEnv : Pas trouv?? l'entit??" ++ show n ++ " dans la Map"
                                                                        Just endroit -> M.foldrWithKey etape M.empty cases
                                                                            where etape co s
                                                                                    | co == endroit = M.insert co (appliqueIdSeq n f s)
                                                                                    | otherwise = M.insert co s
prop_post_appliqueIdEnv :: Int -> (Entite -> Entite) -> Environnement -> Bool
prop_post_appliqueIdEnv n f envi = case appliqueIdEnv n f envi of
                                    Left _ -> True 
                                    Right e -> prop_envi_inv e

prop_pre_enleveId :: Int -> S.Seq Entite -> Bool
prop_pre_enleveId n _ = n >= 0

enleveId :: Int -> S.Seq Entite -> S.Seq Entite
enleveId i = Prelude.foldr etape S.empty 
    where etape e acc
            | idEntite e == i = acc
            | otherwise  = e S.:<|acc

prop_post_enleveId :: Int -> S.Seq Entite -> Bool
prop_post_enleveId n seq = case S.lookup n seq of
                            Nothing -> True 
                            _ -> False

prop_pre_enleveEnvi :: Int -> Environnement -> Bool
prop_pre_enleveEnvi n envi = n >= 0 && prop_envi_inv envi

enleveEnvi :: Int -> Environnement -> Environnement
enleveEnvi n (Environnement h l ents cases) = Environnement h l nents ncases
                                            where   nents = enleveId n ents
                                                    ncases = case trouveIdMap n cases of
                                                            Nothing -> cases
                                                            Just endroit -> case M.lookup endroit cases of
                                                                                    Nothing  -> undefined 
                                                                                    Just s -> M.insert endroit (enleveId n s) cases

prop_post_enleveEnvi :: Int -> Environnement -> Bool 
prop_post_enleveEnvi n envi = (case trouveIdEnvi n (enleveEnvi n envi) of
                                Nothing -> True
                                _ -> False) && prop_envi_inv (enleveEnvi n envi)
                                                                           
prop_pre_deplaceDansEnvironnement :: Int -> Coord -> Environnement -> Bool
prop_pre_deplaceDansEnvironnement n  c envi = n >= 0 && prop_coord_inv c && prop_envi_inv envi

deplaceDansEnvironnement :: Int -> Coord -> Environnement -> Environnement
deplaceDansEnvironnement n dest (Environnement h l ents cases) = case trouveIdSeq n ents of
                                                    Nothing -> error $ "deplaceDansEnvi : Pas trouv?? l'entit?? " ++ show n ++ " dans la Seq"
                                                    Just e -> Environnement h l nents ncases
                                                        where nents = appliqueIdSeq n (deplaceP dest) ents
                                                              ncases = case trouveIdMap n cases of
                                                                        Nothing ->  error $ "deplaceDansEnvi : Pas trouv?? l'entit?? " ++ show n ++ " dans la Map"
                                                                        Just source -> let dents = Y.fromMaybe S.empty $ M.lookup dest cases in
                                                                                        let sents = Y.fromMaybe S.empty $ M.lookup source cases in
                                                                                        let ncases = M.insert source (enleveId n sents) cases in
                                                                                            M.insert dest (deplaceP dest e S.:<| dents) ncases
prop_post_deplaceDansEnvironnement :: Int -> Coord -> Environnement -> Bool 
prop_post_deplaceDansEnvironnement n dest envi@(Environnement h l ents cases) = let envi'@(Environnement h' l' ents' cases') = deplaceDansEnvironnement n dest envi in
                                                                                  (case M.lookup dest cases' of
                                                                                        Nothing -> False 
                                                                                        Just seq -> case S.lookup n seq of
                                                                                                Nothing -> False 
                                                                                                _ -> True) && prop_envi_inv envi'
                                                                                                
nouveauId :: Environnement -> Int
nouveauId (Environnement h l ents cases) = 1 + Prelude.foldr etape 0 ents
    where etape ent = max (idEntite ent)

prop_pre_ajouteEntite :: Entite -> Environnement -> Bool 
prop_pre_ajouteEntite ent envi = prop_entite_inv ent && prop_envi_inv envi

ajouteEntite :: Entite -> Environnement -> Environnement
ajouteEntite ent (Environnement h l ents cases) = Environnement h l nents ncases
                                        where nents = ent S.:<| ents
                                              ncases = M.insert (coordP ent) (ent S.:<| cents) cases
                                                where cents = Y.fromMaybe S.empty $ M.lookup (coordP ent) cases

prop_post_ajouteEntite :: Entite -> Environnement -> Bool 
prop_post_ajouteEntite ent@(Lem n _) envi = let envi' =  ajouteEntite ent envi in
                                                                                (case trouveIdEnvi n envi' of
                                                                                    Just ent -> True
                                                                                    _ -> False) && prop_envi_inv envi'
prop_post_tuerEntiteCase :: Coord -> Int -> Environnement -> Bool
prop_post_tuerEntiteCase c i envi = prop_coord_inv c && prop_envi_inv envi && i >= 0

tuerEntiteCase :: Coord -> Int -> Environnement -> (Environnement, Int)
tuerEntiteCase co i e@(Environnement h l ents cases) = case M.lookup co cases of
                        Just seq -> S.foldlWithIndex (\(envi,acc) _ (Lem id _) -> case appliqueIdEnv id (const (Lem id (Mort co))) envi of
                                                                                Right envi' -> (envi', acc + 1)
                                                                                Left _ -> (envi, acc)) (e,i) seq
                        _ -> (e, i)

prop_pre_tuerEntiteCase :: Coord -> Int -> Environnement -> Bool
prop_pre_tuerEntiteCase c i envi = let (envi',i') = tuerEntiteCase c i envi in
                                        prop_envi_inv envi' && i' >= i

prop_pre_explosion :: Coord -> Environnement -> Bool
prop_pre_explosion co envi = prop_coord_inv co && prop_envi_inv envi

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

prop_post_explosion :: Coord -> Environnement -> Bool
prop_post_explosion co envi = let (envi', i) = explosion co envi in
                                    prop_envi_inv envi' && i >= 0 &&
                                        case M.lookup co (casesEnvironnement envi') of
                                            Just seq -> S.null seq &&
                                                    case M.lookup (droite co) (casesEnvironnement envi') of
                                                        Just seq2 -> S.null seq2 &&
                                                            case M.lookup (gauche co) (casesEnvironnement envi') of
                                                                Just seq3 -> S.null seq3 && 
                                                                    case M.lookup (haut co) (casesEnvironnement envi') of
                                                                        Just seq4 -> S.null seq4 &&
                                                                            case M.lookup (bas co) (casesEnvironnement envi') of
                                                                                Just seq5 -> S.null seq5 &&
                                                                                    case M.lookup (gauche (bas co)) (casesEnvironnement envi') of
                                                                                        Just seq6 -> S.null seq6 &&
                                                                                            case M.lookup (droite (bas co)) (casesEnvironnement envi') of
                                                                                                Just seq7 -> S.null seq7 &&
                                                                                                    case M.lookup (gauche (haut co)) (casesEnvironnement envi') of
                                                                                                        Just seq8 -> S.null seq8 &&
                                                                                                                case M.lookup (droite (haut co)) (casesEnvironnement envi') of
                                                                                                                    Just seq8 -> S.null seq8
                                                                                                                    _ -> False
                                                                                                        _ -> False
                                                                                                _ -> False  
                                                                                        _ -> False
                                                                                _ -> False
                                                                        _ -> False
                                                                _ -> False 
                                                        _ -> False
                                            _ -> False
                            