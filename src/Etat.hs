{-# LANGUAGE EmptyCase #-}
module Etat where

import qualified Data.Map as Map
import Niveau
import Environnement
import Lemmings
import Coord
import Data.String as String
import Data.List as List
import Keyboard as K
import SDL

import Data.Sequence as Seq

data Etat = Etat{
    enviE :: Environnement,
    niveauE :: Niveau,
    nbLemmingsRestants :: Int,
    nbLemmingsVivants :: Int,
    nbLemmingsSortis :: Int
}

data Fin = Victoire Int | Defaite

showFin :: Fin -> String
showFin (Victoire _) = "Victoire"
showFin Defaite = "Defaite"

makeEtat :: Niveau -> Int -> Etat
makeEtat niv@(Niveau h l cases) n = Etat (makeEnvironnement h l) niv n 0 0
instance Show Fin where
  show = showFin

hauteurMax :: Int
hauteurMax = 1

rassembleEntNiv :: String -> String -> String
rassembleEntNiv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEntNiv xs1 xs2 else x1:rassembleEntNiv xs1 xs2
rassembleEntNiv [] [] = ""

showEtat :: Etat -> String
showEtat e = rassembleEntNiv (show (enviE e)) (show (niveauE e))

instance Show Etat where
    show etat =  showEtat etat

tourLemming :: Int -> Lemming -> Etat -> Etat
tourLemming n (Mort c _) (Etat envi niv r v s) = Etat (enleveEnvi n envi) niv r (v-1) s

tourLemming n (Marcheur Gauche c se) (Etat envi niv r m s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) else suite
                                                                where suite = case (passable (gauche c) niv, dure (bas c) niv) of
                                                                                (True, True) -> Etat (deplaceDansEnvironnement n (gauche c) envi) niv r m s
                                                                                (_, False) -> case appliqueIdEnv n (const (Lem n (Tombeur Gauche hauteurMax c se))) envi of
                                                                                        Right e -> Etat e niv r m s
                                                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c se)))envi of
                                                                                        Right e -> Etat e niv r m s

tourLemming n (Marcheur Droite c se) (Etat envi niv r m s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) else suite
                                                                where suite = case (passable (droite c) niv && passable (haut (droite c)) niv , dure(bas c) niv) of
                                                                                    (True, True) -> Etat (deplaceDansEnvironnement n (droite c) envi) niv r m s
                                                                                    (_,False) -> case appliqueIdEnv n (const (Lem n (Tombeur Droite hauteurMax c se)))envi of
                                                                                            Right e-> Etat e niv r m s
                                                                                    (_,_) -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c se))) envi of
                                                                                            Right e -> Etat e niv r m s

tourLemming n (Tombeur di k c se) (Etat envi niv r v s) = case (dure (bas c) niv, n <= 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Mort c se))) envi of
                                                                Right e -> Etat e niv r (v-1) s
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Marcheur di c se))) envi of
                                                                Right e -> Etat e niv r v s
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Tombeur di (n-1) (bas c) se))) (deplaceDansEnvironnement n (bas c) envi) of
                                                                Right e -> Etat e niv r v s

tourLemming n (Creuseur di i c se) (Etat envi niv r v s) = case (terre (bas c) niv, i > 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Creuseur di (i-1) c se))) envi of
                                                                Right e -> Etat e niv r v s
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Tombeur di hauteurMax c se))) envi of
                                                                Right e -> Etat e (supprimerCase c niv) r v s
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Marcheur di c se))) envi of
                                                                Right e -> Etat e niv r v s

tourEntite :: Int -> Etat -> Etat
tourEntite n etat = case trouveIdEnvi n (enviE etat) of
                    Nothing -> etat
                    Just (Lem _ l) -> tourLemming n l etat

ajouterLemming :: Etat -> Etat
ajouterLemming (Etat envi niv r v s) = case coordEntree niv of
                                Nothing -> Etat envi niv r v s
                                Just c -> Etat nenvi niv (r-1) (v+1) s
                                 where nenvi = ajouteEntite nlem envi
                                       nlem = Lem (nouveauId envi) (Tombeur Droite hauteurMax c False)

tourEtat :: Int -> Etat -> Either Fin Etat
tourEtat t e = (verif . pop) $ foldr etape e (entitesEnvironnement (enviE e))
                where etape enti acc = tourEntite (idEntite enti) acc
                      pop = if restants > 0 && t `mod`5 == 0 then ajouterLemming else id
                      restants = nbLemmingsRestants e
                      verif et = if nbLemmingsRestants et == 0 && nbLemmingsVivants et == 0
                                    then Left $ Victoire $ nbLemmingsSortis et
                                    else Right et


selectLemming :: Int -> Etat -> Etat
selectLemming id etat@(Etat envi niv r m s) = let etat' = allFalse etat in case Environnement.trouveIdSeq id (entitesEnvironnement (enviE etat')) of
                            Just (Lem id l) -> case l of
                                (Mort c _) -> case appliqueIdEnv id (const (Lem id (Mort c True))) (enviE etat') of
                                    Right e -> Etat e niv r m s
                                    Left _ -> etat'
                                (Marcheur d c _) -> case appliqueIdEnv id (const (Lem id (Marcheur d c True))) (enviE etat') of
                                                        Right e -> Etat e niv r m s
                                                        Left _ -> etat
                                (Creuseur d i c _) -> case appliqueIdEnv id (const (Lem id (Creuseur d i c True)))(enviE etat') of
                                                         Right e -> Etat e niv r m s
                                                         Left _ -> etat
                                (Poseur d c _) -> case appliqueIdEnv id (const (Lem id (Poseur d c True)))(enviE etat') of
                                                        Right e -> Etat e niv r m s
                                                        Left _ -> etat
                                (Tombeur d n c _) -> case appliqueIdEnv n (const (Lem n (Tombeur d n c True))) (enviE etat') of
                                                        Right e -> Etat e niv r m s
                                                        Left _ -> etat
                            Nothing -> etat'

allFalse :: Etat -> Etat
allFalse etat@(Etat (Environnement h l seq ma) niv r m s) = Etat (Environnement h l seq (Map.mapWithKey (\k s -> Seq.mapWithIndex (\i e -> case e of
                                                               Lem id (Mort c _) -> Lem id (Mort c False)
                                                               Lem id (Marcheur d c _) -> Lem id (Marcheur d c False)
                                                               Lem id (Creuseur d i c _) -> Lem id (Creuseur d i c False)
                                                               Lem id (Tombeur d n c _) -> Lem id (Tombeur d n c False)
                                                                ) s) ma)) niv r m s


playLemming ::  Int -> Etat -> Keyboard -> Etat
playLemming id etat@(Etat envi niv r v s) kbd =
        let modif
              | K.keypressed KeycodeC kbd = case trouveIdEnvi id envi of
                        Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Creuseur d 8 c True)))envi of
                                                         Right e -> Etat e niv r v s
                                                         Left _ -> etat
                        _ -> etat
              | otherwise = etat
        in modif