module Etat where

import qualified Data.Map as Map
import Niveau
import Environnement
import Lemmings
import Coord
import Data.String as String
import Data.List as List

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
hauteurMax = 8

rassembleEntNiv :: String -> String -> String
rassembleEntNiv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEntNiv xs1 xs2 else x1:rassembleEntNiv xs1 xs2
rassembleEntNiv [] [] = ""

showEtat :: Etat -> String
showEtat e = rassembleEntNiv (show (enviE e)) (show (niveauE e))

instance Show Etat where
    show etat =  showEtat etat

tourLemming :: Int -> Lemming -> Etat -> Etat
tourLemming n (Mort c) (Etat envi niv r v s) = Etat (enleveEnvi v envi) niv r (v-1) s

tourLemming n (Marcheur Gauche c) (Etat envi niv r m s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) else suite
                                                                where suite = case (passable (gauche c) niv, dure (bas c) niv) of
                                                                                (True, True) -> Etat (deplaceDansEnvironnement n (gauche c) envi) niv r m s
                                                                                (_, False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche hauteurMax c))) envi) niv r m s
                                                                                (_,_) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c)))envi ) niv r m s

tourLemming n (Marcheur Droite c) (Etat envi niv r m s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (m-1) (s+1) else suite
                                                                where suite = case (passable (droite c) niv && passable (haut (droite c)) niv , dure(bas c) niv) of
                                                                                    (True, True) -> Etat (deplaceDansEnvironnement n (droite c) envi) niv r m s
                                                                                    (_,False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite hauteurMax c)))envi) niv r m s
                                                                                    (_,_) ->Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c))) envi) niv r m s

tourLemming n (Tombeur di k c) (Etat envi niv r v s) = case (dure (bas c) niv, n <= 0) of
                                                        (True, True) -> Etat (appliqueIdEnv n (const (Lem n (Mort c))) envi) niv r (v-1) s
                                                        (True, _) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur di c))) envi) niv r v s
                                                        (_, _) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur di (n-1) (bas c)))) (deplaceDansEnvironnement n (bas c) envi)) niv r v s


tourEntite :: Int -> Etat -> Etat
tourEntite n etat = case trouveIdEnvi n (enviE etat) of
                    Nothing -> etat
                    Just (Lem _ l) -> tourLemming n l etat

ajouterLemming :: Etat -> Etat
ajouterLemming (Etat envi niv r v s) = case coordEntree niv of
                                Nothing -> Etat envi niv r v s
                                Just c -> Etat nenvi niv (r-1) (v+1) s
                                 where nenvi = ajouteEntite nlem envi
                                       nlem = Lem (nouveauId envi) (Tombeur Droite hauteurMax c)

tourEtat :: Int -> Etat -> Either Fin Etat
tourEtat t e = (verif . pop) $ foldr etape e (entitesEnvironnement (enviE e))
                where etape enti acc = tourEntite (idEntite enti) acc
                      pop = if restants > 0 && t `mod`5 == 0 then ajouterLemming else id
                      restants = nbLemmingsRestants e
                      verif et = if nbLemmingsRestants et == 0 && nbLemmingsVivants et == 0
                                    then Left $ Victoire $ nbLemmingsSortis et
                                    else Right et

                              
