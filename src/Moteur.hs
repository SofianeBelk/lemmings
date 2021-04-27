module Moteur where
import qualified Data.Map as Map
import Etat
import Niveau
import Lemmings
import Coord

data Situation = EnCours Etat | Perdu | Gagne
                     
showSituation :: Situation -> String
showSituation Perdu = "Perdu"
showSituation Gagne = "Gagné"
showSituation (EnCours e) = show e

instance Show Situation where
    show = showSituation

eqSituation :: Situation -> Situation -> Bool 
eqSituation s1 s2
    |show s1 == show s2 = True
    |otherwise = False
        
instance Eq Situation where 
    (==) = eqSituation

etatInit :: Niveau -> Etat
etatInit niv = Etat niv Map.empty 0 0 0 0

gameInit :: Niveau -> Situation
gameInit niv = let etat = etatInit niv in
        let (C x y) = coordEntree niv in
            transformeSituation (EnCours (ajouterLemming (Marcheur R (C (x+1) (y-1))) etat))

getEtat :: Situation -> Maybe Etat
getEtat (EnCours e) = Just e
getEtat _ = Nothing

introduireLemming :: Situation -> Situation
introduireLemming (EnCours etat@(Etat niv _ _ _ _ _)) = let (C x y) = coordEntree niv in
            EnCours (ajouterLemming (Marcheur R (C (x+1) (y-1))) etat)

transformeSituation :: Situation -> Situation
transformeSituation s
    |s == Perdu = Perdu
    |s == Gagne = Gagne
    |otherwise = let e@(Etat niv lem nb nv nm ns) = case getEtat s of 
                                                        Just etat -> etat 
                                                        in 
                                                            if nm == nb then 
                                                                Perdu
                                                            else 
                                                                if (ns+nm) == nb then 
                                                                    Gagne
                                                                else
                                                                    EnCours (transformeEtat e)