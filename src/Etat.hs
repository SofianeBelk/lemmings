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

data Fin = Victoire Int | Defaite | Abandon

prop_etat_inv :: Etat -> Bool
prop_etat_inv etat = prop_envi_inv (enviE etat)  && prop_niveau_inv (niveauE etat) && nbLemmingsRestants etat + nbLemmingsVivants etat + nbLemmingsSortis etat > 0

hauteurMax :: Int
hauteurMax = 8          -- Hauteur chute maximale auquelle un Lemming peut survivre

poseMax :: Int
poseMax = 4             -- Nombre de cases dans l'inventaire du Lemming boucheur

nbLemmings :: Int
nbLemmings = 6          -- Nombre initial de Lemmings dans le niveau

showFin :: Fin -> String
showFin (Victoire s) = "Victoire avec " <> show (s*100 `div` nbLemmings) <> "% des lemmings."
showFin Defaite = "Defaite"

prop_pre_makeEtat :: Niveau -> Bool
prop_pre_makeEtat = prop_niveau_inv

makeEtat :: Niveau -> Etat
makeEtat niv@(Niveau h l cases) = Etat (makeEnvironnement h l) niv nbLemmings 0 0

prop_post_makeEtat :: Niveau -> Bool
prop_post_makeEtat niv= prop_etat_inv (makeEtat niv)

instance Show Fin where
  show = showFin

rassembleEntNiv :: String -> String -> String
rassembleEntNiv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEntNiv xs1 xs2 else x1:rassembleEntNiv xs1 xs2
rassembleEntNiv [] [] = ""

showEtat :: Etat -> String
showEtat e = rassembleEntNiv (show (enviE e)) (show (niveauE e))

instance Show Etat where
    show etat =  showEtat etat

prop_pre_tourLemming :: Int -> Lemming -> Etat -> Bool
prop_pre_tourLemming n l etat = n >= 0 && prop_lemming_inv l && prop_etat_inv etat

tourLemming :: Int -> Lemming -> Etat -> Etat

tourLemming n (Mort c) (Etat envi niv r v s) = Etat (enleveEnvi n envi) niv r v s
tourLemming n (Marcheur Gauche c _) etat@(Etat envi niv r v s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (v-1) (s+1) else suite
                                                                where suite = case Map.lookup (bas c) (casesNiveau niv) of
                                                                                        Just Mine -> case appliqueIdEnv n (const (Lem n (Brule 8 c)))envi of
                                                                                                        Right e -> Etat e (activerMine (bas c) niv) r v s
                                                                                                        Left _ -> etat
                                                                                        Nothing -> etat
                                                                                        _ -> case (passable (gauche c) niv && passable (haut (gauche c)) niv, dure (bas c) niv) of
                                                                                                (True, True) -> Etat (deplaceDansEnvironnement n (gauche c) envi) niv r v s
                                                                                                (_, False) -> case appliqueIdEnv n (const (Lem n (Tombeur Gauche hauteurMax c))) envi of
                                                                                                                Right e -> Etat e niv r v s
                                                                                                                Left _ -> etat
                                                                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c False)))envi of
                                                                                                        Right e -> Etat e niv r v s
                                                                                                        Left _ -> etat

tourLemming n (Marcheur Droite c _) etat@(Etat envi niv r v s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (v-1) (s+1) else suite
                                                                where suite = case Map.lookup (bas c) (casesNiveau niv) of
                                                                                        Just Mine -> case appliqueIdEnv n (const (Lem n (Brule 8 c)))envi of
                                                                                                        Right e -> Etat e (activerMine (bas c) niv) r v s
                                                                                                        Left _ -> etat
                                                                                        Nothing -> etat
                                                                                        _ -> case (passable (droite c) niv && passable (haut (droite c)) niv , dure(bas c) niv) of
                                                                                                (True, True) -> Etat (deplaceDansEnvironnement n (droite c) envi) niv r v s
                                                                                                (_,False) -> case appliqueIdEnv n (const (Lem n (Tombeur Droite hauteurMax c)))envi of
                                                                                                                Right e-> Etat e niv r v s
                                                                                                                Left _ -> etat
                                                                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False))) envi of
                                                                                                                Right e -> Etat e niv r v s
                                                                                                                Left _ -> etat

tourLemming n (Tombeur di k c) etat@(Etat envi niv r v s) = case (dure (bas c) niv, k <= 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Mort c))) envi of
                                                                Right e -> Etat e niv r (v-1) s
                                                                Left _ -> etat
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Marcheur di c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Tombeur di (k-1) (bas c)))) (deplaceDansEnvironnement n (bas c) envi) of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Creuseur Gauche i c) etat@(Etat envi niv r v s) = case (terre (gauche (bas c)) niv, i > 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Creuseur Gauche (i-1) c ))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c False))) envi of
                                                                Right e -> Etat e (supprimerCase (gauche (bas c)) niv) r v s
                                                                Left _ -> etat
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Creuseur Droite i c) etat@(Etat envi niv r v s) = case (terre (droite (bas c)) niv, i > 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Creuseur Droite (i-1) c ))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False))) envi of
                                                                Right e -> Etat e (supprimerCase (droite (bas c)) niv) r v s
                                                                Left _ -> etat
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Constructeur Gauche i c) etat@(Etat envi niv r v s) = case (vide (gauche c) niv, i > 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Constructeur Gauche (i-1) c))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False))) envi of
                                                                Right e -> Etat e (poserCase (gauche c) niv) r v s
                                                                Left _ -> etat
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Constructeur Droite i c) etat@(Etat envi niv r v s) = case (vide (droite c) niv, i > 0) of
                                                        (True, True) -> case appliqueIdEnv n (const (Lem n (Constructeur Droite (i-1) c))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                                                        (True, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c False))) envi of
                                                                Right e -> Etat e (poserCase (droite c) niv) r v s
                                                                Left _ -> etat
                                                        (_, _) -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Boucheur Gauche i c) etat@(Etat envi niv r v s)
                | i <= 0 = case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                | otherwise = case coordSortie niv of
                                Nothing -> suite
                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (v-1) (s+1) else suite
                            where suite = case (passable (gauche c) niv, dure (bas c) niv) of
                                                (True, True) -> Etat (deplaceDansEnvironnement n (gauche c) envi) niv r v s
                                                (_, False) -> case appliqueIdEnv n (const (Lem n (Boucheur Gauche (i-1) c))) envi of
                                                                Right e -> Etat e (poserCase (bas c) niv) r v s
                                                                Left _ -> etat
                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Boucheur Droite i c)))envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Boucheur Droite i c) etat@(Etat envi niv r v s)
                | i <= 0 = case appliqueIdEnv n (const (Lem n (Marcheur Droite c False))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                | otherwise = case coordSortie niv of
                                Nothing -> suite
                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (v-1) (s+1) else suite
                            where suite = case (passable (droite c) niv, dure (bas c) niv) of
                                                (True, True) -> Etat (deplaceDansEnvironnement n (droite c) envi) niv r v s
                                                (_, False) -> case appliqueIdEnv n (const (Lem n (Boucheur Droite (i-1) c))) envi of
                                                                Right e -> Etat e (poserCase (bas c) niv) r v s
                                                                Left _ -> etat
                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Boucheur Gauche i c)))envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat

tourLemming n (Exploseur i c) etat@(Etat envi niv r v s)
                | i > 0 = case appliqueIdEnv n (const (Lem n (Exploseur (i-1) c))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                |otherwise = case appliqueIdEnv n (const (Lem n (Mort c))) envi of
                                                                Right e -> let (etat', morts) = explosion c e in Etat etat' (exploserCase c niv) r (v-morts) s
                                                                Left _ -> etat

tourLemming n (Bloqueur d i c) etat@(Etat envi niv r v s)
                | i > 0 = case appliqueIdEnv n (const (Lem n (Bloqueur d (i-1) c))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                | otherwise = case appliqueIdEnv n (const (Lem n (Marcheur d c False))) envi of
                                                                Right e -> Etat e (debloquer c niv) r v s
                                                                Left _ -> etat

tourLemming n (Brule i c) etat@(Etat envi niv r v s)
                | i > 0 = case appliqueIdEnv n (const (Lem n (Brule (i-1) c))) envi of
                                                                Right e -> Etat e niv r v s
                                                                Left _ -> etat
                |otherwise = case appliqueIdEnv n (const (Lem n (Mort c))) envi of
                                                                Right e ->  Etat e (supprimerCase (bas c) niv) r (v-1) s
                                                                Left _ -> etat

tourLemming n (Demineur Gauche c) etat@(Etat envi niv r v s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (v-1) (s+1) else suite
                                                                where suite = case Map.lookup (bas c) (casesNiveau niv) of
                                                                                        Just Mine -> case appliqueIdEnv n (const (Lem n (Marcheur Gauche c False)))envi of
                                                                                                        Right e -> Etat e (desactiverMine (bas c) niv) r v s
                                                                                                        Left _ -> etat
                                                                                        Nothing -> etat
                                                                                        _ -> case (passable (gauche c) niv && passable (haut (gauche c)) niv, dure (bas c) niv) of
                                                                                                (True, True) -> Etat (deplaceDansEnvironnement n (gauche c) envi) niv r v s
                                                                                                (_, False) -> case appliqueIdEnv n (const (Lem n (Tombeur Gauche hauteurMax c))) envi of
                                                                                                                Right e -> Etat e niv r v s
                                                                                                                Left _ -> etat
                                                                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Demineur Droite c)))envi of
                                                                                                        Right e -> Etat e niv r v s
                                                                                                        Left _ -> etat

tourLemming n (Demineur Droite c) etat@(Etat envi niv r v s)  = case coordSortie niv of
                                                                Nothing -> suite
                                                                Just cs -> if cs == c then Etat (enleveEnvi n envi) niv r (v-1) (s+1) else suite
                                                                where suite = case Map.lookup (bas c) (casesNiveau niv) of
                                                                                        Just Mine -> case appliqueIdEnv n (const (Lem n (Marcheur Droite c False)))envi of
                                                                                                        Right e -> Etat e (desactiverMine (bas c) niv) r v s
                                                                                                        Left _ -> etat
                                                                                        Nothing -> etat
                                                                                        _ -> case (passable (droite c) niv && passable (haut (droite c)) niv, dure (bas c) niv) of
                                                                                                (True, True) -> Etat (deplaceDansEnvironnement n (droite c) envi) niv r v s
                                                                                                (_, False) -> case appliqueIdEnv n (const (Lem n (Tombeur Droite hauteurMax c))) envi of
                                                                                                                Right e -> Etat e niv r v s
                                                                                                                Left _ -> etat
                                                                                                (_,_) -> case appliqueIdEnv n (const (Lem n (Demineur Gauche c)))envi of
                                                                                                        Right e -> Etat e niv r v s
                                                                                                        Left _ -> etat

prop_post_tourLemming :: Int -> Lemming -> Etat -> Bool
prop_post_tourLemming n l etat = prop_etat_inv (tourLemming n l etat)

prop_pre_tourEntite :: Int -> Etat -> Bool
prop_pre_tourEntite n etat = n >= 0 && prop_etat_inv etat

tourEntite :: Int -> Etat -> Etat
tourEntite n etat = case trouveIdEnvi n (enviE etat) of
                    Nothing -> etat
                    Just (Lem _ l) -> tourLemming n l etat

prop_post_tourEntite :: Int -> Etat -> Bool
prop_post_tourEntite n etat = prop_etat_inv (tourEntite n etat)

prop_pre_ajouterLemming :: Etat -> Bool
prop_pre_ajouterLemming etat = prop_etat_inv (ajouterLemming etat)

ajouterLemming :: Etat -> Etat
ajouterLemming (Etat envi niv r v s) = case coordEntree niv of
                                Nothing -> Etat envi niv r v s
                                Just c -> Etat nenvi niv (r-1) (v+1) s
                                 where nenvi = ajouteEntite nlem envi
                                       nlem = Lem (nouveauId envi) (Tombeur Droite hauteurMax c)

prop_post_ajouterLemming :: Etat -> Bool
prop_post_ajouterLemming etat@(Etat _ _ r v _) = let etat'@(Etat _ _ r' v' _) = ajouterLemming etat in prop_etat_inv etat' && r' == r - 1 && v' == v + 1

prop_pre_tourEtat :: Int -> Etat -> Bool
prop_pre_tourEtat n etat = n >= 0 && prop_etat_inv etat

tourEtat :: Int -> Etat -> Either Fin Etat
tourEtat t e = verif . pop $ foldr etape e (entitesEnvironnement (enviE e))
                where etape enti acc = tourEntite (idEntite enti) acc
                      pop = if restants > 0 && t `mod`5 == 0 then ajouterLemming else id
                      restants = nbLemmingsRestants e
                      verif et = if nbLemmingsRestants et == 0 && nbLemmingsVivants et == 0
                                    then if nbLemmingsSortis et > 0 then Left $ Victoire $ nbLemmingsSortis et
                                        else Left Defaite
                                    else Right et
   
prop_post_tourEtat :: Int -> Etat -> Bool
prop_post_tourEtat n etat = case tourEtat n etat of
                                Left _ -> True
                                Right etat' -> prop_etat_inv etat'

prop_pre_selectLemming :: Int -> Etat -> Bool
prop_pre_selectLemming id etat = id >= 0 && prop_etat_inv etat

selectLemming :: Int -> Etat -> Etat
selectLemming id etat@(Etat envi niv r m s) = case Environnement.trouveIdSeq id (entitesEnvironnement (enviE etat)) of
                            Just (Lem id l) -> case l of
                                (Marcheur d c _) -> case appliqueIdEnv id (const (Lem id (Marcheur d c True))) (enviE etat) of
                                                        Right e -> Etat e niv r m s
                                                        Left _ -> etat
                                _ -> etat
                            Nothing -> etat

prop_post_selectLemming :: Int -> Etat -> Bool
prop_post_selectLemming id etat = let etat' = selectLemming id etat in 
                                        prop_etat_inv etat' &&
                                                case Seq.lookup id (entitesEnvironnement (enviE etat')) of
                                                        Just (Lem id (Marcheur _ _ se )) -> se
                                                        _ -> False

playLemming ::  Int -> Etat -> Keyboard -> Either Fin Etat
playLemming id etat@(Etat envi niv r v s) kbd =
        let modif
                | K.keypressed KeycodeC kbd = case trouveIdEnvi id envi of
                        Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Creuseur d 8 c)))envi of
                                                         Right e -> Right (Etat e niv r v s)
                                                         Left _ -> Right etat
                        _ -> Right etat
                | K.keypressed KeycodeB kbd = case trouveIdEnvi id envi of
                                Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Constructeur d 8 c)))envi of
                                                         Right e -> Right (Etat e niv r v s)
                                                         Left _ -> Right etat
                                _ -> Right etat
                | K.keypressed KeycodeX kbd = case trouveIdEnvi id envi of
                                Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Exploseur 8 c)))envi of
                                                         Right e -> Right (Etat e niv r v s)
                                                         Left _ -> Right etat
                                _ -> Right etat
                | K.keypressed KeycodeV kbd = case trouveIdEnvi id envi of
                                Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Bloqueur d 8 c))) envi of
                                                         Right e -> Right (Etat e (bloquer c niv) r v s)
                                                         Left _ -> Right etat
                                _ -> Right etat
                | K.keypressed KeycodeN kbd = case trouveIdEnvi id envi of
                                Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Boucheur d poseMax c))) envi of
                                                         Right e -> Right (Etat e niv r v s)
                                                         Left _ -> Right etat
                                _ -> Right etat
                | K.keypressed KeycodeW kbd = case trouveIdEnvi id envi of
                                Just (Lem id (Marcheur d c _)) -> case appliqueIdEnv id (const (Lem id (Demineur d c))) envi of
                                                         Right e -> Right (Etat e niv r v s)
                                                         Left _ -> Right etat
                                _ -> Right etat
                | K.keypressed KeycodeEscape kbd = Left Abandon
                | otherwise = Right etat
        in  modif