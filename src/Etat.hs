module Etat where

import qualified Data.Map as Map
import Niveau
import Environnement
import Lemmings
import Coord
import Data.String as String
import Data.List as List

data Etat = Etat{
    enviE :: Envi,
    niveauE :: Niveau,
    nbLemmingsRestants :: Int,
    nbLemmingsVivants :: Int,
    nbLemmingsSortis :: Int
}

data Fin = Victoire Int | Defaite

hauteurMax :: Int
hauteurMax = 8

rassembleEntNiv :: String -> String -> String
rassembleEntNiv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEntNiv xs1 xs2 else x1:rassembleEntNiv xs1 xs2

showEtat :: Etat -> String
showEtat e = rassembleEntNiv (show (enviE e)) (show (niveauE e))

instance Show Etat where
    show etat =  showEtat etat

tourLemming :: Int -> Lemming -> Etat -> Etat
tourLemming n (Mort c) (Etat envi niv r v s) = Etat (enleveEnvi v envi) niv r (v-1) s




-- TOURLEMMING MARCHEUR && CREUSEUR && POSEUR



tourLemming n (Tombeur di k c) (Etat envi niv r v s) = case (dure (bas c) niv, n <= 0) of
                                                        (True, True) -> Etat (appliqueIdEnv n (const (Lem n (Mort c))) envi) niv r (v-1) s
                                                        (True, _) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur di c))) envi) niv r v s
                                                        (_, _) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur di (n-1) (bas c)))) envi) niv r v s


tourEntite :: Int -> Etat -> Etat
tourEntite n etat = case trouveIdEnvi n (enviE etat) of
                    Nothing -> etat
                    Just (Lem _ l) -> tourLemming n l etat

popLem :: Etat -> Etat
popLem (Etat envi niv r v s) = case coordEntree niv of
                                Nothing -> Etat envi niv r v s
                                Just c -> Etat nenvi niv (r-1) (v+1) s
                                 where nenvi = addEntite nlem envi
                                       nlem = Lem (idFrais envi) (Tombeur Droite hauteurMax c)

tourEtat :: Int -> Etat -> Either Fin Etat
tourEtat t e = (verif . pop) $ foldr etape e (entitesEnvi (enviE e))
                where etape enti acc = tourEntite (idEnt enti) acc
                      pop = if restants > 0 && t `mod`5 == 0 then popLem else id
                      restants = nbLemmingsRestants e
                      verif et = if nbLemmingsRestants et == 0 && nbLemmingsVivants et == 0
                                    then Left $ Victoire $ nbLemmingsSortis et
                                    else Right et

                    
{-
tourLemming lem@(Marcheur di (C x y) ) niv = if dure (C x (y-1)) niv then
                                                case di of
                                                    L ->if passable (C (x-1) y) niv && passable (C (x-1) (y+1)) niv then
                                                            (bougeP G lem,niv)
                                                        else 
                                                            if dure (C (x-1) y) niv && passable (C (x-1) (y+1)) niv && passable (C (x-1) (y+2)) niv
                                                                then (bougeP GH lem,niv)
                                                            else 
                                                                (Marcheur R (C x y),niv)
                                                    R ->if passable (C (x+1) y) niv && passable (C (x+1) (y+1)) niv then
                                                            (bougeP D lem,niv)
                                                        else 
                                                            if dure (C (x+1) y) niv && passable (C (x+1) (y+1)) niv && passable (C (x+1) (y+2)) niv
                                                                then (bougeP DH lem,niv)
                                                            else 
                                                                (Marcheur L (C x y),niv)
                                            else
                                                (Tombeur di hauteurMax (C x y),niv)
tourLemming lem@(Creuseur di (C x y)) niv@(Niveau h l cns) = if Map.lookup (C x (y-1)) cns == Just Terre then
                                                                let niv'@(Niveau h' l' cns') = Niveau h l (Map.insert (C x (y-1)) Vide cns) in
                                                                    if Map.lookup (C (x-1) (y-1)) cns' == Just Terre then
                                                                        let niv'@(Niveau h' l' cns') = Niveau h' l' (Map.insert (C (x-1) (y-1)) Vide cns') in
                                                                        if Map.lookup (C (x+1) (y-1)) cns' == Just Terre then
                                                                            let niv'@(Niveau h' l' cns') = Niveau h' l' (Map.insert (C (x-1) (y-1)) Vide cns') in
                                                                                (bougeP B lem, niv')
                                                                        else
                                                                            (bougeP B lem, niv')
                                                                    else
                                                                        if Map.lookup (C (x+1) (y-1)) cns' == Just Terre then
                                                                            let niv'@(Niveau h' l' cns') = Niveau h' l' (Map.insert (C (x-1) (y-1)) Vide cns') in
                                                                                (bougeP B lem, niv')
                                                                        else
                                                                            (bougeP B lem, niv')
                                                            else
                                                                (lem,niv)
tourLemming lem@(Poseur di (C x y)) niv@(Niveau h l cns) = case di of
                                                        L -> case Map.lookup (C (x-1) y) cns of
                                                                Just Vide -> (lem,Niveau h l (Map.insert (C (x-1) y) Terre cns))
                                                                _ -> (lem,niv)
                                                        R -> case Map.lookup (C (x+1) y) cns of
                                                                Just Vide -> (lem,Niveau h l (Map.insert (C (x+1) y) Terre cns))
                                                                _ -> (lem,niv)
tourLemming (Tombeur di n (C x y)) niv =if dure (C x (y-1)) niv then
                                            if n<=0 then
                                                (Mort (C x y),niv)
                                            else 
                                                (Marcheur di (C x y),niv)
                                        else
                                            (bougeP B (Tombeur di (n-1) (C x y)),niv)

-}








{-

makeEtat :: Niveau -> Etat
makeEtat niveau = Etat niveau Map.empty 0 0 0 0

prop_etatInv :: Etat -> Bool 
prop_etatInv (Etat niv _ nb nv nm ns) = prop_niveauInv niv && prop_niveau niv &&
                                            nb == (nv + nm + ns)

ajouterLemming :: Lemming -> Etat -> Etat
ajouterLemming lem (Etat niv lems nb nv nm ns) = Etat niv (Map.insert (nb+1) lem lems) (nb+1) (nv+1) nm ns

prop_ajouterLemmingPost :: Lemming -> Etat -> Bool
prop_ajouterLemmingPost lem etat@(Etat _ lems nb nv _ _) = let etat'@(Etat _ lems' nb' nv' _ _) = ajouterLemming lem etat in
                                                                    (nb + 1) == nb' &&  (nv + 1) == nv' && Map.lookup nb' lems' == Just lem

enleverLemming :: Int -> Etat -> Etat
enleverLemming id etat@(Etat niv lems nb nv nm ns) =
    case Map.lookup id lems of
        Just _ -> Etat niv (Map.delete id lems) nb (nv-1) nm (ns+1)
        _ -> etat

prop_enleverLemmingPost :: Int -> Etat -> Bool
prop_enleverLemmingPost i etat = let etat'@(Etat _ lems' _ _ _ _) = enleverLemming i etat in
                                                                    Map.lookup i lems' == Nothing

deplacerLemming :: Int -> Coord -> Etat -> Etat
deplacerLemming id c etat@(Etat niv lems nb nv nm ns) =
    case Map.lookup id lems of
        Just l -> Etat niv (Map.insert id (deplaceP c l) lems) nb nv nm ns
        _  -> etat

transformeLemming :: Int -> Etat -> Etat
transformeLemming id etat@(Etat niv lems nb nv nm ns) =
    case Map.lookup id lems of
        Just lem -> let (lem',niv') = tourLemming lem niv in
            case lem' of
                Mort _ -> Etat niv' (Map.insert id lem' lems) nb (nv-1) (nm+1) ns
                Marcheur _ co -> if co == coordSortie niv then
                                    Etat niv' (Map.delete id lems) nb nv nm (ns+1)
                                else
                                    Etat niv' (Map.insert id lem' lems) nb nv nm ns
                Tombeur _ _ co -> Etat niv' (Map.insert id lem' lems) nb nv nm ns
        _ -> etat

transformeEtat :: Etat -> Etat
transformeEtat etat@(Etat _ lems _ _ _ _) =
    Map.foldrWithKey (\i lem e -> transformeLemming i e) etat lems

showLemmings :: Etat -> String
showLemmings (Etat _ lems _ _ _ _) = Map.foldrWithKey  (\id lem acc -> case lem of
                                                                        Mort co -> acc ++ "(" ++ show id ++ ")" ++ show lem ++ show co ++ " | "
                                                                        Marcheur _ co -> acc ++ "(" ++ show id ++ ")" ++ show lem ++ show co ++ " | "
                                                                        Tombeur _ _ co -> acc ++ "(" ++ show id ++ ")" ++ show lem ++ show co ++ " | "
                                                                        ) "" lems

                                                                        -}