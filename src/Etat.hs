module Etat where

import qualified Data.Map as Map
import Niveau
import Lemmings
import Coord
import Data.String as String
import Data.List as List

data Etat = Etat{
    niveau :: Niveau,
    lemmings :: Map.Map Int Lemming,
    nbLemmings :: Int,
    nbLemmingsVivants :: Int,
    nbLemmingsMorts :: Int,
    nbLemmingsSortis :: Int
}

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
        Just l -> Etat niv (Map.insert id (deplaceP l c) lems) nb nv nm ns
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

showEtat :: Etat -> String
showEtat etat@(Etat niv lems _ _ _ _) = show niv ++ "\n" ++ showLemmings etat

instance Show Etat where
    show etat =  showEtat etat