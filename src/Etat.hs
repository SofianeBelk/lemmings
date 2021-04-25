module Etat where

    import qualified Data.Map as Map
    import Niveau
    import Lemmings ( tourLemming, Direction(R, L), Lemming(..) )
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

    ajouterLemming :: Lemming -> Etat -> Etat
    ajouterLemming l (Etat niv ls nb nv nm ns) = Etat niv (Map.insert (nb+1) l ls) (nb+1) (nv+1) nm ns

    enleverLemming :: Int -> Etat -> Etat
    enleverLemming id e@(Etat niv ls nb nv nm ns) = case Map.lookup id ls of
                                Just _ -> Etat niv (Map.delete id ls) nb nv nm (ns+1)
                                _ -> e

    deplacerLemming :: Int -> Coord -> Etat -> Etat
    deplacerLemming id c e@(Etat niv ls nb nv nm ns) = case Map.lookup id ls of
                                                Just l -> Etat niv (Map.insert id (deplaceP l c) ls) nb nv nm ns
                                                _  -> e

    transformeLemming :: Int -> Etat -> Etat
    transformeLemming id e@(Etat niv ls nb nv nm ns) = case Map.lookup id ls of
                                                Just l -> let l' = tourLemming l niv in
                                                    case l' of
                                                        Mort _ -> Etat niv (Map.insert id l' ls) nb (nv-1) (nm+1) ns
                                                        _ -> Etat niv (Map.insert id l' ls) nb nv nm ns
                                                _ -> e






-- >>> replaceNth 3 2 (List.take 10 (List.repeat 1))
-- [1,1,1,2,1,1,1,1,1,1]

    showEtat :: Etat -> String
    showEtat e@(Etat niv@(Niveau h l cns) ls nb nv nm ns) = case Map.lookup 1 ls of
                                                                    Just lem -> updateNiveau (show niv) l lem  0 ""
                                                                    _ -> ""
    replaceNth :: Int -> a -> [a] -> [a]
    replaceNth _ _ [] = []
    replaceNth n newVal (x:xs)
                                | n == 0 = newVal:xs
                                | otherwise = x:replaceNth (n-1) newVal xs
    showE :: Etat -> String
    showE e@(Etat niv@(Niveau h l cns) ls nb nv nm ns) =
                                                     let sn = show niv in
                                                       Map.foldlWithKey (\acc id lem -> case lem of
                                                                                              Mort (C x y) ->        replaceNth (l*(y-1)+x+h) (List.head (show lem))  acc
                                                                                              Marcheur L (C x y) ->  replaceNth (l*(y-1)+x+h) (List.head (show lem))  acc
                                                                                              Marcheur R (C x y) ->  replaceNth (l*(y-1)+x+h) (List.head (show lem))  acc
                                                                                              Tombeur _ _ (C x y) ->  replaceNth (l*(y-1)+x+h) (List.head (show lem))  acc
                                                                                             ) sn ls


    inverseEtat :: Etat -> Etat
    inverseEtat (Etat niv ls nb nv nm ns) = Etat (inverseNiveau niv) ls nb nv nm ns

    instance Show Etat where
        show etat =  showE (inverseEtat etat)

    updateNiveau :: String -> Int -> Lemming -> Int -> String -> String
    updateNiveau [] _ _ _ acc = acc
    updateNiveau niv l lim i acc = case lim of
                                    Mort (C x y) -> if l*(y-1)+x == i then updateNiveau (List.tail niv) l lim (i+1) (acc ++ show lim) else updateNiveau (List.tail niv) l lim (i+1) acc ++ [List.head niv]
                                    Marcheur _ (C x y) -> if l*(y-1)+x == i then updateNiveau (List.tail niv) l lim (i+1) (acc ++ show lim) else updateNiveau (List.tail niv) l lim (i+1) acc ++ [List.head niv]
                                    Tombeur _ _ (C x y) -> if l*(y-1)+x == i then updateNiveau (List.tail niv) l lim (i+1) (acc ++ show lim) else updateNiveau (List.tail niv) l lim (i+1) acc ++ [List.head niv]



-- nb == toujours à nv + nm
-- nb == nm PERDU
-- ns == nb GAGNÉ
-- ns > 0 partiellement gagné
