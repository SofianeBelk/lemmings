module Etat where
    
    import qualified Data.Map as Map
    import Niveau
    import Lemmings
    import Coord

    data Etat = Etat{
        niveau :: Niveau,
        lemmings :: Map.Map Int Lemming,
        nbLemmings :: Int,
        nbLemmingsVivants :: Int,
        nbLemmingsMorts :: Int,
        nbLemmingsSortis :: Int
    } deriving Show

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

    transformeLimming :: Int -> Etat -> Etat
    transformeLimming id e@(Etat niv ls nb nv nm ns) = case Map.lookup id ls of
                                                Just l -> let l' = tourLemming l niv in
                                                    case l' of
                                                        Mort _ -> Etat niv (Map.insert id l' ls) nb (nv-1) (nm+1) ns
                                                        _ -> Etat niv (Map.insert id l' ls) nb nv nm ns
                                                _ -> e

-- nb == toujours à nv + nm
-- nb == nm PERDU
-- ns == nb GAGNÉ
-- ns > 0 partiellement gagné