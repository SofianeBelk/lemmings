module Etat where
    
    import qualified Data.Map as Map
    import Niveau
    import Lemmings
    import Coord

    data Etat = Etat{
        niveau :: Niveau,
        lemmings :: Map.Map Int Lemming,
        nbLemmings :: Int
    }

    ajouterLemming :: Lemming -> Etat -> Etat
    ajouterLemming l (Etat niv ls nb) = Etat niv (Map.insert (nb+1) l ls) (nb+1)

    enleverLemming :: Int -> Etat -> Etat
    enleverLemming id e@(Etat niv ls nb) = case Map.lookup id ls of
                                Just _ -> Etat niv (Map.delete id ls) (nb-1)
                                _ -> e

    deplacerLemming :: Int -> Coord -> Etat -> Etat
    deplacerLemming id c e@(Etat niv ls nb) = case Map.lookup id ls of
                                                Just l -> Etat niv (Map.insert id (deplaceP l c) ls) nb
                                                _  -> e                                          

    transformeLimming :: Int -> Etat -> Etat
    transformeLimming id e@(Etat niv ls nb) = case Map.lookup id ls of
                                                Just l -> Etat niv (Map.insert id (tourLemming l niv) ls) nb
                                                _ -> e