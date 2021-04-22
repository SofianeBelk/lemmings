module Moteur where
    import qualified Data.Map as Map
    import Etat
    import Niveau
    import Lemmings

    import Coord


    data Situation = EnCours Etat | Perdu | Gagne
                     deriving Show

    etatInit :: Niveau -> Etat
    etatInit niv = Etat niv Map.empty 0 0 0 0


    gameInit :: Niveau -> Situation
    gameInit niv = let g = etatInit niv in
                    let res = aux (Marcheur R (C 0 0)) g 5
                             where aux l e nb
                                    | nb == 0 = ajouterLemming l e
                                    | nb > 0  = aux l (ajouterLemming l e) (nb-1) in
                            EnCours res

    gameLoop :: Situation -> Situation
    gameLoop Perdu = Perdu
    gameLoop Gagne = Gagne
    gameLoop (EnCours e@(Etat _ _ nb _ nm ns))
        | nm == nb = Perdu
        | ns == nb = Gagne
        | otherwise = EnCours (transformeEtat 0 e)

    transformeEtat :: Int -> Etat -> Etat
    transformeEtat n e@(Etat _ _ nb _ _ _)
        | n == nb = e
        | otherwise = transformeEtat (n+1) (transformeLimming n e)


        -- De 0 à nb -> transformeLimming id etat -> etat
        