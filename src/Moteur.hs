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
        |otherwise  = False
        
    instance Eq Situation where 
        (==) = eqSituation

    etatInit :: Niveau -> Etat
    etatInit niv = Etat niv Map.empty 0 0 0 0


    gameInit :: Niveau -> Situation
    gameInit niv = let g = etatInit niv in
                    let (C x y) = getCoordEntree niv in
                    let res = aux (Marcheur R (C (x+1) y)) g 5
                             where aux l e nb
                                    | nb == 0 = ajouterLemming l e
                                    | nb > 0  = aux l (ajouterLemming l e) (nb-1) in
                            EnCours res

    -- gameLoop :: Situation -> Situation
    -- gameLoop Perdu = Perdu
    -- gameLoop Gagne = Gagne
    -- gameLoop (EnCours e@(Etat _ _ nb _ nm ns))
    --     | nm == nb = Perdu
    --     | ns == nb = Gagne
    --     | otherwise = gameLoop (EnCours (transformeEtat 0 e))
    getEtat :: Situation -> Maybe Etat
    getEtat (EnCours e) = Just e
    getEtat _ = Nothing

    transformeEtat :: Situation -> Situation
    transformeEtat s
        |s == Perdu = Perdu
        |s == Gagne = Gagne
        |otherwise = let e@(Etat niv lem nb nv nm ns) = case getEtat s of 
                                                            Just etat -> etat 
                            
                                                        in 
                                                            if nm == nb then 
                                                                Perdu
                                                            else 
                                                                if ns == nb then 
                                                                    Gagne
                                                                else
                                                                    let k =  (EnCours (Map.foldlWithKey (\er k l -> transformeLemming k er) e lem)) in
                                                                         k
                                                                        

        