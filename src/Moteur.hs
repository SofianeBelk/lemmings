module Moteur where
import qualified Data.Map as Map
import Control.Concurrent
-- import System.Console.ANSI
import Etat
import Niveau
import Lemmings
import Coord


tourMoteur :: Int -> Etat -> IO(Either Fin (Int,Etat))
tourMoteur n e = case tourEtat n e of
                    Left f -> return  $ Left f
                    Right ne -> do
                                    print ne
                                    putStrLn ("Tour "<> show n <> " restants : "<> show (nbLemmingsRestants ne) <> ", vivants : "<> show (nbLemmingsVivants ne)<>", sortis : "<> show (nbLemmingsSortis ne))
                                    putStrLn ""
                                    threadDelay 500000
                                   -- clearScreen
                                    return $ Right (n+1, ne)

tourne :: Int -> Etat -> IO String 
tourne n e = do
                    e1 <- tourMoteur n e
                    case e1 of
                        Left f -> return "Fini"
                        Right (i, n) -> tourne i n

lance :: Etat -> IO String
lance = tourne 0




{-
data Situation = EnCours Etat | Perdu | Gagne
                     
makeSituation :: Niveau ->Situation
makeSituation niveau = EnCours(makeEtat niveau)

gagne :: Situation -> Bool 
gagne Gagne = True 
gagne _ = False

perdu :: Situation -> Bool 
perdu Perdu = True 
perdu _ = False

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

getNiveau :: Situation -> Niveau
getNiveau (EnCours (Etat niv _ _ _ _ _)) = niv
getNiveau _ = makeNiveau 0 0 Map.empty

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
                                                                    -}