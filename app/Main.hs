module Main where
import Moteur
import Niveau
import Etat
import Data.Map

e :: Situation
e = gameInit exempleNiveau

main :: IO ()
main =  do
    gameLoop e 10


gameLoop :: Situation -> Int -> IO()
gameLoop Perdu _ = print Perdu
gameLoop Gagne _ = print Gagne
gameLoop s@(EnCours e@(Etat _ _ nb _ nm ns)) 1
    | nm == nb = print Perdu
    | ns == nb = print Gagne
    | otherwise = do
         print s
         gameLoop (transformeSituation s) 1
gameLoop s@(EnCours e@(Etat _ _ nb _ nm ns)) n
    | nm == nb = print Perdu
    | ns == nb = print Gagne
    | otherwise = do
         print s
         gameLoop (transformeSituation (introduireLemming s)) (n -1)