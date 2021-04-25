module Main where

import Moteur
import Niveau
import Etat
import Data.Map ( foldlWithKey )


e :: Situation
e = gameInit exempleNiveau

main :: IO ()
main =  do
    print e
    (loopMain (gameLoop e))
    (loopMain (gameLoop (gameLoop e)))
    (loopMain (gameLoop (gameLoop (gameLoop e))))
    (loopMain (gameLoop(gameLoop (gameLoop (gameLoop e)))))
    (loopMain (gameLoop(gameLoop(gameLoop (gameLoop (gameLoop e))))))
    (loopMain (gameLoop(gameLoop(gameLoop(gameLoop (gameLoop (gameLoop e)))))))
    (loopMain (gameLoop(gameLoop(gameLoop(gameLoop(gameLoop (gameLoop (gameLoop e))))))))
    (loopMain (gameLoop(gameLoop(gameLoop(gameLoop(gameLoop(gameLoop (gameLoop (gameLoop e)))))))))
    (loopMain (gameLoop(gameLoop(gameLoop(gameLoop(gameLoop(gameLoop(gameLoop (gameLoop (gameLoop e))))))))))
    (loopMain (gameLoop(gameLoop(gameLoop(gameLoop(gameLoop(gameLoop(gameLoop(gameLoop (gameLoop (gameLoop e)))))))))))
    {- do
                let  (EnCours etat@(Etat nv gh nb b nm ns)) =  e in
                                    loopMain e;
                                    --print (foldlWithKey (\a  k  b ->  show b <> a) " " gh);
                                    let (EnCours (Etat nvv aa nbb bb nmm nns)) =gameLoop e in
                                    
                                    return ()
-}
loopMain :: Situation -> IO()
loopMain s 
    |show s == "Perdu" = print s
    |show s == "Gagné" = print s
    |otherwise = print s

gameLoop :: Situation -> Situation
gameLoop Perdu = Perdu
gameLoop Gagne = Gagne
gameLoop s@(EnCours e@(Etat _ _ nb _ nm ns))
    | nm == nb = Perdu
    | ns == nb = Gagne
    | otherwise = (transformeEtat s)