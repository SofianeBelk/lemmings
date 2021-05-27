module Moteur where
import qualified Data.Map as Map
import Control.Concurrent
import System.Console.ANSI
import Etat
import Niveau
import Lemmings
import Coord

tourMoteur :: Int -> Etat -> IO(Either Fin (Int,Etat))
tourMoteur n e = case tourEtat n e of
                    Left f -> do
                                print f
                                return  $ Left f
                    Right ne -> do
                                    print ne
                                    putStrLn ("Tour "<> show n <> " restants : "<> show (nbLemmingsRestants ne) <> ", vivants : "<> show (nbLemmingsVivants ne)<>", sortis : "<> show (nbLemmingsSortis ne))
                                    putStrLn ""
                                    threadDelay 25000
                                    clearScreen
                                    return $ Right (n+1, ne)

tourne :: Int -> Etat -> IO String 
tourne n e = do
                    e1 <- tourMoteur n e
                    case e1 of
                        Left f -> return "Fini"
                        Right (i, n) -> tourne i n

lance :: Etat -> IO String
lance = tourne 0