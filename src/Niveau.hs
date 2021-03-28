module Niveau where

import qualified Data.Map as Map
import qualified Data.List as List

import Coord

data Case = Metal | Terre | Entree | Sortie | Vide
    deriving Eq

instance Show Case where
    show Vide   =  " "
    show Metal  =  "X"
    show Terre  =  "∅"
    show Entree =  "E"
    show Sortie =  "S"

instance Read Case where
    readsPrec _ x = [(lectureCase x ,"")]

lectureCase :: String -> Case
lectureCase " "= Vide
lectureCase "X"= Metal
lectureCase "∅"= Terre
lectureCase "E"= Entree
lectureCase "S"= Sortie
lectureCase _= Vide

data Niveau = Niveau {
    hNiveau :: Int,
    lNibeau :: Int,
    casesNiveau :: Map.Map Coord Case} 
    deriving Eq

