module Lemmings where

import  Coord

data Direction = L | R
                deriving(Eq, Show)


data Lemming = Mort Coord
              | Marcheur Direction Coord 
              | Tombeur Direction Int Coord
              deriving (Eq)
            
instance Show Lemming where 
    show (Mort _) = "+"
    show (Marcheur R _) = ">"
    show (Marcheur L _) = "<"
    show Tombeur {} = "V"
