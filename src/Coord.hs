module Coord where

data Coord = C Int Int
            deriving (Eq, Show)


data Deplacement = N | G | D | H | B | GH | GB | DH | DB
                   deriving (Eq, Show)


bougeCoord :: Deplacement -> Coord -> Coord
bougeCoord N (C x y) = C x y
bougeCoord G (C x y) = C (x-1) y
bougeCoord D (C x y) = C (x+1) y
bougeCoord H (C x y) = C x (y+1)
bougeCoord B (C x y) = C x (y-1)
bougeCoord GH (C x y) = C (x-1) (y+1)
bougeCoord GB (C x y) = C (x-1) (y-1)
bougeCoord DH (C x y) = C (x+1) (y+1)
bougeCoord DB (C x y) = C (x+1) (y-1)

instance Ord Coord where  
        (<=) (C x1 y1) (C x2 y2) = (y1 > y2) || (y1==y2 && x1<=x2)


prop_bougeCoordGaucheDroite :: Coord -> Bool
prop_bougeCoordGaucheDroite (C x y) =   bougeCoord D (bougeCoord G (C x y)) == C x y

class Placable a where
        coordP :: a -> Coord
        bougeP :: a -> Deplacement -> a
        deplaceP :: a -> Coord -> a 

prop_placableLaw :: (Placable a , Eq a)=> a  -> Bool
prop_placableLaw v = let (C x y) = coordP v in
                        bougeP v G == deplaceP v (C (x-1) y) 
 