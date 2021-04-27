module Coord where

data Coord = C Int Int
            deriving Eq

prop_coordInv :: Coord -> Bool                  
prop_coordInv (C x y) = x >= 0 && y >= 0

-- >>> prop_coordInv (C 0 0)
-- True
-- >>> prop_coordInv (C 3 5)
-- True
-- >>> prop_coordInv (C (-1) 3)
-- False
-- >>> prop_coordInv (C 0 (-1))
-- False

instance Show Coord where
        show (C x y)= "("++show x++","++show y++")"

instance Ord Coord where  
        (<=) (C x1 y1) (C x2 y2) = (y1 > y2) || (y1==y2 && x1<=x2)

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

prop_bougeCoordDroitGauche :: Coord -> Bool
prop_bougeCoordDroitGauche (C x y) =   bougeCoord G (bougeCoord D (C x y)) == C x y
prop_bougeCoordGaucheDroite :: Coord -> Bool
prop_bougeCoordGaucheDroite (C x y) =   bougeCoord D (bougeCoord G (C x y)) == C x y
prop_bougeCoordGaucheHaut :: Coord -> Bool
prop_bougeCoordGaucheHaut (C x y) =   bougeCoord H (bougeCoord G (C x y)) == bougeCoord GH (C x y)
prop_bougeCoordDroiteBas :: Coord -> Bool
prop_bougeCoordDroiteBas (C x y) =   bougeCoord B (bougeCoord D (C x y)) == bougeCoord DB (C x y)

-- >>> prop_bougeCoordDroitGauche (C 4 5)
-- True
-- >>> prop_bougeCoordGaucheDroite (C 4 1)
-- True
-- >>> prop_bougeCoordGaucheHaut (C 3 2)
-- True
-- >>> prop_bougeCoordDroiteBas (C 5 4)
-- True

class Placable a where
        coordP :: a -> Coord
        bougeP :: a -> Deplacement -> a
        deplaceP :: a -> Coord -> a 

prop_placableLaw :: (Placable a , Eq a)=> a -> Bool
prop_placableLaw v = let (C x y) = coordP v in
                        bougeP v G == deplaceP v (C (x-1) y)
                        &&
                        bougeP v D == deplaceP v (C (x+1) y)
                        &&
                        bougeP v H == deplaceP v (C x (y+1))
                        &&
                        bougeP v B == deplaceP v (C x (y-1))
                        &&
                        bougeP v GH == deplaceP v (C (x-1) (y+1))
                        &&
                        bougeP v GB == deplaceP v (C (x-1) (y-1))
                        &&
                        bougeP v DH == deplaceP v (C (x+1) (y+1))
                        &&
                        bougeP v DB == deplaceP v (C (x+1) (y-1))