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
bougeCoord N c = c
bougeCoord G c = gauche c
bougeCoord D c = droite c
bougeCoord H c = haut c
bougeCoord B c = bas c
bougeCoord GH c = haut (gauche c)
bougeCoord GB c = bas (gauche c)
bougeCoord DH c = haut (droite c)
bougeCoord DB c = bas (droite c)

bas :: Coord -> Coord
bas (C x y) = C x (y-1)

haut :: Coord -> Coord
haut (C x y) = C x (y+1)

gauche :: Coord -> Coord
gauche (C x y) = C (x-1) y

droite :: Coord -> Coord
droite (C x y) = C (x+1) y

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
        bougeP :: Deplacement -> a -> a
        deplaceP :: Coord -> a -> a

prop_placableLaw :: (Placable a , Eq a)=> a -> Bool
prop_placableLaw v = let (C x y) = coordP v in
                        bougeP G v == deplaceP (C (x-1) y) v
                        &&
                        bougeP D v == deplaceP (C (x+1) y) v
                        &&
                        bougeP H v == deplaceP (C x (y+1)) v
                        &&
                        bougeP B v == deplaceP (C x (y-1)) v
                        &&
                        bougeP GH v == deplaceP (C (x-1) (y+1)) v
                        &&
                        bougeP GB v == deplaceP (C (x-1) (y-1)) v
                        &&
                        bougeP DH v == deplaceP (C (x+1) (y+1)) v
                        &&
                        bougeP DB v == deplaceP (C (x+1) (y-1)) v
