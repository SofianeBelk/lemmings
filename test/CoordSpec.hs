module CoordSpec where

import Test.Hspec
import Test.QuickCheck

import Coord


genCoordOk :: Gen Coord
genCoordOk = do
        x <- choose((0,100) :: (Int, Int))
        y <- choose((0,100) :: (Int,Int))
        return $ initCoord x y 

genCoordFree :: Gen Coord
genCoordFree = do
        x <- choose(100,1000)
        y <- choose(100,1000)
        return $ initCoord x y 

prop_genCoordOk_inv :: Property
prop_genCoordOk_inv = forAll genCoordOk $ prop_coord_inv

instance Arbitrary Coord where
    arbitrary =
        frequency [(2, genCoordFree) -- 20% de génération libre
              , (8, genCoordOk)]     -- 80% de génération sûre

-- Deplacement
genDeplacementOne :: Gen Deplacement 
genDeplacementOne = do
    c <- choose (('A', 'Z') ::(Char, Char))
    case c of
        'N' -> return N
        'G' -> return G
        'D' -> return D
        'H' -> return H 
        'B' -> return B 
        _ -> return N

genDeplacement = vectorOf 2 (choose ('A','Z'))

genDeplacementTwo :: Gen Deplacement 
genDeplacementTwo = do
    dep <- genDeplacement
    case dep of
        "GH" -> return GH 
        "GB" -> return GB
        "DB" -> return DB
        "DH" -> return DH
        _ -> return N

instance Arbitrary Deplacement where
    arbitrary =
        frequency [(5, genDeplacementOne) 
              , (5, genDeplacementTwo)]     

-- quickCheck prop_genCoordOk_inv

coordSpecGenOK = do
    describe "Génération des coord" $ do
        it "Génération des coordonnées qui satisfait l'invariant" $
            property prop_genCoordOk_inv

prop_deplacementBougeCoord_inv :: Coord -> Deplacement ->Property
prop_deplacementBougeCoord_inv c@(C x y) d  =
    prop_coord_inv c
    ==> classify ((x < 10) && (y < 10) && (x>0) && (y>0)) "small coord" $
        classify ((x < 50) && (y < 50) && (x>0) && (y>0)) "meduim coord" $
        classify ((x <= 100) && (y <= 100) && (x>0) && (y>0)) "large coord" $
        property $ prop_coord_inv (bougeCoord d c)


deplacementBougeCoordSpec = do 
    describe "BougeCoord" $ do 
        it "Préserver l'invariant" $
            property $  prop_deplacementBougeCoord_inv 



