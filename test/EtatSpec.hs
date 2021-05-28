{-# LANGUAGE FlexibleInstances #-}
module EtatSpec where

import Test.Hspec
import Test.QuickCheck

import Etat
import Niveau
import Lemmings
import Coord

prop_initEtat :: Niveau -> Property
prop_initEtat c  =  prop_pre_makeEtat c
    ==>
    property $ prop_etat_inv (makeEtat c) 

genNiveau:: Gen Niveau 
genNiveau = do
    return (read exempleNiveau )

instance Arbitrary Niveau  where
    arbitrary =
            frequency [(10,genNiveau)] 

initEtat = do
  describe "initEtat" $ do
    it "preserves the invariant initEtat" $ property $ \c -> prop_initEtat c 



prop_tourEtat_inv :: Int  -> Etat -> Property
prop_tourEtat_inv i  e =
   prop_pre_tourEtat i e
  ==> classify (i >= 0) "n >=0" $
  property $ prop_post_tourEtat i e
        

genCoordOk :: Gen Coord
genCoordOk = do
        x <- choose((0,9) :: (Int, Int))
        y <- choose((0,11) :: (Int,Int))
        return $ initCoord x y 

instance Arbitrary Coord where
    arbitrary =
        frequency [(10, genCoordOk) -- 20% de génération libre
              ]     -- 80% de génération sûre

genName = vectorOf 6 (choose ('A','Z'))

genDirection :: Gen Direction 
genDirection = do
    c <- genName
    case c of
        "GAUCHE" -> return Gauche
        "DROITE" -> return Droite 
        _ -> return Gauche

instance Arbitrary Direction where
    arbitrary =
        frequency [(10, genDirection) 
              ]  

genLemming :: Gen Lemming 
genLemming = do
    d <- genDirection
    c <- genCoordOk
    return $ Marcheur d c False

instance Arbitrary Lemming where
    arbitrary =
        frequency [(10, genLemming) 
              ]  

genEtat :: Gen Etat 
genEtat = do

    return $ ajouterLemming (makeEtat (read exempleNiveau) ) 

instance Arbitrary Etat where
    arbitrary =
        frequency [(10, genEtat) 
              ]  

tourEtatSpec = do
  describe "tourEtat" $ do
    it "preserves the invariant tourEtat" $
      property prop_tourEtat_inv