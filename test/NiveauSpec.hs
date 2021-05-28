module NiveauSpec where

import Test.Hspec
import Test.QuickCheck

import Niveau 
import Coord


prop_supprimerCase_inv :: Coord -> Niveau -> Property
prop_supprimerCase_inv c n =
    prop_coord_inv c && prop_niveau_inv n && prop_pre_supprimerCase c n
    ==> property $ prop_post_supprimerCase c n

prop_poserCase_inv ::Coord -> Niveau -> Property
prop_poserCase_inv c n =
    prop_coord_inv c && prop_niveau_inv n && prop_pre_poserCase c n
    ==> property $ prop_post_poserCase c n


prop_activerMine_inv :: Coord -> Niveau -> Property
prop_activerMine_inv c n =
    prop_coord_inv c && prop_niveau_inv n && prop_pre_activerMine c n
    ==> property $ prop_post_activerMine c n

prop_desactiverMine_inv ::Coord -> Niveau -> Property
prop_desactiverMine_inv c n =
    prop_coord_inv c && prop_niveau_inv n && prop_pre_desactiverMine c n
    ==> property $ prop_post_desactiverMine c n

prop_exploserCase_inv ::Coord -> Niveau -> Property
prop_exploserCase_inv c n =
    prop_coord_inv c && prop_niveau_inv n 
    ==> property $ prop_post_exploserCase c n

prop_bloquer_inv ::Coord -> Niveau -> Property
prop_bloquer_inv c n =
    prop_coord_inv c && prop_niveau_inv n && prop_pre_bloquer c n
    ==> property $ prop_post_bloquer c n


prop_debloquer_inv ::Coord -> Niveau -> Property
prop_debloquer_inv c n =
    prop_coord_inv c && prop_niveau_inv n && prop_pre_debloquer c n
    ==> property $ prop_post_debloquer c n


genCoordOk :: Gen Coord
genCoordOk = do
        x <- choose((0,5) :: (Int, Int))
        y <- choose((0,6) :: (Int,Int))
        return $ initCoord x y 

genCoordFree :: Gen Coord
genCoordFree = do
        x <- choose(6,9)
        y <- choose(7,11)
        return $ initCoord x y 

genCoordRandom :: Gen Coord
genCoordRandom = do
        x <- choose(0,5)
        y <- choose(7,11)
        return $ initCoord x y 


genCoordUltra :: Gen Coord
genCoordUltra = do
        x <- choose(6,9)
        y <- choose(0,6)
        return $ initCoord x y 
instance Arbitrary Coord where
    arbitrary =
        frequency [(0, genCoordFree) -- 20% de génération libre
              , (2, genCoordOk), (3,genCoordRandom), (0,genCoordUltra)]     -- 80% de génération sûre


genNiveau:: Gen Niveau 
genNiveau = do
    return (read exempleNiveau )

instance Arbitrary Niveau  where
    arbitrary =
            frequency [(10,genNiveau)]     -- 100% de génération sûre

supprimerCaseSpec = do
  describe "supprimerCase" $ do
    it "preserves the invariant supprimer case" $
      property prop_supprimerCase_inv


poserCaseSpec = do
  describe "poserCase" $ do
    it "preserves the invariant poser case" $
      property prop_poserCase_inv


activerMineSpec = do
  describe "activerMine" $ do
    it "preserves the invariant activer mine" $
      property prop_activerMine_inv

desactiverMineSpec = do
  describe "desactiver Mine" $ do
    it "preserves the invariant desactiver Mine" $
      property prop_desactiverMine_inv

exploserCaseSpec = do
  describe "exploserCase" $ do
    it "preserves the invariant exploser case" $
      property prop_exploserCase_inv

bloquerSpec = do
  describe "bloquer" $ do
    it "preserves the invariant bloquer " $
      property prop_bloquer_inv


debloquerSpec = do
  describe "supprimerCase" $ do
    it "preserves the invariant supprimer case" $
      property prop_debloquer_inv