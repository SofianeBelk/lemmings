module LemmingsSpec where

import Test.Hspec
import Test.QuickCheck

import Lemmings
import Coord

prop_initLemmingMort_inv :: Coord -> Property
prop_initLemmingMort_inv c =
    property $ prop_lemming_inv (makeMort c)

prop_initLemmingMarcheur_inv :: Direction -> Coord -> Property
prop_initLemmingMarcheur_inv d c =
    property $ prop_lemming_inv (makeMarcheur d c)


prop_initLemmingCreuseur_inv :: Direction -> Int -> Coord -> Property
prop_initLemmingCreuseur_inv d i c = prop_pre_makeCreuseur d i c
    ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeCreuseur d i c)

prop_initLemmingConstructeur_inv :: Direction -> Int -> Coord -> Property
prop_initLemmingConstructeur_inv d i c = prop_pre_makeConstructeur d i c
    ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeConstructeur d i c)


prop_initLemmingExploseur_inv :: Int -> Coord -> Property
prop_initLemmingExploseur_inv i c = prop_pre_makeExploseur i c
    ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeExploseur i c)


prop_initLemmingBloqueur_inv ::  Direction -> Int -> Coord -> Property
prop_initLemmingBloqueur_inv d i c = prop_pre_makeBloqueur d i c
     ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeBloqueur d i c)


prop_initLemmingBoucheur_inv ::  Direction -> Int -> Coord -> Property
prop_initLemmingBoucheur_inv d i c = prop_pre_makeBoucheur d i c
    ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeBoucheur d i c)


prop_initLemmingTombeur_inv ::  Direction -> Int -> Coord -> Property
prop_initLemmingTombeur_inv d i c = prop_pre_makeTombeur d i c
    ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeTombeur d i c)


prop_initLemmingBrule_inv ::  Int -> Coord -> Property
prop_initLemmingBrule_inv i c = prop_pre_makeBrule i c
    ==> classify (i>=0) "valeurs >+ 0" $
    property $ prop_lemming_inv (makeBrule i c)


prop_initLemmingDemineur_inv :: Direction -> Coord -> Property
prop_initLemmingDemineur_inv d c =
    property $ prop_lemming_inv (makeDemineur d c)

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

-- Direction
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

initLemmingMort = do
  describe "initLemmingMort" $ do
    it "preserves the invariant initLemmingMort" $ property $ \c -> prop_initLemmingMort_inv c

initLemmingMarcheur = do
  describe "initLemmingMarcheur" $ do
    it "preserves the invariant initLemmingMarcheur" $ property $ \d c -> prop_initLemmingMarcheur_inv d c

initLemmingCreuseur = do
  describe "initLemmingCreuseur" $ do
    it "preserves the invariant initLemmingCreuseur" $ property $ \d i c -> prop_initLemmingCreuseur_inv d i  c

initLemmingConstructeur = do
  describe "initLemmingConstructeur" $ do
    it "preserves the invariant initLemmingConstructeur" $ property $ \d i c -> prop_initLemmingConstructeur_inv d i c

initLemmingExploseur = do
  describe "initLemmingExploseur" $ do
    it "preserves the invariant initLemmingExploseur" $ property $ \d i  -> prop_initLemmingExploseur_inv d i

initLemmingBloqueur = do
  describe "initLemmingBloqueur" $ do
    it "preserves the invariant initLemmingBloqueur" $ property $ \d i c -> prop_initLemmingBloqueur_inv d i c

initLemmingBoucheur = do
  describe "initLemmingBoucheur" $ do
    it "preserves the invariant initLemmingBoucheur" $ property $ \d i c -> prop_initLemmingBoucheur_inv d i c

initLemmingTombeur = do
  describe "initLemmingTombeur" $ do
    it "preserves the invariant initLemmingTombeur" $ property $ \d i c -> prop_initLemmingTombeur_inv d i c

initLemmingBrule = do
  describe "initLemmingBrule" $ do
    it "preserves the invariant initLemmingBrule" $ property $ \d i  -> prop_initLemmingBrule_inv d i

initLemmingDemineur = do
  describe "initLemmingDemineur" $ do
    it "preserves the invariant initLemmingDemineur" $ property $ \d  c -> prop_initLemmingDemineur_inv d  c