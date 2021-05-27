import Test.Hspec

import CoordSpec as CS 

main :: IO ()
main = hspec $ do
    CS.coordSpecGenOK
    CS.deplacementBougeCoordSpec
