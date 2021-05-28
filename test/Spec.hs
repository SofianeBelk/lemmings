import Test.Hspec

import CoordSpec as CS 
import NiveauSpec as NS

import LemmingsSpec as LS
import EtatSpec as ES

main :: IO ()
main = hspec $ do
    CS.coordSpecGenOK
    CS.deplacementBougeCoordSpec
    NS.supprimerCaseSpec
    NS.poserCaseSpec
    NS.activerMineSpec
    NS.desactiverMineSpec
    NS.exploserCaseSpec
    NS.bloquerSpec
    NS.debloquerSpec
    LS.initLemmingBloqueur 
    LS.initLemmingMort
    LS.initLemmingMarcheur 
    LS.initLemmingBrule 
    LS.initLemmingDemineur 
    LS.initLemmingExploseur
    LS.initLemmingCreuseur 
    LS.initLemmingTombeur 
    LS.initLemmingConstructeur 
    ES.initEtat
    ES.tourEtatSpec
