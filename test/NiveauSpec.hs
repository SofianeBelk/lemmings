module NiveauSpec where

import Test.Hspec
import Test.QuickCheck

import Niveau 

prop_readNiveau_inv ::  String -> Property 
prop_readNiveau_inv chaine =
    (prop_PreReadNiveau chaine) && (prop_post_readNiveau chaine)
    ==>  property $ prop_niveau_inv (readNiveau chaine)

