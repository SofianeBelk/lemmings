module Main where
import Control.Monad
import Control.Concurrent
import Moteur
import Niveau
import Etat
import Data.Map as Map
import Data.Int
import System.Environment
import Coord
import Foreign.C.Types

import Data.Key
import qualified Data.Key as Key

import Data.Maybe
import qualified Data.Maybe as Maybe

import SDL

import TextureMap
import qualified TextureMap as TM

import Sprite 
import qualified Sprite as S

import SpriteMap
import qualified SpriteMap as SM

import Environnement

niv :: Niveau
niv = exempleNiveau

env :: Environnement
env = makeEnvironnement (hNiveau niv) (lNiveau niv)

etat :: IO Etat
etat = return (Etat env niv 6 0 0)

mapFileParse :: [String] -> IO Niveau
mapFileParse [] = error "File name missing"
mapFileParse (filename:_) = do
    content <- readFile filename
    return (fst $ head (reads content))

loadCase :: String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadCase str rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId str) tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId str ) (S.mkArea 0 0 50 50)
    let smap' = SM.addSprite (SpriteId str) sprite smap
    return (tmap', smap')

main :: IO ()
main = do
    args <- getArgs
    niveau <- mapFileParse args
    let map = casesNiveau niveau
    let tileSize = 50
    let h = CInt(fromIntegral (tileSize * (hNiveau niveau+1)) :: Int32)
    let l = CInt(fromIntegral (tileSize * lNiveau niveau) :: Int32)


    initializeAll 
    window <- createWindow "Lemmings" $ defaultWindow {windowInitialSize = V2 h l}
    rdr <- createRenderer window (-1) defaultRenderer
    (tmap, smap) <- loadCase "X" rdr "assets/metal.bmp" TM.createTextureMap SM.createSpriteMap
    (tmap, smap) <- loadCase "0" rdr "assets/dirt.bmp" tmap smap
    (tmap, smap) <- loadCase "E" rdr "assets/enter.bmp" tmap smap
    (tmap, smap) <- loadCase " " rdr "assets/empty.bmp" tmap smap
    (tmap, smap) <- loadCase "S" rdr "assets/exit.bmp" tmap smap
    gameLoop (l,h) 50 60 (makeEtat niveau 6) rdr tmap smap 0

gagne :: Either Fin Etat -> Bool
gagne e = case e of
        Left f -> case f of
                    Victoire _ -> True
                    _ -> False
        _ -> False 

perdu :: Either Fin Etat -> Bool
perdu e = case e of
        Left f -> case f of
                    Defaite -> True
                    _ -> False
        _ -> False

enCours :: Either Fin Etat -> Bool
enCours e = case e of
            Left f -> False
            _ -> True

getEtat :: Either Fin Etat -> Maybe Etat
getEtat e = case e of
                Right etat -> Just etat
                Left f -> Nothing


gameLoop :: (RealFrac a, Show a) => (CInt,CInt) -> Int -> a -> Etat -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
gameLoop dimensions tileSize frameRate etat renderer tmap smap nb_tours = do
    startTime <- time
    clear renderer
    let map = casesNiveau $ niveauE etat
    let (width,height) = dimensions

    let cells = (\(C x y) c -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show c)) smap) (fromIntegral (x*tileSize)) (fromIntegral (y*tileSize))))
    Key.mapWithKeyM_ cells map
    present renderer
    endTime <- time
    let refreshTime = endTime - startTime

    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    threadDelay $ delayTime * 1000000
    let newEtat = tourEtat nb_tours etat
    let etat' = Maybe.fromJust (getEtat newEtat)
    if gagne newEtat then print "GAGNE" else when (perdu newEtat) $ print "PERDU"
    unless (enCours newEtat) (gameLoop dimensions tileSize frameRate etat' renderer tmap smap (nb_tours + 1))



-- main = etat >>= lance >> return ()




{-

loadLemming :: String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadLemming lem rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId lem) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId lem) (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId lem) sprite smap
  return (tmap', smap')

main :: IO ()
main =  do
    initializeAll
    window <- createWindow "Lemmings" $ defaultWindow {windowInitialSize = V2 640 480}
    renderer <- createRenderer window (-1) defaultRenderer

    (t, s) <- loadCase "X" renderer "assets/metal.png" TM.createTextureMap SM.createSpriteMap
    (t', s') <- loadCase "0" renderer "assets/dirt.png" t s
    (t'', s'') <- loadCase "E" renderer "assets/enter.png" t' s'
    (t''', s''') <- loadCase " " renderer "assets/empty.png" t'' s''
    (tmap, smap) <- loadCase "S" renderer "assets/exit.png" t''' s'''
    let (EnCours (Etat niv _ _ _ _ _)) = e in
        loadNiveau niv renderer tmap smap


gameLoop :: Situation -> Int -> IO()
gameLoop Perdu _ = print Perdu
gameLoop Gagne _ = print Gagne
gameLoop s@(EnCours e@(Etat _ _ nb _ nm ns)) 1
    | nm == nb = print Perdu
    | ns == nb = print Gagne
    | otherwise = do
         print s
         gameLoop (transformeSituation s) 1
gameLoop s@(EnCours e@(Etat _ _ nb _ nm ns)) n
    | nm == nb = print Perdu
    | ns == nb = print Gagne
    | otherwise = do
         print s
         gameLoop (transformeSituation (introduireLemming s)) (n -1)-}
{-
niveauFileParse :: [String] -> IO Niveau
niveauFileParse [] = error "File name missing"
niveauFileParse (file:_) = do
    content <- readFile file
    return (fst $ head (reads content))

loadCase :: Int -> String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadCase tileSize name rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId name) tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId name ) (S.mkArea 0 0 50 50)
    let smap' = SM.addSprite (SpriteId name) sprite smap
    return (tmap', smap')

main :: IO ()
main = do
    args <- getArgs
    map <- niveauFileParse args
    let m = casesNiveau map
    let tileSize = 50
    let hauteur = CInt (fromIntegral (tileSize * hNiveau map + 10) :: Int32)
    let largeur = CInt (fromIntegral (tileSize * lNiveau map) :: Int32)
    let initEtat = makeEtat map
    let initMoteur = makeSituation map
    let initMoteur = introduireLemming initMoteur

    initializeAll
    window <- createWindow "Lemmings" $ defaultWindow { windowInitialSize = V2 largeur hauteur}
    renderer <- createRenderer window (-1) defaultRenderer
    (tmap, smap) <- loadCase tileSize "X" renderer "assets/metal.bmp" TM.createTextureMap SM.createSpriteMap
    (tmap, smap) <- loadCase tileSize "0" renderer "assets/dirt.bmp" tmap smap
    (tmap, smap) <- loadCase tileSize "E" renderer "assets/enter.bmp" tmap smap
    (tmap, smap) <- loadCase tileSize " " renderer "assets/empty.bmp" tmap smap
    (tmap, smap) <- loadCase tileSize "S" renderer "assets/exit.bmp" tmap smap
    gameLoop (largeur, hauteur) 50 70 initMoteur renderer tmap smap 0
    print "OK"

gameLoop :: (CInt, CInt) -> Int -> a -> Situation -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
gameLoop dimensions tileSize frameRate situation rdr tmap smap nb_tours = do
    startTime <- time

    clear rdr
    let map = casesNiveau $ getNiveau situation
    let env = getEtat situation
    let caseToSprite = (\(C x y) c -> S.displaySprite rdr tmap (S.moveTo (SM.fetchSprite (SpriteId (show (c :: Case))) smap) (fromIntegral (x*tileSize)) (fromIntegral (y*tileSize))))

    let(largeur, hauteur) = dimensions
    
    Key.mapWithKeyM_ caseToSprite map
    present rdr
    {-endTime <- time
    let refreshTime = endTime - startTime
    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    threadDelay $ delayTime * 1000
    endTime <- time-}
    let newSituation = transformeSituation situation
    when (gagne situation || perdu situation) (print "FINI")
    unless True (gameLoop dimensions tileSize frameRate situation rdr tmap smap nb_tours)


    -}