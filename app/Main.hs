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

env :: Envi
env = envide (hNiveau niv) (lNiveau niv)

etat :: IO Etat
etat = return (Etat env niv 6 0 0)

main :: IO ()
main = etat >>= lance >> return ()











{-
e :: Situation
e = gameInit exempleNiveau
-}
{-loadMetal :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMetal rdr path tmap smap = do
    tmap' <- TM.loadNiveau rdr path (TextureId "metal") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "metal" ) (S.mkArea 0 0 640 480)
    let smap' = SM.addSprite (SpriteId "metal") sprite smap
    return (tmap', smap')

loadTerre :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadTerre rdr path tmap smap = do
    tmap' <- TM.loadNiveau rdr path (TextureId "terre") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "terre" ) (S.mkArea 0 0 640 480)
    let smap' = SM.addSprite (SpriteId "terre") sprite smap
    return (tmap', smap')

loadEntree :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadEntree rdr path tmap smap = do
    tmap' <- TM.loadNiveau rdr path (TextureId "entree") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "entree" ) (S.mkArea 0 0 640 480)
    let smap' = SM.addSprite (SpriteId "entree") sprite smap
    return (tmap', smap')

loadSortie :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSortie rdr path tmap smap = do
    tmap' <- TM.loadNiveau rdr path (TextureId "sortie") tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "sortie" ) (S.mkArea 0 0 640 480)
    let smap' = SM.addSprite (SpriteId "sortie") sprite smap
    return (tmap', smap')
loadNiveau :: Niveau -> Renderer -> TextureMap -> SpriteMap  -> IO()
loadNiveau (Niveau l h cds) rdr tm sm = Map.foldWithKey (\(C x y) c acc -> S.displaySprite rdr tm (S.moveTo (TM.fetchTexture (show c) x y))) cds

loadCase :: String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadCase c rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId c) tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId c ) (S.mkArea 0 0 640 480)
    let smap' = SM.addSprite (SpriteId c) sprite smap
    return (tmap', smap')

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