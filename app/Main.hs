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

import Data.Sequence
import qualified Data.Sequence as Seq

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

loadAsset :: String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadAsset str rdr path tmap smap = do
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
    window <- createWindow "Lemmings" $ defaultWindow {windowInitialSize = V2 l h}
    rdr <- createRenderer window (-1) defaultRenderer
    (tmap, smap) <- loadAsset " " rdr "assets/empty.bmp" TM.createTextureMap SM.createSpriteMap
    (tmap, smap) <- loadAsset "X" rdr "assets/metal.bmp" tmap smap
    (tmap, smap) <- loadAsset "0" rdr "assets/dirt.bmp" tmap smap
    (tmap, smap) <- loadAsset "E" rdr "assets/enter.bmp" tmap smap
    (tmap, smap) <- loadAsset "S" rdr "assets/exit.bmp" tmap smap

    (tmap, smap) <- loadAsset ">" rdr "assets/lemming_walk_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset ">0" rdr "assets/lemming_walk_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset ">1" rdr "assets/lemming_walk_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset ">2" rdr "assets/lemming_walk_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset ">3" rdr "assets/lemming_walk_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset ">4" rdr "assets/lemming_walk_r/4.bmp" tmap smap
    (tmap, smap) <- loadAsset ">5" rdr "assets/lemming_walk_r/5.bmp" tmap smap
    (tmap, smap) <- loadAsset ">6" rdr "assets/lemming_walk_r/6.bmp" tmap smap
    (tmap, smap) <- loadAsset ">7" rdr "assets/lemming_walk_r/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "<" rdr "assets/lemming_walk_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "<0" rdr "assets/lemming_walk_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "<1" rdr "assets/lemming_walk_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "<2" rdr "assets/lemming_walk_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "<3" rdr "assets/lemming_walk_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "<4" rdr "assets/lemming_walk_l/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "<5" rdr "assets/lemming_walk_l/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "<6" rdr "assets/lemming_walk_l/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "<7" rdr "assets/lemming_walk_l/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "V" rdr "assets/lemming_fall_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "V0" rdr "assets/lemming_fall_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "V1" rdr "assets/lemming_fall_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "V2" rdr "assets/lemming_fall_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "V3" rdr "assets/lemming_fall_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "V4" rdr "assets/lemming_fall_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "V5" rdr "assets/lemming_fall_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "V6" rdr "assets/lemming_fall_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "V7" rdr "assets/lemming_fall_l/3.bmp" tmap smap

    (tmap, smap) <- loadAsset "v" rdr "assets/lemming_fall_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "v0" rdr "assets/lemming_fall_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "v1" rdr "assets/lemming_fall_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "v2" rdr "assets/lemming_fall_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "v3" rdr "assets/lemming_fall_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "v4" rdr "assets/lemming_fall_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "v5" rdr "assets/lemming_fall_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "v6" rdr "assets/lemming_fall_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "v7" rdr "assets/lemming_fall_r/3.bmp" tmap smap

    (tmap, smap) <- loadAsset "+" rdr "assets/empty.bmp" tmap smap

    gameLoop (l,h) tileSize 60 (makeEtat niveau 6) rdr tmap smap 0

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
    print etat
    startTime <- time
    clear renderer
    let map = casesNiveau $ niveauE etat
    let lems = casesEnvironnement $ enviE etat

    let (width,height) = dimensions

    let cells = (\(C x y) c -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show c)) smap) (fromIntegral (x*tileSize)) (fromIntegral ((hNiveau (niveauE etat) - y)*tileSize))))
    let lemmings = (\(C x y) se -> if not (Seq.null se) then
                                    S.displaySprite renderer tmap (S.moveTo
                                    (SM.fetchSprite (SpriteId (show (Maybe.fromJust (Seq.lookup 0 se)) <> show (nb_tours `mod` 8))) smap)
                                    (fromIntegral (x*tileSize)) (fromIntegral ((hNiveau (niveauE etat) - y)*tileSize)))
                                    else
                                        S.displaySprite renderer tmap (S.moveTo
                                            (SM.fetchSprite (SpriteId " ") smap)
                                                (fromIntegral (x*tileSize)) (fromIntegral((hNiveau (niveauE etat) - y)*tileSize))))

    Key.mapWithKeyM_ cells map
    Key.mapWithKeyM_ lemmings lems
    present renderer
    endTime <- time
    let refreshTime = endTime - startTime

    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    threadDelay $ delayTime * 100000
    let newEtat = tourEtat nb_tours etat

    if gagne newEtat then do
        print "GAGNE"
        return ()
        else
            if perdu newEtat then do
                print "PERDU"
                return ()
                else
                    let etat' = Maybe.fromJust (getEtat newEtat) in
    --unless (enCours newEtat)
                        gameLoop dimensions tileSize frameRate etat' renderer tmap smap (nb_tours + 1)
    return ()


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

    (t, s) <- loadAsset "X" renderer "assets/metal.png" TM.createTextureMap SM.createSpriteMap
    (t', s') <- loadAsset "0" renderer "assets/dirt.png" t s
    (t'', s'') <- loadAsset "E" renderer "assets/enter.png" t' s'
    (t''', s''') <- loadAsset " " renderer "assets/empty.png" t'' s''
    (tmap, smap) <- loadAsset "S" renderer "assets/exit.png" t''' s'''
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

loadAsset :: Int -> String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadAsset tileSize name rdr path tmap smap = do
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
    (tmap, smap) <- loadAsset tileSize "X" renderer "assets/metal.bmp" TM.createTextureMap SM.createSpriteMap
    (tmap, smap) <- loadAsset tileSize "0" renderer "assets/dirt.bmp" tmap smap
    (tmap, smap) <- loadAsset tileSize "E" renderer "assets/enter.bmp" tmap smap
    (tmap, smap) <- loadAsset tileSize " " renderer "assets/empty.bmp" tmap smap
    (tmap, smap) <- loadAsset tileSize "S" renderer "assets/exit.bmp" tmap smap
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