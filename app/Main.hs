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
import Data.StateVar
import Environnement
import Data.Maybe
import qualified Data.Maybe as Maybe
import Data.Sequence
import qualified Data.Sequence as Seq
import SDL
import Mouse (Mouse)
import qualified Mouse as M
import Keyboard (Keyboard)
import qualified Keyboard as K
import TextureMap
import qualified TextureMap as TM
import Sprite
import qualified Sprite as S
import SpriteMap
import qualified SpriteMap as SM

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

loadAsset :: String -> Int -> Int -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadAsset str tileSizeX tileSizeY rdr path tmap smap = do
    tmap' <- TM.loadTexture rdr path (TextureId str) tmap
    let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId str ) (S.mkArea 0 0 (fromIntegral tileSizeX) (fromIntegral tileSizeY))
    let smap' = SM.addSprite (SpriteId str) sprite smap
    return (tmap', smap')

main :: IO ()
main = do
    args <- getArgs
    niveau <- mapFileParse args
    let map = casesNiveau niveau

    initializeAll
    window <- createWindow "Lemmings" $ defaultWindow {windowInitialSize = V2 0 0}
    SDL.setWindowMode window SDL.FullscreenDesktop

    V2 xw yw <- get (windowSize window)

    let tileSizeX = fromIntegral xw `div` lNiveau niveau
    let tileSizeY = fromIntegral yw `div` hNiveau niveau
    let h = CInt(fromIntegral (tileSizeY * hNiveau niveau) :: Int32)
    let l = CInt(fromIntegral (tileSizeX * lNiveau niveau) :: Int32)

    rdr <- createRenderer window (-1) defaultRenderer
    (tmap, smap) <- loadAssets tileSizeX tileSizeY rdr

    let kbd = K.createKeyboard
    let ms = M.createMouse
    gameLoop (l,h) (-1) tileSizeX tileSizeY 60 (makeEtat niveau) rdr tmap smap ms kbd 0

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

abandone :: Either Fin Etat -> Bool
abandone e = case e of
        Left f -> case f of
                    Abandon -> True
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


gameLoop :: (RealFrac a, Show a) => (CInt,CInt) -> Int -> Int -> Int -> a -> Etat -> Renderer -> TextureMap -> SpriteMap -> Mouse -> Keyboard -> Int -> IO ()
gameLoop dimensions clickedLem tileSizeX tileSizeY frameRate etat renderer tmap smap ms kbd nb_tours = do
    let etat' = Etat.selectLemming clickedLem etat
    startTime <- time
    events <- pollEvents
    let ms' = M.handleEvents events ms
    let kbd' = K.handleEvents events kbd

    clear renderer
    let map = casesNiveau $ niveauE etat'
    let lems = casesEnvironnement $ enviE etat'

    let (width,height) = dimensions

    let cells = (\(C x y) c -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show c)) smap) (fromIntegral (x*tileSizeX)) (fromIntegral ((hNiveau (niveauE etat') - y - 1)*tileSizeY))))
    let lemmings = (\(C x y) se -> if not (Seq.null se) then
                                    S.displaySprite renderer tmap (S.moveTo
                                    (SM.fetchSprite (SpriteId (show (Maybe.fromJust (Seq.lookup 0 se)) <> show (nb_tours `mod` 8))) smap)
                                    (fromIntegral (x*tileSizeX)) (fromIntegral ((hNiveau (niveauE etat') - y - 1)*tileSizeY)))
                                    else
                                        S.displaySprite renderer tmap (S.moveTo
                                            (SM.fetchSprite (SpriteId " ") smap)
                                                (fromIntegral (x*tileSizeX)) (fromIntegral((hNiveau (niveauE etat') - y)*tileSizeY))))

    Key.mapWithKeyM_ cells map
    Key.mapWithKeyM_ lemmings lems
    present renderer
    endTime <- time
    let refreshTime = endTime - startTime

    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    threadDelay $ delayTime * 25000
    let newEtat = tourEtat nb_tours etat
    let newEtat' = case newEtat of
                    Right e -> Etat.playLemming clickedLem e kbd'
                    Left err -> Right etat'
    SDL.P (SDL.V2 x y) <- getAbsoluteMouseLocation
    when (M.mousepressed (fromIntegral x, fromIntegral y) ms') (let res = Map.lookup (C (fromIntegral x `div`tileSizeX) ((fromIntegral height - fromIntegral y) `div`tileSizeY)) (casesEnvironnement (enviE etat)) in
                                                                 let clickedLem' = case res of
                                                                                        Just s -> case Seq.lookup 0 s of
                                                                                            Just (Lem id l) -> id
                                                                                            Nothing -> clickedLem
                                                                                        Nothing -> clickedLem
                                                                in
                                                                        if gagne newEtat then do
                                                                            print "GAGNE"
                                                                            return ()
                                                                            else
                                                                                if perdu newEtat then do
                                                                                    print "PERDU"
                                                                                    return ()
                                                                                    else if abandone newEtat' then do
                                                                                        print "Abandon"
                                                                                        return ()
                                                                                    else
                                                                                        case newEtat' of
                                                                                            Right e -> gameLoop dimensions clickedLem' tileSizeX tileSizeY frameRate e renderer tmap smap ms' kbd' (nb_tours + 1)
                                                                )
    unless (M.mousepressed (fromIntegral x, fromIntegral y) ms') (
        if gagne newEtat || perdu newEtat then (do
            case newEtat of
                Left e -> print e
            return ()) else
                        if abandone newEtat' then do
                                                print "Abandon"
                                                return ()
                            else
                                case newEtat' of
                                            Right e -> gameLoop dimensions clickedLem tileSizeX tileSizeY frameRate e renderer tmap smap ms' kbd' (nb_tours + 1)
                )

loadAssets :: Int -> Int -> Renderer -> IO(TextureMap, SpriteMap)
loadAssets tileSizeX tileSizeY rdr = do
    (tmap, smap) <- loadAsset " " tileSizeX tileSizeY rdr "assets/empty.bmp" TM.createTextureMap SM.createSpriteMap
    (tmap, smap) <- loadAsset "X" tileSizeX tileSizeY rdr "assets/metal.bmp" tmap smap
    (tmap, smap) <- loadAsset "0" tileSizeX tileSizeY rdr "assets/dirt.bmp" tmap smap
    (tmap, smap) <- loadAsset "E" tileSizeX tileSizeY rdr "assets/enter.bmp" tmap smap
    (tmap, smap) <- loadAsset "S" tileSizeX tileSizeY rdr "assets/exit.bmp" tmap smap
    (tmap, smap) <- loadAsset "M" tileSizeX tileSizeY rdr "assets/mine.bmp" tmap smap
    (tmap, smap) <- loadAsset "W" tileSizeX tileSizeY rdr "assets/mineA.bmp" tmap smap

    (tmap, smap) <- loadAsset ">" tileSizeX tileSizeY rdr "assets/lemming_walk_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset ">0" tileSizeX tileSizeY rdr "assets/lemming_walk_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset ">1" tileSizeX tileSizeY rdr "assets/lemming_walk_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset ">2" tileSizeX tileSizeY rdr "assets/lemming_walk_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset ">3" tileSizeX tileSizeY rdr "assets/lemming_walk_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset ">4" tileSizeX tileSizeY rdr "assets/lemming_walk_r/4.bmp" tmap smap
    (tmap, smap) <- loadAsset ">5" tileSizeX tileSizeY rdr "assets/lemming_walk_r/5.bmp" tmap smap
    (tmap, smap) <- loadAsset ">6" tileSizeX tileSizeY rdr "assets/lemming_walk_r/6.bmp" tmap smap
    (tmap, smap) <- loadAsset ">7" tileSizeX tileSizeY rdr "assets/lemming_walk_r/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "<" tileSizeX tileSizeY rdr "assets/lemming_walk_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "<0" tileSizeX tileSizeY rdr "assets/lemming_walk_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "<1" tileSizeX tileSizeY rdr "assets/lemming_walk_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "<2" tileSizeX tileSizeY rdr "assets/lemming_walk_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "<3" tileSizeX tileSizeY rdr "assets/lemming_walk_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "<4" tileSizeX tileSizeY rdr "assets/lemming_walk_l/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "<5" tileSizeX tileSizeY rdr "assets/lemming_walk_l/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "<6" tileSizeX tileSizeY rdr "assets/lemming_walk_l/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "<7" tileSizeX tileSizeY rdr "assets/lemming_walk_l/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "V" tileSizeX tileSizeY rdr "assets/lemming_fall_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "V0" tileSizeX tileSizeY rdr "assets/lemming_fall_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "V1" tileSizeX tileSizeY rdr "assets/lemming_fall_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "V2" tileSizeX tileSizeY rdr "assets/lemming_fall_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "V3" tileSizeX tileSizeY rdr "assets/lemming_fall_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "V4" tileSizeX tileSizeY rdr "assets/lemming_fall_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "V5" tileSizeX tileSizeY rdr "assets/lemming_fall_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "V6" tileSizeX tileSizeY rdr "assets/lemming_fall_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "V7" tileSizeX tileSizeY rdr "assets/lemming_fall_l/3.bmp" tmap smap

    (tmap, smap) <- loadAsset "v" tileSizeX tileSizeY rdr "assets/lemming_fall_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "v0" tileSizeX tileSizeY rdr "assets/lemming_fall_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "v1" tileSizeX tileSizeY rdr "assets/lemming_fall_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "v2" tileSizeX tileSizeY rdr "assets/lemming_fall_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "v3" tileSizeX tileSizeY rdr "assets/lemming_fall_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "v4" tileSizeX tileSizeY rdr "assets/lemming_fall_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "v5" tileSizeX tileSizeY rdr "assets/lemming_fall_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "v6" tileSizeX tileSizeY rdr "assets/lemming_fall_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "v7" tileSizeX tileSizeY rdr "assets/lemming_fall_r/3.bmp" tmap smap

    (tmap, smap) <- loadAsset "c" tileSizeX tileSizeY rdr "assets/lemming_mine_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "c0" tileSizeX tileSizeY rdr "assets/lemming_mine_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "c1" tileSizeX tileSizeY rdr "assets/lemming_mine_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "c2" tileSizeX tileSizeY rdr "assets/lemming_mine_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "c3" tileSizeX tileSizeY rdr "assets/lemming_mine_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "c4" tileSizeX tileSizeY rdr "assets/lemming_mine_r/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "c5" tileSizeX tileSizeY rdr "assets/lemming_mine_r/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "c6" tileSizeX tileSizeY rdr "assets/lemming_mine_r/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "c7" tileSizeX tileSizeY rdr "assets/lemming_mine_r/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "C" tileSizeX tileSizeY rdr "assets/lemming_mine_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "C0" tileSizeX tileSizeY rdr "assets/lemming_mine_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "C1" tileSizeX tileSizeY rdr "assets/lemming_mine_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "C2" tileSizeX tileSizeY rdr "assets/lemming_mine_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "C3" tileSizeX tileSizeY rdr "assets/lemming_mine_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "C4" tileSizeX tileSizeY rdr "assets/lemming_mine_l/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "C5" tileSizeX tileSizeY rdr "assets/lemming_mine_l/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "C6" tileSizeX tileSizeY rdr "assets/lemming_mine_l/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "C7" tileSizeX tileSizeY rdr "assets/lemming_mine_l/7.bmp" tmap smap

    (tmap, smap) <- loadAsset ">'" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/0.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'0" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/0.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'1" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/1.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'2" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/2.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'3" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/3.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'4" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/4.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'5" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/5.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'6" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/6.bmp" tmap smap
    (tmap, smap) <- loadAsset ">'7" tileSizeX tileSizeY rdr "assets/lemming_walk_r_clicked/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "<'" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'0" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'1" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'2" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'3" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'4" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'5" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'6" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "<'7" tileSizeX tileSizeY rdr "assets/lemming_walk_l_clicked/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "b" tileSizeX tileSizeY rdr "assets/lemming_build_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "b0" tileSizeX tileSizeY rdr "assets/lemming_build_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "b1" tileSizeX tileSizeY rdr "assets/lemming_build_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "b2" tileSizeX tileSizeY rdr "assets/lemming_build_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "b3" tileSizeX tileSizeY rdr "assets/lemming_build_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "b4" tileSizeX tileSizeY rdr "assets/lemming_build_r/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "b5" tileSizeX tileSizeY rdr "assets/lemming_build_r/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "b6" tileSizeX tileSizeY rdr "assets/lemming_build_r/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "b7" tileSizeX tileSizeY rdr "assets/lemming_build_r/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "B" tileSizeX tileSizeY rdr "assets/lemming_build_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "B0" tileSizeX tileSizeY rdr "assets/lemming_build_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "B1" tileSizeX tileSizeY rdr "assets/lemming_build_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "B2" tileSizeX tileSizeY rdr "assets/lemming_build_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "B3" tileSizeX tileSizeY rdr "assets/lemming_build_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "B4" tileSizeX tileSizeY rdr "assets/lemming_build_l/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "B5" tileSizeX tileSizeY rdr "assets/lemming_build_l/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "B6" tileSizeX tileSizeY rdr "assets/lemming_build_l/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "B7" tileSizeX tileSizeY rdr "assets/lemming_build_l/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "Ex" tileSizeX tileSizeY rdr "assets/lemming_explode/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex0" tileSizeX tileSizeY rdr "assets/lemming_explode/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex1" tileSizeX tileSizeY rdr "assets/lemming_explode/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex2" tileSizeX tileSizeY rdr "assets/lemming_explode/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex3" tileSizeX tileSizeY rdr "assets/lemming_explode/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex4" tileSizeX tileSizeY rdr "assets/lemming_explode/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex5" tileSizeX tileSizeY rdr "assets/lemming_explode/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex6" tileSizeX tileSizeY rdr "assets/lemming_explode/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "Ex7" tileSizeX tileSizeY rdr "assets/lemming_explode/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "Q" tileSizeX tileSizeY rdr "assets/lemming_block/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q0" tileSizeX tileSizeY rdr "assets/lemming_block/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q1" tileSizeX tileSizeY rdr "assets/lemming_block/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q2" tileSizeX tileSizeY rdr "assets/lemming_block/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q3" tileSizeX tileSizeY rdr "assets/lemming_block/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q4" tileSizeX tileSizeY rdr "assets/lemming_block/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q5" tileSizeX tileSizeY rdr "assets/lemming_block/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q6" tileSizeX tileSizeY rdr "assets/lemming_block/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "Q7" tileSizeX tileSizeY rdr "assets/lemming_block/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "P" tileSizeX tileSizeY rdr "assets/lemming_plug_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "P0" tileSizeX tileSizeY rdr "assets/lemming_plug_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "P1" tileSizeX tileSizeY rdr "assets/lemming_plug_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "P2" tileSizeX tileSizeY rdr "assets/lemming_plug_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "P3" tileSizeX tileSizeY rdr "assets/lemming_plug_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "P4" tileSizeX tileSizeY rdr "assets/lemming_plug_l/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "P5" tileSizeX tileSizeY rdr "assets/lemming_plug_l/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "P6" tileSizeX tileSizeY rdr "assets/lemming_plug_l/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "P7" tileSizeX tileSizeY rdr "assets/lemming_plug_l/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "p" tileSizeX tileSizeY rdr "assets/lemming_plug_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "p0" tileSizeX tileSizeY rdr "assets/lemming_plug_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "p1" tileSizeX tileSizeY rdr "assets/lemming_plug_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "p2" tileSizeX tileSizeY rdr "assets/lemming_plug_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "p3" tileSizeX tileSizeY rdr "assets/lemming_plug_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "p4" tileSizeX tileSizeY rdr "assets/lemming_plug_r/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "p5" tileSizeX tileSizeY rdr "assets/lemming_plug_r/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "p6" tileSizeX tileSizeY rdr "assets/lemming_plug_r/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "p7" tileSizeX tileSizeY rdr "assets/lemming_plug_r/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "A" tileSizeX tileSizeY rdr "assets/lemming_burn/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "A0" tileSizeX tileSizeY rdr "assets/lemming_burn/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "A1" tileSizeX tileSizeY rdr "assets/lemming_burn/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "A2" tileSizeX tileSizeY rdr "assets/lemming_burn/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "A3" tileSizeX tileSizeY rdr "assets/lemming_burn/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "A4" tileSizeX tileSizeY rdr "assets/lemming_burn/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "A5" tileSizeX tileSizeY rdr "assets/lemming_burn/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "A6" tileSizeX tileSizeY rdr "assets/lemming_burn/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "A7" tileSizeX tileSizeY rdr "assets/lemming_burn/7.bmp" tmap smap
    
    (tmap, smap) <- loadAsset "D" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "D0" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "D1" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "D2" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "D3" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "D4" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "D5" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "D6" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "D7" tileSizeX tileSizeY rdr "assets/lemming_demineur_l/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "d" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "d0" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/0.bmp" tmap smap
    (tmap, smap) <- loadAsset "d1" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/1.bmp" tmap smap
    (tmap, smap) <- loadAsset "d2" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/2.bmp" tmap smap
    (tmap, smap) <- loadAsset "d3" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/3.bmp" tmap smap
    (tmap, smap) <- loadAsset "d4" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/4.bmp" tmap smap
    (tmap, smap) <- loadAsset "d5" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/5.bmp" tmap smap
    (tmap, smap) <- loadAsset "d6" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/6.bmp" tmap smap
    (tmap, smap) <- loadAsset "d7" tileSizeX tileSizeY rdr "assets/lemming_demineur_r/7.bmp" tmap smap

    (tmap, smap) <- loadAsset "+" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+0" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+1" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+2" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+3" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+4" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+5" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    (tmap, smap) <- loadAsset "+6" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap
    loadAsset "+7" tileSizeX tileSizeY rdr "assets/lemming_dead.bmp" tmap smap