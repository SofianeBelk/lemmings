module SpriteMap where

import Data.Map.Strict (Map)
import qualified Data.Map as M

import Sprite (Sprite)
import qualified Sprite as S

-- SpriteId

newtype SpriteId = SpriteId String
  deriving (Eq, Ord)

-- Instanciation show

instance Show SpriteId where
  show (SpriteId id) = show id

-- spriteMap
type SpriteMap = Map SpriteId Sprite

-- Constructeur

createSpriteMap :: SpriteMap
createSpriteMap = M.empty

-- les fonctions de modification de sprite "pas de pré-condition ni de post-condition"

addSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
addSprite sid =
  M.insertWithKey (\_ _ _ -> error $ "addSprite - Sprite '" <> show sid<> "' already in sprite map.")
  sid

fetchSprite :: SpriteId -> SpriteMap -> Sprite
fetchSprite sid smap = case M.lookup sid smap of
                         Nothing -> error $ "fetchSprite - No such Sprite: " <> show sid
                         Just spr -> spr

updateSprite :: (Sprite -> Sprite) -> SpriteId -> SpriteMap -> SpriteMap
updateSprite f sid smap = M.alter aux sid smap
  where aux Nothing = error $ "updateSprite - No such sprite '" <> show sid <> "' in sprite map."
        aux (Just old) = Just $ f old

changeSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
changeSprite sid spr = updateSprite (const spr) sid

removeSprite :: SpriteId -> SpriteMap -> SpriteMap
removeSprite sid smap = case M.lookup sid smap of
                          Nothing -> error $ "removeSprite - No such sprite '" <> show sid <> "' in sprite map."
                          Just _ -> M.delete sid smap

