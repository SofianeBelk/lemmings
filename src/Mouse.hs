module Mouse where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

-- Mouse

type Mouse = Set (Integer , Integer)

-- Constructeur Mouse

createMouse :: Mouse
createMouse = S.empty


handleEvent :: Event -> Mouse -> Mouse
handleEvent event m =
  case eventPayload event of
    MouseButtonEvent ms ->
      let SDL.P (SDL.V2 x y) = SDL.mouseButtonEventPos ms in
        if mouseButtonEventMotion ms == Pressed then
                S.insert  (fromIntegral x, fromIntegral y)  m
        else
            if mouseButtonEventMotion ms == Released then
                S.delete (fromIntegral x, fromIntegral y)  m
           else m
    _ -> m

-- | prise en compte des événements SDL2 pour mettre à jour l'état de la sourie
handleEvents :: [Event] -> Mouse -> Mouse
handleEvents events kbd = foldl' (flip handleEvent) kbd events


-- | Vérifies si les coordonées  spécifier est actuellement
-- | actif sur la sourie.
mousepressed :: (Integer, Integer) -> Mouse -> Bool
mousepressed = S.member

