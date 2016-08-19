{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data World = World { cells :: [(Int, Int)], playing :: Bool, viewPort :: ViewPort }

main :: IO ()
main = do
    mapM_ putStrLn
        [ "Instructions:"
        , "Left click: Add cell"
        , "Right click: Remove cell"
        , "Space bar: Play/pause"
        , "Arrow keys: Navigate"
        , "Z: Zoom in"
        , "X: Zoom out"
        , "C: Center and reset zoom"
        , "R: Restart game"
        , "Press enter to continue." ]
    _ <- getLine
    play (InWindow "Game of Life" (800, 600) (0, 0)) white 5 initialWorld drawWorld handleInput stepWorld

initialWorld :: World
initialWorld = World { cells = [], playing = False, viewPort = initialViewPort }

initialViewPort :: ViewPort
initialViewPort = viewPortInit { viewPortScale = 15 }

drawWorld :: World -> Picture
drawWorld World {..} = applyViewPortToPicture viewPort $ pictures $ map drawCell cells
    where drawCell (x, y) = translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1

stepWorld :: Float -> World -> World
stepWorld _ world@World { playing = False } = world
stepWorld _ world@World {..} = world { cells = nub $ concatMap (filter alive . surrounding) cells }
    where surrounding (x, y) = [ (x + w, y + h) | w <- [-1..1], h <- [-1..1] ]
          alive cell = if neighbors == 4 then cell `elem` cells else neighbors == 3
              where neighbors = length $ filter (`elem` cells) $ surrounding cell

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World {..} = world { playing = not playing }
handleInput (EventKey (MouseButton button) Down _ coords) world@World {..}
    | button == LeftButton && cell `notElem` cells = world { cells = cell : cells }
    | button == RightButton && cell `elem` cells = world { cells = delete cell cells }
    where cell = mapPair round $ invertViewPort viewPort coords
handleInput (EventKey (SpecialKey key) Down _ _) world@World {..}
    | key == KeyLeft = t (tx + 5, ty)
    | key == KeyRight = t (tx - 5, ty)
    | key == KeyUp = t (tx, ty - 5)
    | key == KeyDown = t (tx, ty + 5)
    where t translation = world { viewPort = viewPort { viewPortTranslate = translation } }
          (tx, ty) = viewPortTranslate viewPort
handleInput (EventKey (Char c) Down _ _) world@World {..}
    | c == 'z' = s $ zoom + 2
    | c == 'x' = s $ max 1 $ zoom - 2
    where s factor = world { viewPort = viewPort { viewPortScale = factor } }
          zoom = viewPortScale viewPort
handleInput (EventKey (Char 'c') Down _ _) world = world { viewPort = initialViewPort }
handleInput (EventKey (Char 'r') Down _ _) _ = initialWorld
handleInput _ world = world

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)