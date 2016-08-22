{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data World = World { cells :: Set (Int, Int), playing :: Bool, viewPort :: ViewPort }

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
    play (InWindow "Game of Life" (800, 600) (0, 0)) white 10 initialWorld drawWorld handleInput $ const stepWorld

initialWorld :: World
initialWorld = World { cells = Set.empty, playing = False, viewPort = initialViewPort }

initialViewPort :: ViewPort
initialViewPort = viewPortInit { viewPortScale = 10 }

drawWorld :: World -> Picture
drawWorld World {..} = applyViewPortToPicture viewPort $ pictures $ map drawCell $ Set.toList cells
    where drawCell (x, y) = translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1

stepWorld :: World -> World
stepWorld world@World { playing = False } = world
stepWorld world@World {..} = world { cells = Set.foldr' accFunc Set.empty cells }
    where accFunc cell acc = Set.union acc $ Set.filter alive $ surrounding cell
          surrounding (x, y) = Set.fromList [ (x + w, y + h) | w <- [-1..1], h <- [-1..1] ]
          alive cell = if neighbors == 4 then Set.member cell cells else neighbors == 3
              where neighbors = Set.size $ Set.filter (`Set.member` cells) $ surrounding cell

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World {..} = world { playing = not playing }
handleInput (EventKey (MouseButton button) Down _ coords) world@World {..}
    | button == LeftButton && Set.notMember cell cells = world { cells = Set.insert cell cells }
    | button == RightButton && Set.member cell cells = world { cells = Set.delete cell cells }
    where cell = let (x, y) = invertViewPort viewPort coords in (round x, round y)
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