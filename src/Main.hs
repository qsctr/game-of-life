module Main where

import Graphics.Gloss
import Data.List

type Cell = (Int, Int)
type World = [Cell]

main :: IO ()
main = play window white 2 initialWorld drawWorld handleInput $ const stepWorld
    where
        window = InWindow "Game of Life" (600, 600) (0, 0)

initialWorld :: World
initialWorld = []

drawWorld :: World -> Picture
drawWorld = pictures . map drawCell
    where
        drawCell (x, y) = translate (x' * factor) (y' * factor) $ rectangleSolid factor factor
            where
                x' = fromIntegral x
                y' = fromIntegral y
                factor = 20

stepWorld :: World -> World
stepWorld world = nub $ concatMap (filter alive . surrounding) world
    where
        surrounding (x, y) = [(x + w, y + h) | w <- [-1..1], h <- [-1..1]]
        alive cell = if neighbors == 4 then cell `elem` world else neighbors == 3
            where
                neighbors = length $ filter (`elem` world) $ surrounding cell
