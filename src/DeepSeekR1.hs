module DeepSeekR1 where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

-- Constants
gravity :: Float
gravity = -9.8

friction :: Float
friction = 0.99

boxWidth :: Float
boxWidth = 800

boxHeight :: Float
boxHeight = 600

-- Data types
data Ball = Ball
  { position :: (Float, Float)
  , velocity :: (Float, Float)
  , radius   :: Float
  , mass     :: Float
  } deriving (Eq, Show)

type World = [Ball]

-- Initialize the world with random balls
initializeWorld :: Int -> IO World
initializeWorld n = do
  gen <- newStdGen
  let balls = take n $ randomBalls gen
  return balls

-- Generate an infinite list of random balls
randomBalls :: RandomGen g => g -> [Ball]
randomBalls gen = map createBall $ randomRs (0.1, 1.0) gen
  where
    createBall r =
      let posX = fst $ randomR (0, boxWidth) gen
          posY = fst $ randomR (0, boxHeight) gen
          velX = fst $ randomR (-100, 100) gen
          velY = fst $ randomR (-100, 100) gen
          massVal = r * 10  -- Mass proportional to radius
      in Ball (posX, posY) (velX, velY) r massVal

-- Update the world
updateWorld :: ViewPort -> Float -> World -> World
updateWorld _ dt = map (updateBall dt)

-- Update a single ball
updateBall :: Float -> Ball -> Ball
updateBall dt ball@(Ball (x, y) (vx, vy) r m) =
  let -- Apply gravity
      vy' = vy + gravity * dt
      -- Apply friction
      vx' = vx * friction
      vy'' = vy' * friction
      -- Update position
      x' = x + vx' * dt
      y' = y + vy'' * dt
      -- Handle collisions with walls
      (x'', vx'') = if x' - r < 0 || x' + r > boxWidth
                    then (if x' - r < 0 then r else boxWidth - r, -vx')
                    else (x', vx')
      (y'', vy''') = if y' - r < 0 || y' + r > boxHeight
                     then (if y' - r < 0 then r else boxHeight - r, -vy'')
                     else (y', vy'')
  in Ball (x'', y'') (vx'', vy''') r m

-- Detect and resolve collisions between balls
resolveCollisions :: World -> World
resolveCollisions balls = foldl handleCollision balls pairs
  where
    pairs = [(b1, b2) | b1 <- balls, b2 <- balls, b1 /= b2]

handleCollision :: World -> (Ball, Ball) -> World
handleCollision world (b1, b2)
  | areColliding b1 b2 = resolveCollision b1 b2 world
  | otherwise = world

areColliding :: Ball -> Ball -> Bool
areColliding (Ball (x1, y1) _ r1 _) (Ball (x2, y2) _ r2 _) =
  let dx = x2 - x1
      dy = y2 - y1
      distance = sqrt (dx * dx + dy * dy)
  in distance < (r1 + r2)

resolveCollision :: Ball -> Ball -> World -> World
resolveCollision b1 b2 world =
  let (v1', v2') = calculateNewVelocities b1 b2
      b1' = b1 { velocity = v1' }
      b2' = b2 { velocity = v2' }
  in map (\b -> if b == b1 then b1' else if b == b2 then b2' else b) world

calculateNewVelocities :: Ball -> Ball -> ((Float, Float), (Float, Float))
calculateNewVelocities (Ball (x1, y1) (vx1, vy1) r1 m1) (Ball (x2, y2) (vx2, vy2) r2 m2) =
  let dx = x2 - x1
      dy = y2 - y1
      distance = sqrt (dx * dx + dy * dy)
      nx = dx / distance
      ny = dy / distance
      p = 2 * (vx1 * nx + vy1 * ny - vx2 * nx - vy2 * ny) / (m1 + m2)
      v1x' = vx1 - p * m2 * nx
      v1y' = vy1 - p * m2 * ny
      v2x' = vx2 + p * m1 * nx
      v2y' = vy2 + p * m1 * ny
  in ((v1x', v1y'), (v2x', v2y'))

-- Render the world
renderWorld :: World -> Picture
renderWorld balls = pictures $ map renderBall balls

renderBall :: Ball -> Picture
renderBall (Ball (x, y) _ r _) =
  translate x y $ color ballColor $ circleSolid r
  where
    ballColor = makeColor 1 0 0 1  -- Red color

-- Main function
main :: IO ()
main = do
  putStrLn "Enter the number of balls:"
  n <- readLn
  world <- initializeWorld n
  simulate (InWindow "Bouncing Balls" (round boxWidth, round boxHeight) (10, 10)) white 60 world renderWorld updateWorld
