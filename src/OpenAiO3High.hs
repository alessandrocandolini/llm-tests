{-# LANGUAGE RecordWildCards #-}
module OpenAiO3High where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.Random

-- A ball is represented by its position, velocity, radius, mass and colour.
data Ball = Ball
  { pos        :: (Float, Float)
  , vel        :: (Float, Float)
  , radius     :: Float
  , mass       :: Float
  , ballColour :: Color
  } deriving Show

-- Simulation parameters.
gravity :: Float
gravity = -980    -- Acceleration due to gravity (in pixels per second²)

frictionCoeff :: Float
frictionCoeff = 0.99   -- Friction (or air resistance) factor applied every frame

-- Dimensions of the box (window).
boxWidth, boxHeight :: Float
boxWidth = 800
boxHeight = 600

xMin, xMax, yMin, yMax :: Float
xMin = -boxWidth / 2
xMax =  boxWidth / 2
yMin = -boxHeight / 2
yMax =  boxHeight / 2

----------------------------------------------------------------------
-- UPDATE FUNCTIONS
----------------------------------------------------------------------

-- | The main update function. For each time step the programme
-- updates each ball’s position and velocity (applying gravity and friction),
-- handles collisions with the walls and then resolves collisions between balls.
updateWorld :: Float -> [Ball] -> [Ball]
updateWorld dt balls =
  let balls' = map (handleWallCollision . updateBall dt) balls
  in resolveCollisions balls'

-- | Update a single ball’s velocity and position.
updateBall :: Float -> Ball -> Ball
updateBall dt ball@Ball{..} =
  let (vx, vy) = vel
      vy' = vy + gravity * dt
      vx' = vx
      vx'' = vx' * frictionCoeff
      vy'' = vy' * frictionCoeff
      (x, y) = pos
      x' = x + vx'' * dt
      y' = y + vy'' * dt
  in ball { pos = (x', y'), vel = (vx'', vy'') }

-- | Handle collisions with the walls of the box.
handleWallCollision :: Ball -> Ball
handleWallCollision ball@Ball{..} =
  let (x, y)   = pos
      (vx, vy) = vel
      r        = radius
      (x', vx') | x - r < xMin = (xMin + r, -vx)
                | x + r > xMax = (xMax - r, -vx)
                | otherwise    = (x, vx)
      (y', vy') | y - r < yMin = (yMin + r, -vy)
                | y + r > yMax = (yMax - r, -vy)
                | otherwise    = (y, vy)
  in ball { pos = (x', y'), vel = (vx', vy') }

----------------------------------------------------------------------
-- COLLISION DETECTION AND RESPONSE BETWEEN BALLS
----------------------------------------------------------------------

colliding :: Ball -> Ball -> Bool
colliding b1 b2 =
  let (x1, y1) = pos b1
      (x2, y2) = pos b2
      dx = x1 - x2
      dy = y1 - y2
      distance = sqrt (dx * dx + dy * dy)
  in distance < (radius b1 + radius b2)

resolveCollision :: Ball -> Ball -> (Ball, Ball)
resolveCollision b1 b2
  | not (colliding b1 b2) = (b1, b2)
  | otherwise =
      let (x1, y1) = pos b1
          (x2, y2) = pos b2
          dx = x1 - x2
          dy = y1 - y2
          d = sqrt (dx * dx + dy * dy)
          nx = dx / d
          ny = dy / d
          (vx1, vy1) = vel b1
          (vx2, vy2) = vel b2
          a1 = vx1 * nx + vy1 * ny
          a2 = vx2 * nx + vy2 * ny
      in if a1 - a2 >= 0
         then (b1, b2)
         else let m1 = mass b1
                  m2 = mass b2
                  optimizedP = (2 * (a1 - a2)) / (m1 + m2)
                  vx1' = vx1 - optimizedP * m2 * nx
                  vy1' = vy1 - optimizedP * m2 * ny
                  vx2' = vx2 + optimizedP * m1 * nx
                  vy2' = vy2 + optimizedP * m1 * ny
                  overlap = (radius b1 + radius b2 - d) / 2
                  x1' = x1 + nx * overlap
                  y1' = y1 + ny * overlap
                  x2' = x2 - nx * overlap
                  y2' = y2 - ny * overlap
              in ( b1 { pos = (x1', y1'), vel = (vx1', vy1') }
                 , b2 { pos = (x2', y2'), vel = (vx2', vy2') }
                 )

resolveCollisions :: [Ball] -> [Ball]
resolveCollisions balls = foldl resolvePair balls pairs
  where
    n = length balls
    pairs = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]
    resolvePair bs (i, j) =
      let b1 = bs !! i
          b2 = bs !! j
          (b1', b2') = resolveCollision b1 b2
          bs' = updateList i b1' bs
      in updateList j b2' bs'

updateList :: Int -> a -> [a] -> [a]
updateList i x xs = take i xs ++ [x] ++ drop (i + 1) xs

----------------------------------------------------------------------
-- DRAWING FUNCTIONS
----------------------------------------------------------------------

drawWorld :: [Ball] -> Picture
drawWorld balls = Pictures (drawBox : map drawBall balls)

drawBox :: Picture
drawBox = Color white $ Line [(xMin, yMin), (xMax, yMin), (xMax, yMax), (xMin, yMax), (xMin, yMin)]

drawBall :: Ball -> Picture
drawBall Ball{..} =
  let (x, y) = pos
  in Translate x y $ Color ballColour $ circleSolid radius

----------------------------------------------------------------------
-- BALL GENERATION
----------------------------------------------------------------------

generateBalls :: Int -> StdGen -> [Ball]
generateBalls n gen =
  let (genX, gen1) = split gen
      xs = take n $ randomRs (xMin + 30, xMax - 30) genX
      (genY, gen2) = split gen1
      ys = take n $ randomRs (yMin + 30, yMax - 30) genY
      (genVX, gen3) = split gen2
      vxs = take n $ randomRs (-150, 150) genVX
      (genVY, gen4) = split gen3
      vys = take n $ randomRs (-150, 150) genVY
      (genR, gen5) = split gen4
      rs = take n $ randomRs (10, 30) genR
      (genC1, gen6) = split gen5
      cs1 = take n $ randomRs (0, 1) genC1
      (genC2, genC3) = split gen6
      cs2 = take n $ randomRs (0, 1) genC2
      cs3 = take n $ randomRs (0, 1) genC3
  in [ Ball { pos = (x, y)
            , vel = (vx, vy)
            , radius = r
            , mass = r * r
            , ballColour = makeColor c1 c2 cs3 1
            }
     | (x, y, vx, vy, r, c1, c2, cs3) <- zip8 xs ys vxs vys rs cs1 cs2 cs3
     ]

zip8 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [(a, b, c, d, e, f, g, h)]
zip8 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
  (a, b, c, d, e, f, g, h) : zip8 as bs cs ds es fs gs hs
zip8 _ _ _ _ _ _ _ _ = []

----------------------------------------------------------------------
-- EVENT HANDLING
----------------------------------------------------------------------

-- Using playIO allows us to perform IO actions in the event handler.
handleEvent :: Event -> [Ball] -> IO [Ball]
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleEvent _ world = return world

----------------------------------------------------------------------
-- MAIN
----------------------------------------------------------------------

main :: IO ()
main = do
  let numBalls = 10  -- Change this value for a different number of balls.
  gen <- newStdGen
  let balls = generateBalls numBalls gen
  playIO (InWindow "Bouncing Balls" (round boxWidth, round boxHeight) (100, 100))
         black                           -- Background colour.
         60                              -- Simulation steps per second.
         balls                           -- Initial world.
         (return . drawWorld)            -- Drawing function.
         handleEvent                     -- Event handling.
         (\dt world -> return (updateWorld dt world))  -- Update function.
