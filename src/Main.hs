module Main where

import Linear.V2 (V2(V2))
import qualified System.Random as Rand

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Sub as Sub
import qualified Helm.Cmd as Cmd
import qualified Helm.Time as Time
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard as Keyboard

data Action = Idle | Tick | Press Keyboard.Key
data Object = Object {mass :: Double, pos :: V2 Double, vel :: V2 Double}
newtype Model = Model [Object]

initial :: [Object] -> (Model, Cmd SDLEngine Action)
initial objs = (Model objs, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model objs) Tick = (Model objs', Cmd.none)
  where
    objs' = fmap (bounce dimensions . applyVelocity . applyGravity) objs
    dimensions = V2 screenSize screenSize
update (Model objs) (Press Keyboard.DownKey) = (Model objs', Cmd.none)
  where
    objs' = fmap applyFriction objs
update (Model objs) (Press Keyboard.RightKey) = (Model objs', Cmd.none)
  where
    objs' = fmap applyWind objs
update model (Press _) = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.every Time.millisecond $ const Tick
                      , Keyboard.downs Press ]

view :: Model -> Graphics SDLEngine
view (Model objs) = Graphics2D $ collage $ ([bg] ++) $ map mkCircleFromObj objs
  where
    mkCircleFromObj obj = move (pos obj) $ filled (rgba 0.8 0.8 0.8 0.8) $ circle (mass obj)
    bg = filled (rgb 0.1 0.1 0.1) $ square $ screenSize * 4

main :: IO ()
main = do
  engine <- SDL.startupWith engineConfig

  g <- Rand.newStdGen
  let r = Rand.randomRs (0, screenSize) g
  let objs = mkXObjectsFromValues objCount r
  run engine GameConfig
    { initialFn       = initial objs
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }



-- PHYSICS FUNCTIONS --

applyVelocity :: Object -> Object
applyVelocity (Object m p v) = Object m (p + v) v

applyForce :: V2 Double -> Object -> Object
applyForce f (Object m p v) = Object m p (v + (f `vDiv` m))

applyGravity :: Object -> Object
applyGravity obj@(Object m _ _) = applyForce gravity obj
  where gravity = V2 0 (0.2 * m)

applyWind :: Object -> Object
applyWind = applyForce wind
  where wind = V2 0.5 0


applyFriction :: Object -> Object
applyFriction obj = applyForce friction obj
  where friction = mult (-2) $ normalize $ vel obj


bounce :: V2 Double -> Object -> Object
bounce (V2 lx ly) (Object m (V2 px py) (V2 vx vy)) = Object m (V2 px' py') (V2 vx' vy')
  where
    (px', vx') = bounceAndSetPos px vx lx
    (py', vy') = bounceAndSetPos py vy ly
    radius = m
    bounceAndSetPos p v l
      | p - radius <= 0 = (radius, -v)
      | p + radius >= l = (l - radius, -v)
      | otherwise = (p, v)


-- INIT HELPERS --

mkXObjectsFromValues :: Integral a => a -> [Double] -> [Object]
mkXObjectsFromValues _ [] = []
mkXObjectsFromValues _ [_] = []
mkXObjectsFromValues _ [_,_] = []
mkXObjectsFromValues count (m:x:y:xs) = if count >= 0 then newObj : mkXObjectsFromValues (count-1) xs else [newObj]
  where
    m' = abs $ m * 0.05
    newObj = Object m' (V2 x y) (V2 0 0)



-- CONSTANTS --

objCount :: Integer
objCount = 10

screenCenter :: V2 Double
screenCenter = V2 halfWSize halfWSize

halfWSize :: Double
halfWSize = screenSize / 2

screenSize :: Num a => a
screenSize = 800

engineConfig :: SDL.SDLEngineConfig
engineConfig = SDL.SDLEngineConfig (V2 screenSize screenSize) False False "Force"



-- VECTOR FUNCTIONS --

mult :: Num a => a -> V2 a -> V2 a
mult s (V2 x y) = V2 (s*x) (s*y)

vDiv :: Fractional a => V2 a -> a -> V2 a
vDiv (V2 x y) s = V2 (x/s) (y/s)

limit :: (Floating a, Ord a) => a -> V2 a -> V2 a
limit l v = if mag v > l then mult l (normalize v) else v

mag :: (Floating a) => V2 a -> a
mag (V2 x y) = sqrt $ x^(2::Int) + y^(2::Int)

normalize :: Floating a => V2 a -> V2 a
normalize v@(V2 x y) = V2 (x / m) (y / m)
  where
    m = mag v
