module Main where

import Linear.V2 (V2(V2))
import qualified System.Random as Rand
import Data.Monoid

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Sub as Sub
import qualified Helm.Cmd as Cmd
import qualified Helm.Time as Time
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard as Keyboard

data Action = Idle | Tick | Press Keyboard.Key | NewState [Object]
data Object = Object {mass :: Double, pos :: V2 Double, vel :: V2 Double}
data Area = Area {aPos :: V2 Double, dimensions :: V2 Double}
data Model = Model [Object] [Area]

initial :: [Object] -> (Model, Cmd SDLEngine Action)
initial objs = (Model objs [Area (V2 300 500) (V2 600 200), Area (V2 300 200) (V2 700 40), Area (V2 700 700) (V2 100 100)], Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model objs areas) Tick = (Model objs' areas, Cmd.none)
  where
    objs' = fmap (bounce dim . applyDragIfInAnyArea areas . applyVelocity . applyGravity) objs
    dim = V2 screenSize screenSize
update (Model objs areas) (Press k) =
  case k of
    Keyboard.RightKey -> (Model (fmap applyWind objs) areas, Cmd.none)
    Keyboard.ReturnKey -> (Model objs areas, Cmd.execute mkRandomObjects NewState)
    _ -> (Model objs areas, Cmd.none)
update (Model _ areas) (NewState newObjs) = (Model newObjs areas, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.every Time.millisecond $ const Tick
                      , Keyboard.downs Press ]

view :: Model -> Graphics SDLEngine
view (Model objs areas) = Graphics2D $ collage $ map (toForm . collage) [background, areaRects, circles]
  where
    mkCircleFromObj obj = move (pos obj) $ filled (redIfInArea obj) $ circle (mass obj)
    redIfInArea obj = if isInAnyArea areas obj then rgba 0.8 0 0 0.8 else rgba 0.8 0.8 0.8 0.8
    lightGray = rgb 0.3 0.3 0.3
    darkGray = rgb 0.1 0.1 0.1
    background = pure $ filled darkGray $ square $ screenSize * 4
    areaRects = map (\area -> move (aPos area) $ filled lightGray $ rect $ dimensions area) areas
    circles = map mkCircleFromObj objs

main :: IO ()
main = do
  engine <- SDL.startupWith engineConfig
  objs <- mkRandomObjects
  run engine GameConfig
    { initialFn       = initial objs
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }


mkRandomObjects :: IO [Object]
mkRandomObjects = do
  g <- Rand.newStdGen
  let r = Rand.randomRs (0, screenSize) g
  let objs = mkXObjectsFromValues objCount r
  return objs

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
  where friction = mult (-0.2) $ normalize $ vel obj


type AreaLeftTop = V2 Double
type AreaRightBottom = V2 Double


isInAnyArea :: [Area] -> Object -> Bool
isInAnyArea areas obj = getAny $ foldMap (Any . isInArea obj) areas

isInArea :: Object -> Area -> Bool
isInArea (Object _ (V2 x y) _) (Area (V2 px py) (V2 sx sy)) =
     x  >= left && x <= right
  && y  >= top  && y <= bottom
  where
    left      = px - sx / 2
    right     = px + sx / 2
    top       = py - sy / 2
    bottom    = py + sy / 2


applyDragIfInAnyArea :: [Area] -> Object -> Object
applyDragIfInAnyArea areas obj = if isInAnyArea areas obj then applyForce drag obj else obj
  where
    n = normalize $ vel obj
    s = mag $ vel obj
    factor = -0.1
    drag = mult (factor * s^(2::Int)) n


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
    m' = abs $ m * 0.03
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
