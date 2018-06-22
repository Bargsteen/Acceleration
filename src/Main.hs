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

data Action = Idle | Tick | Press
data Model = Model (V2 Double) (V2 Double)


screenCenter :: V2 Double
screenCenter = V2 halfW halfW
  where halfW = screenSize / 2

initial :: (Model, Cmd SDLEngine Action)
initial = (Model screenCenter (V2 0 0), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model pos vel) Tick = (model', Cmd.none)
  where
    vel' = applyGravity vel
    pos' = pos + vel'
    dimensions = V2 screenSize screenSize
    model' = bounce dimensions (Model pos' vel')
update (Model pos vel) Press = (Model pos' vel', Cmd.none)
  where
    vel' = applyWind vel
    pos' = pos + vel'

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.every Time.millisecond $ const Tick
                      , Keyboard.downs (\k -> if k == Keyboard.SpaceKey then Press else Idle) ]

view :: Model -> Graphics SDLEngine
view (Model pos _) = Graphics2D $ collage [move pos $ filled (rgb 1 1 1) $ circle 10]

main :: IO ()
main = do
  engine <- SDL.startupWith engineConfig

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }


screenSize :: Num a => a
screenSize = 800

engineConfig :: SDL.SDLEngineConfig
engineConfig = SDL.SDLEngineConfig (V2 screenSize screenSize) False False "Force"

mult :: Num a => a -> V2 a -> V2 a
mult s (V2 x y) = V2 (s*x) (s*y)

limit :: (Floating a, Ord a) => a -> V2 a -> V2 a
limit l v = if mag v > l then mult l (normalize v) else v

mag :: (Floating a) => V2 a -> a
mag (V2 x y) = sqrt $ x^(2::Int) + y^(2::Int)

normalize :: Floating a => V2 a -> V2 a
normalize v@(V2 x y) = V2 (x / m) (y / m)
  where
    m = mag v

applyForce :: Num a => V2 a -> V2 a -> V2 a
applyForce f a = a + f

applyGravity :: Fractional a => V2 a -> V2 a
applyGravity = (+ V2 0 0.3)

applyWind :: Fractional a => V2 a -> V2 a
applyWind = (+ V2 0.4 0)

bounce :: V2 Double -> Model -> Model
bounce (V2 lx ly) (Model (V2 px py) (V2 vx vy)) = Model (V2 px py) (V2 vx' vy')
  where
    vx' = if px <= 0 || px >= lx then (-vx) else vx
    vy' = if py <= 0 || py >= ly then (-vy) else vy
