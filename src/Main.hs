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

data Action = Idle | Tick | AddRandomAcceleration Rand.StdGen | Press
data Model = Model (V2 Double) (V2 Double)


screenCenter :: V2 Double
screenCenter = V2 400 400

initial :: (Model, Cmd SDLEngine Action)
initial = (Model screenCenter (V2 0 0), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model Tick = (model, Cmd.execute Rand.newStdGen AddRandomAcceleration)
update (Model pos vel) (AddRandomAcceleration rd) = (Model pos' vel', Cmd.none)
  where
    generated = Rand.randomRs (-0.1, 0.1) rd
    rdAcc = (\(x:y:_) -> V2 x y) generated
    pos' = pos + vel
    vel' = limit 5 $ vel + rdAcc
update (Model pos vel) Press = (Model pos' vel', Cmd.none)
  where
    vel' = limit 5 $ vel + mult 0.01 (screenCenter - pos)
    pos' = pos + vel'

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.every Time.millisecond $ const Tick
                      , Keyboard.downs (\k -> if k == Keyboard.SpaceKey then Press else Idle) ]

view :: Model -> Graphics SDLEngine
view (Model pos _) = Graphics2D $ collage [move pos $ filled (rgb 1 1 1) $ circle 5]

main :: IO ()
main = do
  engine <- SDL.startup

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

mult :: (Floating a) => a -> V2 a -> V2 a
mult s (V2 x y) = V2 (s*x) (s*y)

limit :: (Floating a, Ord a) => a -> V2 a -> V2 a
limit l v = if mag v > l then mult l (normalize v) else v

mag :: (Floating a) => V2 a -> a
mag (V2 x y) = sqrt $ x^(2::Int) + y^(2::Int)

normalize :: Floating a => V2 a -> V2 a
normalize v@(V2 x y) = V2 (x / m) (y / m)
  where
    m = mag v
