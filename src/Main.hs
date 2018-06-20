module Main where

import Linear.V2 (V2(V2))
import qualified System.Random as Rand

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Time as Time
import qualified Helm.Engine.SDL as SDL

data Action = Idle | Tick | AddRandomAcceleration Rand.StdGen
data Model = Model (V2 Double) (V2 Double)


initial :: (Model, Cmd SDLEngine Action)
initial = (Model (V2 200 200) (V2 0 0), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model Tick = (model, Cmd.execute Rand.newStdGen AddRandomAcceleration)
update (Model pos vel) (AddRandomAcceleration rd) = (Model pos' vel', Cmd.none)
  where
    generated = Rand.randomRs (-0.1, 0.1) rd
    rdAcc = (\(x:y:_) -> V2 x y) generated
    pos' = pos + vel
    vel' = vel + rdAcc


subscriptions :: Sub SDLEngine Action
subscriptions = Time.every Time.millisecond $ const Tick

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
