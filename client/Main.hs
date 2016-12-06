-- This file is part of the RayActor Lighting Software.
--
-- RayActor is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- RayActor is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
-- more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Rayactor. If not, see <http://www.gnu.org/licenses/>.
--
module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Linear.V2 (V2(V2))

import Helm (run, GameConfig(..), Graphics(..), Cmd, Sub)
import qualified Helm.Cmd as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Window as Window
import qualified Helm.Sub as Sub
import qualified Helm.Graphics2D as G
import qualified Helm.Color as C

import Connection

type Engine = SDL.SDLEngine

data Model = Model
    { winSize :: V2 Double
    , dmxData :: ByteString
    } deriving Show

data Action = Resize (V2 Double)
            | KeyPress Keyboard.Key
            | DmxIn ByteString
            deriving Show

modelInit :: Model
modelInit = Model
    { winSize = V2 0 0
    , dmxData = BS.replicate 512 0
    }

initial :: (Model, Cmd Engine Action)
initial = (modelInit, Window.size (Resize . fmap fromIntegral))

mkSubs :: Connection -> Sub Engine Action
mkSubs conn = Sub.batch
    [ Keyboard.presses KeyPress
    , Window.resizes (Resize . fmap fromIntegral)
    , netReceived conn DmxIn
    ]

view :: Model -> Graphics Engine
view m = Graphics2D $ G.center ((/ 2) <$> winSize m) $
    G.collage
        [ G.move (V2 (-22) (-22)) $ G.filled (C.rgb r1  0  0) $ G.circle 20.0
        , G.move (V2   22  (-22)) $ G.filled (C.rgb  0 g1  0) $ G.circle 20.0
        , G.move (V2 (-22)   22 ) $ G.filled (C.rgb  0  0 b1) $ G.circle 20.0
        , G.move (V2   22    22 ) $ G.filled (C.rgb r2 g2 b2) $ G.circle 20.0
        ]
  where [r1, g1, b1, r2, g2, b2] = map ((/ 255) . fromIntegral) $
            BS.unpack $ BS.take 6 $ dmxData m

update :: Model -> Action -> (Model, Cmd Engine Action)
update model (Resize new) = (model { winSize = new }, Cmd.none)
update model (DmxIn dmx) = (model { dmxData = dmx }, Cmd.none)
update model _ = (model, Cmd.none)

engineConfig :: SDL.SDLEngineConfig
engineConfig = SDL.defaultConfig
    { SDL.windowTitle = "RayActor" }

engine :: IO Engine
engine = SDL.startupWith engineConfig

mkConfig :: Connection -> GameConfig Engine Model Action
mkConfig conn = GameConfig
    { initialFn = initial
    , updateFn = update
    , subscriptionsFn = mkSubs conn
    , viewFn = view
    }

main :: IO ()
main = withConnection "127.0.0.1" "4444" $ \conn ->
    engine >>= flip run (mkConfig conn)
