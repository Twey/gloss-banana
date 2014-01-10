module Graphics.Gloss.Interface.FRP.ReactiveBanana (playBanana, InputEvent) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A useful type synonym to avoid confusion between
--   Gloss and ReactiveBanana.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior t Picture changes.
playBanana ∷ Display -- ^ The display method
           → Color   -- ^ The background colour
           → Int     -- ^ The refresh rate, in Hertz
           → (∀ t. Frameworks t
              ⇒ Event t Float      -- ^ A stream of refresh events, with
                                   --   time delta since last refresh
              → Event t InputEvent -- ^ A stream of input events
              → Moment t (Behavior t Picture))
           → IO ()
playBanana display colour frequency mPicture = do
  pictureref ← newIORef blank
  (tickHandler,  tick)  ← newAddHandler
  (eventHandler, event) ← newAddHandler
  compile (makeNetwork tickHandler eventHandler (change pictureref)) >>= actuate
  playIO display colour frequency ()
    (\      _ → readIORef pictureref)
    (\ ev   _ → () <$ event ev)
    (\ time _ → () <$ tick time)
  where
    change ∷ IORef Picture → Picture → IO ()
    change pictureref picture = do
      writeIORef pictureref picture
    makeNetwork tickHandler eventHandler change = do
      eTick  ← fromAddHandler tickHandler
      eEvent ← fromAddHandler eventHandler
      bPicture ← mPicture eTick eEvent
      changes bPicture >>= reactimate . fmap change
      initial bPicture >>= liftIO . change
