module Miro where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Foreign.Object as Object
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (Error, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import JIT.EvalSources (freshModules)
import Lib (PlayingState(..), InitSig, initF)
import WAGS.Lib.Tidal.Cycle (bd)

foreign import miroOnReady :: Effect Unit -> InitSig -> (Error -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit

foreign import allTexts :: Effect (Promise (Array { text :: String }))
foreign import stripHtml :: String -> String
foreign import isAuthorized :: Effect (Promise Boolean)
foreign import requestAuthorization :: Effect (Promise Unit)
foreign import getAwfulHack_ :: Effect (Unit -> Effect Unit)

selectionHandler :: Ref.Ref String -> Effect Unit
selectionHandler rf = launchAff_ do
  txts <- toAffE allTexts
  let
    myText = find
      ( _.text >>>
          ((||) <$> contains (Pattern "# @wag") <*> contains (Pattern "-- @wag"))
      )
      txts
  for_ myText \yes ->
    liftEffect do
      Ref.write yes.text rf
      hack <- getAwfulHack_
      hack unit

main :: Effect Unit
main = do
  modulesR <- freshModules >>= Ref.new
  bufferCache <- Ref.new Object.empty
  playingState <- Ref.new Stopped
  cycleRef <- Ref.new bd
  txtRf <- Ref.new ""
  let
    onClick = initF cycleRef playingState bufferCache modulesR
      ( do
          ia <- toAffE isAuthorized
          when (not ia) do
            toAffE requestAuthorization
      )
      (Ref.read txtRf)
  launchAff_ do
    makeAff \cb -> do
      miroOnReady (selectionHandler txtRf) onClick (cb <<< Left) (cb <<< Right)
      mempty
