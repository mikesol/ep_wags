module Unmute where

import Prelude

import WAGS.WebAPI (AudioContext)
import Effect (Effect)

foreign import unmute :: AudioContext -> Boolean -> Boolean -> Effect Unit