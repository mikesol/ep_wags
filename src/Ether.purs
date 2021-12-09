module Ether
  ( acePostWriteDomLineHTML
  , postToolbarInit
  , aceKeyEvent
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3)
import Effect (Effect)
import Foreign (Foreign)
import Lib as L

acePostWriteDomLineHTML :: Fn3 Foreign Foreign (Effect Unit) Unit
acePostWriteDomLineHTML = L.acePostWriteDomLineHTML

postToolbarInit :: Fn2 Foreign Foreign Unit
postToolbarInit = L.postToolbarInit

aceKeyEvent :: Fn2 Foreign Foreign Boolean
aceKeyEvent = L.aceKeyEvent