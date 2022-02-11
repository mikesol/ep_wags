module PrimePump where


primePump :: String
primePump = """module Main where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold, foldl)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (fromString)
import Data.Lens (set, view, traversed)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Maybe as DM
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.String as String
import Data.String.CodeUnits (slice)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant.Maybe (nothing)
import Data.Vec (Vec, (+>))
import Data.Unfoldable as UF
import Data.Vec as V
import Foreign.Object (Object, lookup)
import Foreign.Object as Object
import Math ((%), pi)
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Create.Optionals (bandpass, convolver, delay, gain, lowpass, highpass, pan, ref)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Tidal.Cycle (noteFromSample)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Samples (intentionalSilenceForInternalUseOnly__Sample)
import WAGS.Lib.Tidal.Tidal (addEffect, onTag, parse, changeRate, changeVolume, lnbo, make, s)
import WAGS.Lib.Tidal.Types (AFuture, BufferUrl(..), FoT, Note(..), NoteInFlattenedTime(..), Sample(..), Voice)
import WAGS.Math (calcSlope)

wag :: AFuture
wag = make 1.0 { earth: s "hh" }


-- a useful strategy to import files
sounds :: Object BufferUrl
sounds = Object.fromFoldable $ map ((/\) <$> _.slug <*> BufferUrl <<< append "https://media.graphcms.com/" <<< _.handle) files

type FileInfo = { handle :: String, slug :: String }

files :: Array FileInfo
files =
  [ { handle: "pGWoNQ9XQMiJnIqITbJ1"
    , slug: "tones:110" -- C#5
    }
  ]

"""