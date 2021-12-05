module Main
  ( acePostWriteDomLineHTML
  , postToolbarInit
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Except (runExceptT, throwError)
import Control.Promise (toAffE)
import Data.Array (intercalate)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3)
import Data.Functor (mapFlipped)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (create, makeEvent, subscribe)
import Foreign (Foreign)
import Foreign.Index (readProp)
import JIT.API as API
import JIT.Compile (compile)
import JIT.EvalSources (evalSources, freshModules)
import JIT.Loader (Loader, makeLoader)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Interpret (close, constant0Hack, context, contextResume, contextState, makeFFIAudioSnapshot)
import WAGS.Lib.Learn (Analysers, FullSceneBuilder(..))
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Types (TidalRes)
import WAGS.Lib.Tidal.Util (doDownloads)
import WAGS.Run (Run, run)
import WAGS.WebAPI (AudioContext)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 40 (initialTime - adj)
  in
    fOf 40

r2b :: Ref.Ref ~> Behavior
r2b r = behavior \e ->
  makeEvent \p ->
    subscribe e \f ->
      Ref.read r >>= p <<< f

foreign import sanitizeUsingRegex_ :: String -> String

sanitizePS :: String -> String
sanitizePS = sanitizeUsingRegex_
  <<< intercalate "\n"
  <<< map
    ( (if _ then _ else _)
        <$> (eq "module " <<< String.take 7)
        <*> (const "module Main where")
        <*> identity
    )
  <<< String.split (String.Pattern "\n")

foreign import getCurrentText_ :: Effect String
foreign import postToolbarInit_
  :: Foreign
  -> ( Effect Unit
       -> Effect Unit
       -> Effect Unit
       -> Effect Unit
       -> Effect Unit
       -> ((String -> Effect Unit) -> Effect Unit)
       -> Effect Unit
     )
  -> Effect Unit

foreign import getAwfulHack_ :: Effect (Unit -> Effect Unit)

data PlayingState
  = Playing
      { audioCtx :: AudioContext
      , unsubscribe :: Effect Unit
      }
  | Loading { unsubscribe :: Effect Unit }
  | Stopped

minLoading :: Effect Unit -> PlayingState -> PlayingState
minLoading unsubscribe Stopped = Loading { unsubscribe }
minLoading unsubscribe (Loading _) = Loading { unsubscribe }
minLoading unsubscribe (Playing { audioCtx }) = Playing { unsubscribe, audioCtx }

minPlay :: AudioContext -> PlayingState -> PlayingState
minPlay audioCtx Stopped = Playing { audioCtx, unsubscribe: pure unit }
minPlay audioCtx (Loading { unsubscribe }) = Playing { audioCtx, unsubscribe }
minPlay _ a = a

loaderUrl :: String
loaderUrl = "https://purescript-wags.netlify.app/js/output"

compileUrl :: String
compileUrl = "https://supvghemaw.eu-west-1.awsapprunner.com"

loader = makeLoader loaderUrl :: Loader

compileErrorsToString :: Array API.CompilerError -> String
compileErrorsToString = intercalate "\n" <<< map \err ->
  maybe "" (\position -> "On line " <> show position.startLine <> ":\n") (toMaybe err.position)
    <> err.message
    <> "\n\n"

foreign import setErrorText_ :: String -> Effect Unit

postToolbarInitInternal :: Foreign -> Effect Unit
postToolbarInitInternal args = do
  modulesR <- freshModules >>= Ref.new
  bufferCache <- Ref.new Map.empty
  playingState <- Ref.new Stopped
  postToolbarInit_ args \setAlert removeAlert onLoad onStop onPlay setAwfulHack -> Ref.read playingState >>=
    case _ of
      Stopped -> do
        onLoad
        Ref.write (Loading { unsubscribe: pure unit }) playingState
        { event, push } <- create
        setAwfulHack push
        nextUpR <- Ref.new (pure unit :: Fiber Unit)
        let
          crunch :: AudioContext -> String -> (AFuture -> Effect Unit) -> Aff Unit
          crunch audioCtx txt nextWag = do
            -- todo: can compile be fiberized to be faster?
            -- Log.info "step 1"
            res <- try $ makeAff \cb -> do
              removeAlert
              compile
                { code: sanitizePS txt
                , loader
                , compileUrl
                , ourFaultErrorCallback: fold
                    <<< mapFlipped
                      [ setErrorText_ <<< show
                      , cb <<< Left
                      ]
                    <<< (#)
                , yourFaultErrorCallback: fold
                    <<< mapFlipped
                      [ setErrorText_ <<< compileErrorsToString
                      , cb <<< Left <<< error <<< show
                      ]
                    <<< (#)

                , successCallback: cb <<< Right <<< _.js
                }
              mempty
            -- Log.info "step 2"
            res # either
              ( \_ -> liftEffect do
                  setAlert
                  -- in case we are not playing yet
                  -- we set on play
                  onPlay
              )
              \js -> liftEffect do
                modules <- Ref.read modulesR
                o <- evalSources modules js
                wag' <- (runExceptT $ readProp "wag" o.evaluated)
                  >>= either (throwError <<< error <<< show) pure
                Ref.write o.modules modulesR
                let wag = (unsafeCoerce :: Foreign -> AFuture) wag'
                launchAff_ do
                  doDownloads audioCtx bufferCache mempty identity wag
                  liftEffect do
                    -- Log.info "pushing next wag"
                    nextWag wag
                    st <- Ref.read playingState
                    case st of
                      Loading _ -> onPlay
                      _ -> pure unit
                    Ref.modify_ (minPlay audioCtx) playingState

        -- Log.info "step 4"
        --------------
        --------------

        let
          wagEvent audioCtx = makeEvent \nextWag -> do
            subscribe event \_ -> do
              current <- Ref.read nextUpR
              fib <- launchAff do
                killFiber (error "Could not kill fiber") current
                delay (Milliseconds 400.0)
                txt <- liftEffect getCurrentText_
                crunch audioCtx txt nextWag
              Ref.write fib nextUpR

        audioCtx <- context
        waStatus <- liftEffect $ contextState audioCtx
        -- void the constant 0 hack
        -- this will result in a very slight performance decrease but makes iOS and Mac more sure
        _ <- liftEffect $ constant0Hack audioCtx
        ffiAudio <- liftEffect $ makeFFIAudioSnapshot audioCtx
        launchAff_ do
          when (waStatus /= "running") (toAffE $ contextResume audioCtx)
          let
            FullSceneBuilder { triggerWorld, piece } =
              engine
                (pure unit)
                (map (const <<< const) (wagEvent audioCtx))
                (Left (r2b bufferCache))
          trigger /\ world <- snd $ triggerWorld (audioCtx /\ (pure (pure {} /\ pure {})))
          liftEffect do
            unsubscribe <- subscribe
              (run trigger world { easingAlgorithm } ffiAudio piece)
              ( \(_ :: Run TidalRes Analysers) -> do
                  pure unit
              )
            Ref.modify_ (minLoading unsubscribe) playingState
            txt <- getCurrentText_
            push txt
      Loading _ -> mempty
      Playing { audioCtx, unsubscribe } -> do
        unsubscribe
        close audioCtx
        setAwfulHack mempty
        Ref.write Stopped playingState
        onStop

postToolbarInit :: Fn2 Foreign Foreign Unit
postToolbarInit = mkFn2
  \_ args -> unsafePerformEffect do
    postToolbarInitInternal args

acePostWriteDomLineHTML :: Fn3 Foreign Foreign (Effect Unit) Unit
acePostWriteDomLineHTML = mkFn3
  \_ _ cb -> unsafePerformEffect do
    hack <- getAwfulHack_
    hack unit
    cb