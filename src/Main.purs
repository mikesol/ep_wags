module Main
  ( acePostWriteDomLineHTML
  , postToolbarInit
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Except (runExceptT, throwError)
import Control.Promise (toAffE)
import Data.Array (findMap, intercalate, (!!))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3)
import Data.Functor (mapFlipped)
import Data.Lens (set)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.Number as DN
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
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
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodeUnits (whiteSpace, char, oneOf)
import Text.Parsing.StringParser.CodeUnits as ParserCU
import Text.Parsing.StringParser.Combinators (option)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Interpret (close, constant0Hack, context, contextResume, contextState, makeFFIAudioSnapshot)
import WAGS.Lib.Learn (Analysers, FullSceneBuilder(..))
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Tidal as T
import WAGS.Lib.Tidal.Types (NextCycle(..), TheFuture(..), TidalRes, Voice(..))
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

data InputType = DPureScript | DText

strToInputType :: String -> InputType
strToInputType s
  | String.indexOf (String.Pattern "module ") s /= Nothing = DPureScript
  | otherwise = DText

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
       -> ((Unit -> Effect Unit) -> Effect Unit)
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

stripComment :: String -> { withoutComments :: String, comment :: Maybe String }
stripComment = String.split (String.Pattern "#") >>> case _ of
  [] -> { withoutComments: "", comment: Nothing }
  [ a ] -> { withoutComments: a, comment: Nothing }
  x -> { withoutComments: fromMaybe "" (x !! 0), comment: Just $ intercalate "#" $ Array.drop 1 x }

stripComments :: String -> { withoutComments :: String, comments :: Array String }
stripComments s = { withoutComments: intercalate "\n" (map _.withoutComments toComments), comments: compact $ map _.comment toComments }
  where
  toComments = map stripComment $ String.split (String.Pattern "\n") s

---- uggggh
c2str ∷ Char → Parser String
c2str = pure <<< fromCharArray <<< Array.singleton

ca2str ∷ Array Char → Parser String
ca2str = pure <<< fromCharArray

negativeSign ∷ Parser String
negativeSign = char '-' >>= c2str

digits :: Array Char
digits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

nonZeroDigits :: Array Char
nonZeroDigits = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

ip0 ∷ Parser String
ip0 = (<>) <$> (option "" negativeSign) <*> (char '0' >>= c2str)

ipOther ∷ Parser String
ipOther =
  fold
    <$> sequence
      [ option "" negativeSign
      , oneOf nonZeroDigits >>= c2str
      , Array.many (oneOf digits) >>= ca2str
      ]

integerPart ∷ Parser String
integerPart = (Parser.try ip0) <|> ipOther

fractionalPart ∷ Parser String
fractionalPart =
  (<>)
    <$> (char '.' >>= c2str)
    <*> (Array.many (oneOf digits) >>= ca2str)

floatValueFrac ∷ Parser String
floatValueFrac =
  (<>)
    <$> integerPart
    <*> fractionalPart

exponentPart ∷ Parser String
exponentPart =
  fold
    <$> sequence
      [ oneOf [ 'e', 'E' ] >>= c2str
      , option "" (oneOf [ '+', '-' ] >>= c2str)
      , Array.some (oneOf digits) >>= ca2str
      ]

floatValueExp ∷ Parser String
floatValueExp = (<>) <$> integerPart <*> exponentPart

floatValueFracExp ∷ Parser String
floatValueFracExp =
  fold
    <$> sequence [ integerPart, fractionalPart, exponentPart ]

floatValue ∷ Parser Number
floatValue = (Parser.try floatValueFracExp <|> Parser.try floatValueExp <|> floatValueFrac) >>= maybe (fail "String not a float") pure <<< DN.fromString

getDuration :: Array String -> Maybe Number
getDuration = findMap durationParser
  where
  durationParser = hush <<< runParser
    ( whiteSpace
        *> ParserCU.string "@duration"
        *> whiteSpace
        *> floatValue
    )

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
        nextUpR <- Ref.new (pure unit :: Fiber Unit)
        let
          pushWagAndCarryOn :: AudioContext -> AFuture -> (AFuture -> Effect Unit) -> Aff Unit
          pushWagAndCarryOn audioCtx wag nextWag = do
            -- Log.info "pushing next wag"
            doDownloads audioCtx bufferCache mempty identity wag
            liftEffect do
              nextWag wag
              st <- Ref.read playingState
              case st of
                Loading _ -> onPlay
                _ -> pure unit
              Ref.modify_ (minPlay audioCtx) playingState

          crunch :: AudioContext -> String -> (AFuture -> Effect Unit) -> Aff Unit
          crunch audioCtx txt nextWag = do
            -- todo: can compile be fiberized to be faster?
            -- Log.info "step 1"
            res <- try $ makeAff \cb -> do
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
                launchAff_ $ pushWagAndCarryOn audioCtx wag nextWag
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
                -- need to remove alert as well
                txt <- liftEffect $ removeAlert *> getCurrentText_
                let itype = strToInputType txt
                case itype of
                  DPureScript -> crunch audioCtx txt nextWag
                  DText -> do
                    let
                      modText = stripComments $ sanitizeUsingRegex_ txt
                      duration = fromMaybe 1.0 (getDuration modText.comments)
                      fut = T.make duration { earth: T.s $ String.trim modText.withoutComments }
                    pushWagAndCarryOn audioCtx
                      ( set
                          ( unto TheFuture
                              <<< prop (Proxy :: _ "earth")
                              <<< unto Voice
                              <<< prop (Proxy :: _ "next")
                              <<< unto NextCycle
                              <<< prop (Proxy :: _ "force")
                          )
                          true
                          fut
                      )
                      nextWag
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
            -- start the machine
            push unit
            -- set the pusher
            setAwfulHack push
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
