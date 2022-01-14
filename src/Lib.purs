module Lib
  ( acePostWriteDomLineHTML
  , postToolbarInit
  , aceKeyEvent
  , initF
  , setUpIosAudio
  , chatNewMessage
  , chatSendMessage
  , InitSig
  , PlayingState(..)
  ) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Except (runExceptT, throwError)
import Control.Promise (toAffE)
import Data.Array (findMap, intercalate, (!!))
import Data.Array as Array
import Data.Compactable (compact)
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, for_)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3)
import Data.Functor (mapFlipped)
import Data.HTTP.Method (Method(..))
import Data.Int as DI
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Nullable (toMaybe)
import Data.Number as DN
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested ((/\))
import Data.Variant.Maybe as VM
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (Event, create, makeEvent, subscribe)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object as Object
import JIT.API as API
import JIT.Compile (compile)
import JIT.EvalSources (Modules, evalSources, freshModules)
import JIT.Loader (Loader, makeLoader)
import Simple.JSON as JSON
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodeUnits (anyDigit, anyLetter, char, oneOf, whiteSpace)
import Text.Parsing.StringParser.CodeUnits as ParserCU
import Text.Parsing.StringParser.Combinators (many1, option)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Interpret (FFIAudioSnapshot, close, constant0Hack, context, contextResume, contextState, makeFFIAudioSnapshot)
import WAGS.Lib.Learn (Analysers, FullSceneBuilder(..))
import WAGS.Lib.Tidal (AFuture)
import WAGS.Lib.Tidal.Cycle (bd)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Tidal (drone, openFuture, parseWithBrackets)
import WAGS.Lib.Tidal.Tidal as T
import WAGS.Lib.Tidal.Types (BufferUrl(..), emptyCtrl, TidalRes, SampleCache)
import WAGS.Lib.Tidal.Types as TT
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

type InitSig =
  Effect Unit
  -> Effect Unit
  -> Effect Unit
  -> Effect Unit
  -> Effect Unit
  -> ((Boolean -> Effect Unit) -> Effect Unit)
  -> Effect Unit

foreign import getCurrentText_ :: Effect String
foreign import postToolbarInit_ :: Foreign -> InitSig -> Effect (Effect Unit)

foreign import getAwfulHack_ :: Effect (Boolean -> Effect Unit)
foreign import getPlayKey_ :: Effect (Unit -> Effect Unit)
foreign import setPlayKey_ :: (Unit -> Effect Unit) -> Effect Unit
data PlayingState
  = Playing
      { audioCtx :: AudioContext
      , unsubscribe :: Effect Unit
      }
  | Loading { unsubscribe :: Effect Unit }
  | Stopped

minLoading :: Effect Unit -> PlayingState -> PlayingState
minLoading unsubscribe Stopped = Loading { unsubscribe }
minLoading aa (Loading { unsubscribe }) = Loading { unsubscribe: aa *> unsubscribe }
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

intValue ∷ Parser Int
intValue = integerPart >>= maybe (fail "String not an int") pure <<< DI.fromString

fauxUrl :: Parser String
fauxUrl = fromCharArray <<< Array.fromFoldable <$>
  ( whiteSpace
      *> many1 (Parser.try anyDigit <|> Parser.try anyLetter <|> oneOf [ ':', '\\', '/', '@', '#', '%', '.', '-', '_' ])
  )

getSamples :: Array String -> Object BufferUrl
getSamples = map BufferUrl
  <<< fold
  <<< compact
  <<< map sampleParser
  where
  sampleParser =
    (map (uncurry Object.singleton))
      <<< hush
      <<< runParser
        ( Tuple
            <$>
              ( fromCharArray <<< Array.fromFoldable <$>
                  ( whiteSpace
                      *> ParserCU.string "@sample"
                      *> whiteSpace
                      *> many1 (Parser.try anyDigit <|> Parser.try anyLetter <|> char ':')
                  )
              )
            <*> fauxUrl
        )

getDrone :: Array String -> Maybe String
getDrone = findMap droneParser
  where
  droneParser = hush <<< runParser
    ( fromCharArray <<< Array.fromFoldable <$>
        ( whiteSpace
            *> ParserCU.string "@drone"
            *> whiteSpace
            *> many1 (Parser.try anyDigit <|> Parser.try anyLetter <|> char ':')
        )
    )

getDuration :: Array String -> Maybe Number
getDuration = findMap durationParser
  where
  durationParser = hush <<< runParser
    ( whiteSpace
        *> ParserCU.string "@duration"
        *> whiteSpace
        *> floatValue
    )

parseUsingDefault :: forall event. T.Cycle (VM.Maybe (TT.Note event)) -> String -> T.Cycle (VM.Maybe (TT.Note event))
parseUsingDefault d = fromMaybe d
  <<< hush
  <<< parseWithBrackets

initF
  :: Ref.Ref (T.Cycle (VM.Maybe (TT.Note Unit)))
  -> Ref.Ref PlayingState
  -> Ref.Ref SampleCache
  -> Ref.Ref Modules
  -> Aff Unit
  -> Effect String
  -> InitSig
initF cycleRef playingState bufferCache modulesR checkForAuthorization gcText setAlert removeAlert onLoad onStop onPlay setAwfulHack = Ref.read playingState >>=
  case _ of
    Stopped -> do
      startIosAudio
      onLoad
      Ref.write (Loading { unsubscribe: pure unit }) playingState
      { event, push } <- create
      nextUpR <- Ref.new (pure unit :: Fiber Unit)
      wagRef <- Ref.new (openFuture (wrap 1.0))
      let
        pushWagAndCarryOn :: Boolean -> FFIAudioSnapshot -> AudioContext -> AFuture -> (AFuture -> Effect Unit) -> Aff Unit
        pushWagAndCarryOn shouldStart ffiAudio audioCtx wag nextWag = do
          -- Log.info "pushing next wag"
          doDownloads audioCtx bufferCache mempty identity wag
          liftEffect $ nextWag wag
          when shouldStart do
            let
              FullSceneBuilder { triggerWorld, piece } =
                engine
                  (pure unit)
                  (map (const <<< const) (r2b wagRef))
                  (pure emptyCtrl)
                  (Left (r2b bufferCache))
            trigger /\ world <- snd $ triggerWorld (audioCtx /\ (pure (pure {} /\ pure {})))
            liftEffect do
              unsubscribe <- subscribe
                (run trigger world { easingAlgorithm } ffiAudio piece)
                ( \(_ :: Run TidalRes Analysers) -> do
                    st <- Ref.read playingState
                    Ref.modify_ (minPlay audioCtx) playingState
                    case st of
                      Loading _ -> onPlay
                      _ -> pure unit
                )
              Ref.modify_ (minLoading unsubscribe) playingState

        crunch :: Boolean -> FFIAudioSnapshot -> AudioContext -> String -> (AFuture -> Effect Unit) -> Aff Unit
        crunch shouldStart ffiAudio audioCtx txt nextWag = do
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
              launchAff_ $ pushWagAndCarryOn shouldStart ffiAudio audioCtx wag nextWag
      -- Log.info "step 4"
      --------------
      --------------

      launchAff_ do
        checkForAuthorization
        audioCtx <- liftEffect $ context
        waStatus <- liftEffect $ contextState audioCtx
        -- void the constant 0 hack
        -- this will result in a very slight performance decrease but makes iOS and Mac more sure
        ffiAudio <- liftEffect $ makeFFIAudioSnapshot audioCtx
        when (waStatus /= "running") (toAffE $ contextResume audioCtx)
        _ <- liftEffect $ constant0Hack audioCtx
        usb <- liftEffect $ subscribe event \shouldStart -> do
          current <- Ref.read nextUpR
          fib <- launchAff do
            killFiber (error "Could not kill fiber") current
            when (not shouldStart) $ delay (Milliseconds 400.0)
            -- need to remove alert as well
            txt <- liftEffect $ removeAlert *> gcText
            let itype = strToInputType txt
            let nextWag = flip Ref.write wagRef
            case itype of
              DPureScript -> crunch shouldStart ffiAudio audioCtx txt nextWag
              DText -> do
                prevCyc <- liftEffect $ Ref.read cycleRef
                let
                  modText = stripComments $ sanitizeUsingRegex_ txt
                  duration = fromMaybe 1.0 (getDuration modText.comments)
                  samples = getSamples modText.comments
                  drone' = maybe VM.nothing VM.just
                    $ getDrone modText.comments
                  newCyc = parseUsingDefault prevCyc (String.trim modText.withoutComments)
                  fut = T.make duration
                    { earth: T.s newCyc
                    , sounds: samples
                    , heart: join $ map drone drone'
                    , title: "Local play"
                    }
                liftEffect $ Ref.write newCyc cycleRef
                pushWagAndCarryOn shouldStart ffiAudio audioCtx fut nextWag

          Ref.write fib nextUpR

        -- start the machine
        -- set the pusher
        liftEffect do
          Ref.modify_ (minLoading usb) playingState
          setAwfulHack push
          push true

    Loading _ -> mempty
    Playing { audioCtx, unsubscribe } -> do
      stopIosAudio
      close audioCtx
      unsubscribe
      setAwfulHack mempty
      Ref.write Stopped playingState
      onStop

foreign import setUpIosAudio :: Effect Unit
foreign import startIosAudio :: Effect Unit
foreign import stopIosAudio :: Effect Unit

postToolbarInitInternal :: Event Unit -> Foreign -> Effect Unit
postToolbarInitInternal ctrlPEvt args = do
  liftEffect $ setUpIosAudio
  modulesR <- freshModules >>= Ref.new
  bufferCache <- Ref.new Object.empty
  playingState <- Ref.new Stopped
  cycleRef <- Ref.new bd
  cb <- postToolbarInit_ args
    (initF cycleRef playingState bufferCache modulesR (pure unit) getCurrentText_)
  -- never unsubscribe from key event for now
  -- change later?
  _ <- subscribe ctrlPEvt \_ -> do
    -- Log.info "playing"
    cb
  pure unit

foreign import wireUpCtrlP_ :: (Unit -> Effect Unit) -> Effect Unit

postToolbarInit :: Fn2 Foreign Foreign Unit
postToolbarInit = mkFn2
  \_ args -> unsafePerformEffect do
    { event, push } <- create
    wireUpCtrlP_ push
    setPlayKey_ push
    postToolbarInitInternal event args

foreign import isCtrlG :: Foreign -> Effect Boolean

aceKeyEvent :: Fn2 Foreign Foreign Boolean
aceKeyEvent = mkFn2
  \_ args -> unsafePerformEffect do
    icg <- isCtrlG args
    when (icg) do
      hack <- getPlayKey_
      hack unit
    pure icg

acePostWriteDomLineHTML :: Fn3 Foreign Foreign (Effect Unit) Unit
acePostWriteDomLineHTML = mkFn3
  \_ _ cb -> unsafePerformEffect do
    hack <- getAwfulHack_
    hack false
    cb

data BotAction = CallFS { q :: String, p :: Int }

callFS :: Parser BotAction
callFS = do
  _ <- ParserCU.string "fs"
  _ <- whiteSpace
  q <- fromCharArray <<< Array.fromFoldable <$> many1 ParserCU.anyChar
  pure $ CallFS { q, p: 1 }

callFSP :: Parser BotAction
callFSP = do
  _ <- ParserCU.string "fsp"
  _ <- whiteSpace
  p <- intValue
  _ <- whiteSpace
  q <- fromCharArray <<< Array.fromFoldable <$> many1 ParserCU.anyChar
  pure $ CallFS { q, p }

botParser :: Parser BotAction
botParser = do
  _ <- ParserCU.string "@w"
  _ <- whiteSpace
  Parser.try callFS <|> Parser.try callFSP <|> fail "Could not parse string"

type FSFail' = { detail :: String }
type FSResult' =
  { name :: String
  , description :: String
  , previews ::
      { "preview-lq-ogg" :: String
      , "preview-lq-mp3" :: String
      , "preview-hq-ogg" :: String
      , "preview-hq-mp3" :: String
      }
  }

type FSSuccess' =
  { count :: Int
  , results :: Array FSResult'
  }

data FSResponse = FSFail FSFail' | FSSuccess FSSuccess'

instance readFSResponse :: JSON.ReadForeign FSResponse where
  readImpl = (<|>) <$> map FSSuccess <<< JSON.readImpl <*> map FSFail <<< JSON.readImpl

doBotStuff :: String -> Effect Unit
doBotStuff txt = launchAff_ $ do
  let parsed = hush $ runParser botParser txt
  for_ parsed case _ of
    CallFS { p, q } -> do
      res <- AX.request
        ( AX.defaultRequest
            { url = "https://freesound.org/apiv2/search/text/?page=" <> show p <> "&token=fnqb3U00p5fmEOZSiwGyTLS2ZwYPkygJ7b8KjVEi&query=" <> q <> "&fields=name,description,previews"
            , method = Left GET
            , responseFormat = ResponseFormat.string
            }
        )
      case res of
        Left err -> do
          liftEffect $ Log.error "Request did not go through"
          throwError (error $ AX.printError err)
        Right response -> case (JSON.readJSON response.body) of
          Left err1 -> throwError (error $ ("Got an error " <> show response.body <> " err " <> show err1))
          Right (fsResponse :: FSResponse) -> case fsResponse of
            FSFail _ -> Log.error "Request did not go through"
            FSSuccess { results } -> liftEffect
              $ sendChatMessage_
              $ intercalate "\n\n"
              $ map formatResultForChat results

formatResultForChat :: FSResult' -> String
formatResultForChat { name, description, previews: { "preview-hq-ogg": ogg } } = "Name: " <> name <> "\nDescription: " <> String.take 30 description <> "..." <> "\nOgg: " <> ogg

foreign import sendChatMessage_ :: String -> Effect Unit

chatSendMessage :: Fn3 Foreign Foreign (Effect Unit) Unit
chatSendMessage = mkFn3 \_ ctx cb -> unsafePerformEffect do
  let txt = JSON.read_ ctx :: Maybe { message :: { text :: String } }
  for_ txt $ _.message.text >>> doBotStuff
  cb

chatNewMessage :: Fn3 Foreign Foreign (Effect Unit) Unit
chatNewMessage = mkFn3 \_ ctx cb -> unsafePerformEffect do
  let txt = JSON.read_ ctx :: Maybe { text :: String }
  for_ txt $ _.text >>> doBotStuff
  cb
