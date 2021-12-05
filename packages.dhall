let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let overrides = {=}

let additions =
      { typelevel-peano =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/csicar/purescript-typelevel-peano.git"
        , version = "v1.0.1"
        }
      , jit =
        { dependencies = [ "affjax" ]
        , repo = "https://github.com/mikesol/purescript-jit.git"
        , version = "v0.0.3"
        }
      , nonbili-dom =
        { repo = "https://github.com/nonbili/purescript-nonbili-dom.git"
        , dependencies = [ "web-html", "effect" ]
        , version = "v0.2.0"
        }
      , svg-parser =
        { repo = "https://github.com/rnons/purescript-svg-parser.git"
        , version = "v2.0.0"
        , dependencies = [ "string-parsers" ]
        }
      , svg-parser-halogen =
        { repo = "https://github.com/rnons/purescript-svg-parser-halogen.git"
        , version = "v2.0.0"
        , dependencies = [ "halogen", "svg-parser" ]
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.4.2"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.1.0"
        }
      , wags =
        { dependencies =
          [ "aff-promise"
          , "arraybuffer-types"
          , "behaviors"
          , "console"
          , "convertable-options"
          , "debug"
          , "effect"
          , "event"
          , "free"
          , "heterogeneous"
          , "indexed-monad"
          , "maybe"
          , "ordered-collections"
          , "profunctor-lenses"
          , "psci-support"
          , "record"
          , "simple-json"
          , "sized-vectors"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/mikesol/purescript-wags.git"
        , version = "v0.6.2"
        }
      , free =
        { dependencies =
          [ "catenable-lists"
          , "control"
          , "distributive"
          , "either"
          , "exists"
          , "foldable-traversable"
          , "invariant"
          , "lazy"
          , "maybe"
          , "prelude"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-free.git"
        , version = "master"
        }
      , wags-lib =
        { dependencies =
          [ "wags"
          , "run"
          , "halogen"
          , "halogen-css"
          , "homogeneous"
          , "string-parsers"
          , "strings"
          ]
        , repo = "https://github.com/mikesol/purescript-wags-lib.git"
        , version = "v0.0.68"
        }
      , painting =
        { dependencies =
          [ "canvas"
          , "colors"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "foreign-object"
          , "psci-support"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-painting.git"
        , version = "v0.0.0"
        }
      , framer-motion =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arrays"
          , "console"
          , "effect"
          , "foreign"
          , "foreign-object"
          , "heterogeneous"
          , "literals"
          , "maybe"
          , "nullable"
          , "prelude"
          , "psci-support"
          , "react-basic"
          , "react-basic-dom"
          , "react-basic-hooks"
          , "record"
          , "tuples"
          , "two-or-more"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "untagged-union"
          , "web-dom"
          , "web-events"
          , "web-uievents"
          ]
        , repo =
            "https://github.com/i-am-the-slime/purescript-framer-motion.git"
        , version = "v0.1.0"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      }

in  upstream // overrides // additions
