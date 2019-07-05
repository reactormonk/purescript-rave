{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "rave"
, dependencies =
    [ "aff"
    , "checked-exceptions"
    , "console"
    , "effect"
    , "exceptions"
    , "record"
    , "transformers"
    , "typelevel-prelude"
    , "variant"
    ]
, packages =
    ./packages.dhall
}
