module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ -- common
      NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoImportingEverything.rule []
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule

    -- unused
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule

    -- I'd like to use this, but it doesn't detect uses through `Expect.equal`,
    -- only pattern matching.
    -- , NoUnused.CustomTypeConstructorArgs.rule
    ]
