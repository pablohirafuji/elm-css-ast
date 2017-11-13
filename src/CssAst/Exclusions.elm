module CssAst.Exclusions
    exposing
        ( Declaration(..)
        , declarations
        , WrapFlow(..)
        , WrapThrough(..)
        )

{-| [CSS Exclusions](https://drafts.csswg.org/css-exclusions-1/)

@docs Declaration, WrapFlow, WrapThrough, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


{-| The Exclusions [declarations](https://drafts.csswg.org/css-exclusions-1/#property-index) type.
-}
type Declaration
    = WrapFlow WrapFlow
    | WrapThrough WrapThrough


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Declaration )
declarations =
    [ ( "wrap-flow", wrapFlow |> map WrapFlow )
    , ( "wrap-through", wrapThrough |> map WrapThrough )
    ]


{-| The [`wrap-flow`](https://drafts.csswg.org/css-exclusions-1/#propdef-wrap-flow) property type.

Value definition syntax:

    auto | both | start | end | minimum | maximum | clear

-}
type WrapFlow
    = Auto
    | Both
    | Start
    | End
    | Minimum
    | Maximum
    | Clear


wrapFlow : Parser WrapFlow
wrapFlow =
    keywordsToType
        [ ( "auto", Auto )
        , ( "both", Both )
        , ( "start", Start )
        , ( "end", End )
        , ( "minimum", Minimum )
        , ( "maximum", Maximum )
        , ( "clear", Clear )
        ]


{-| The [`wrap-through`](https://drafts.csswg.org/css-exclusions-1/#propdef-wrap-through) property type.

Value definition syntax:

    wrap | none

-}
type WrapThrough
    = Wrap
    | None


wrapThrough : Parser WrapThrough
wrapThrough =
    keywordsToType
        [ ( "wrap", Wrap )
        , ( "none", None )
        ]
