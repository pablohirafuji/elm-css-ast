module CssAst.Cascade
    exposing
        ( Cascade(..)
        , declarations
        )

{-| [CSS Cascading and Inheritance](https://drafts.csswg.org/css-cascade-4/)

@docs Cascade, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)
import CssAst.Values as V


{-| The Cascade type.

[Source](https://drafts.csswg.org/css-cascade-4/#property-index).

-}
type Cascade
    = All All


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Cascade )
declarations =
    [ ( "all", all |> map All )
    ]


{-| The [`all`](https://drafts.csswg.org/css-cascade-4/#propdef-all) property type.

Value definition syntax:

    initial | inherit | unset | revert

-}
type All
    = Initial
    | Inherit
    | Unset
    | Revert


all : Parser All
all =
    keywordsToType
        [ ( "initial", Initial )
        , ( "inherit", Inherit )
        , ( "unset", Unset )
        , ( "revert", Revert )
        ]
