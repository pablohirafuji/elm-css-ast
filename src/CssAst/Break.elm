module CssAst.Break
    exposing
        ( Break(..)
        , declarations
        , BoxDecorationBreak(..)
        , BreakAfterBefore(..)
        , BreakInside(..)
        )

{-| [CSS Fragmentation Module](https://drafts.csswg.org/css-break-3/)

@docs Break, BoxDecorationBreak,BreakAfterBefore,BreakInside, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail)
import CssAst.Helpers exposing (keywordsToType)
import CssAst.Values exposing (integer)


{-| The Break type.

[Source](https://drafts.csswg.org/css-break-3/#property-index).

-}
type Break
    = BoxDecorationBreak BoxDecorationBreak
    | BreakAfter BreakAfterBefore
    | BreakBefore BreakAfterBefore
    | BreakInside BreakInside
    | Orphans Int
    | Widows Int


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Break )
declarations =
    [ ( "box-decoration-break", map BoxDecorationBreak boxDecorationBreak )
    , ( "break-after", map BreakAfter breakAfterBefore )
    , ( "break-before", map BreakBefore breakAfterBefore )
    , ( "break-inside", map BreakInside breakInside )
    , ( "orphans", map Orphans integer )
    , ( "widows", map Widows integer )
    ]


{-| The possible values for the [`box-decoration-break`](https://drafts.csswg.org/css-break-3/#propdef-box-decoration-break) property.

Value definition syntax:
`slice | clone`

-}
type BoxDecorationBreak
    = Slice
    | Clone


boxDecorationBreak : Parser BoxDecorationBreak
boxDecorationBreak =
    keywordsToType
        [ ( "slice", Slice )
        , ( "clone", Clone )
        ]


{-| The possible values for the [`break-after`](https://drafts.csswg.org/css-break-3/#propdef-break-after) and [`break-before`](https://drafts.csswg.org/css-break-3/#propdef-break-before) properties.

Value definition syntax:
`auto | avoid | avoid-page | page | left | right | recto | verso | avoid-column | column | avoid-region | region`

-}
type BreakAfterBefore
    = Auto
    | Avoid
    | AvoidPage
    | Page
    | Left
    | Right
    | Recto
    | Verso
    | AvoidColumn
    | Column
    | AvoidRegion
    | Region


breakAfterBefore : Parser BreakAfterBefore
breakAfterBefore =
    keywordsToType
        [ ( "auto", Auto )
        , ( "avoid", Avoid )
        , ( "avoid-page", AvoidPage )
        , ( "page", Page )
        , ( "left", Left )
        , ( "right", Right )
        , ( "recto", Recto )
        , ( "verso", Verso )
        , ( "avoid-column", AvoidColumn )
        , ( "column", Column )
        , ( "avoid-region", AvoidRegion )
        , ( "region", Region )
        ]


{-| The possible values for the [`break-inside`](https://drafts.csswg.org/css-break-3/#propdef-break-inside) property.

Value definition syntax:
`auto | avoid | avoid-page | avoid-column | avoid-region`

-}
type BreakInside
    = BIAuto
    | BIAvoid
    | BIAvoidPage
    | BIAvoidColumn
    | BIAvoidRegion


breakInside : Parser BreakInside
breakInside =
    keywordsToType
        [ ( "auto", BIAuto )
        , ( "avoid", BIAvoid )
        , ( "avoid-page", BIAvoidPage )
        , ( "avoid-column", BIAvoidColumn )
        , ( "avoid-region", BIAvoidRegion )
        ]
