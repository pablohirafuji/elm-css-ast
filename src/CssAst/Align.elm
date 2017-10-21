module CssAst.Align
    exposing
        ( Align(..)
        , declarations
        , AlignContent(..)
        , JustifyContent(..)
        , JustifyContentPosition(..)
        , BaselinePosition(..)
        , ContentDistribution(..)
        , OverflowPosition(..)
        , ContentPosition(..)
        , JustifySelf(..)
        , JustifySelfPosition(..)
        , AlignSelf(..)
        , SelfPosition(..)
        , JustifyItems(..)
        , JustifyItemsPosition(..)
        , JustifyItemsPositionSet(..)
        , AlignItems(..)
        , PlaceItemsValue(..)
        , GapValue(..)
        )

{-| [CSS Box Alignment](https://drafts.csswg.org/css-align-3/)

@docs Align, AlignContent, JustifyContent, JustifyContentPosition, BaselinePosition, ContentDistribution, OverflowPosition, JustifySelf, JustifySelfPosition, AlignSelf, SelfPosition, JustifyItems, JustifyItemsPosition, JustifyItemsPositionSet, AlignItems, PlaceItemsValue, GapValue, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail)
import CssAst.Helpers exposing (whitespace)
import CssAst.Values exposing (LengthPercentage, lengthPercentage)


{-| The Align type.

[Source](https://drafts.csswg.org/css-align-3/#property-index).

-}
type Align
    = AlignContent AlignContent
    | JustifyContent JustifyContent
    | PlaceContent AlignContent (Maybe JustifyContent)
    | JustifySelf JustifySelf
    | AlignSelf AlignSelf
    | PlaceSelf AlignSelf (Maybe JustifySelf)
    | JustifyItems JustifyItems
    | AlignItems AlignItems
    | PlaceItems PlaceItemsValue (Maybe PlaceItemsValue)
    | RowGap GapValue
    | ColumnGap GapValue
    | Gap GapValue (Maybe GapValue)


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Align )
declarations =
    [ ( "align-content", map AlignContent alignContent )
    , ( "justify-content", map JustifyContent justifyContent )
    , ( "place-content", placeContent )
    , ( "justify-self", map JustifySelf justifySelf )
    , ( "align-self", map AlignSelf alignSelf )
    , ( "place-self", placeSelf )
    , ( "justify-items", map JustifyItems justifyItems )
    , ( "align-items", map AlignItems alignItems )
    , ( "place-items", placeItems )
    , ( "row-gap", map RowGap gapValue )
    , ( "column-gap", map ColumnGap gapValue )
    , ( "gap", gap )

    -- For compatibility with legacy content, those legacy property names must be supported as aliases
    -- https://drafts.csswg.org/css-align-3/#gap-legacy
    , ( "grid-row-gap", map RowGap gapValue )
    , ( "grid-column-gap", map ColumnGap gapValue )
    , ( "grid-gap", gap )
    ]


{-| The possible values for the [`align-content`](https://drafts.csswg.org/css-align-3/#propdef-align-content) property.

Value definition syntax:
`normal | <baseline-position> | <content-distribution> | <overflow-position>? <content-position>`

-}
type AlignContent
    = ACNormal
    | ACBP BaselinePosition
    | ACCD ContentDistribution
    | ACCP (Maybe OverflowPosition) ContentPosition


alignContent : Parser AlignContent
alignContent =
    oneOf
        [ keyword "normal" |> map (always ACNormal)
        , baselinePosition |> map ACBP
        , contentDistribution |> map ACCD
        , maybeOverflowPositionHelp contentPosition ACCP
        , fail "Invalid `align-content` property value."
        ]


{-| The possible values for the [`justify-content`](https://drafts.csswg.org/css-align-3/#propdef-justify-content) property.

Value definition syntax:
`normal | <content-distribution> | <overflow-position>? [ <content-position> | left | right ]`

-}
type JustifyContent
    = JCNormal
    | JCCD ContentDistribution
    | JCP (Maybe OverflowPosition) JustifyContentPosition


justifyContent : Parser JustifyContent
justifyContent =
    oneOf
        [ keyword "normal" |> map (always JCNormal)
        , contentDistribution |> map JCCD
        , maybeOverflowPositionHelp justifyContentPosition JCP
        , fail "Invalid `justify-content` property value."
        ]


{-| The possible values for the [`justify-content`](https://drafts.csswg.org/css-align-3/#propdef-justify-content) data type.

Value definition syntax:
`<content-position> | left | right`

-}
type JustifyContentPosition
    = JCCP ContentPosition
    | Left
    | Right


justifyContentPosition : Parser JustifyContentPosition
justifyContentPosition =
    oneOf
        [ contentPosition |> map JCCP
        , keyword "left" |> map (always Left)
        , keyword "right" |> map (always Right)
        ]


placeContent : Parser Align
placeContent =
    succeed PlaceContent
        |= alignContent
        |. whitespace
        |= oneOf
            [ justifyContent |> map Just
            , succeed Nothing
            ]


{-| The possible values for the [`<baseline-position>`](https://drafts.csswg.org/css-align-3/#typedef-baseline-position) data type.

Value definition syntax:
`[ first | last ]? baseline`

-}
type BaselinePosition
    = Baseline
    | FirstBaseline
    | LastBaseline


baselinePosition : Parser BaselinePosition
baselinePosition =
    oneOf
        [ keyword "baseline" |> map (always Baseline)
        , succeed FirstBaseline
            |. keyword "first"
            |. whitespace
            |. keyword "baseline"
        , succeed LastBaseline
            |. keyword "last"
            |. whitespace
            |. keyword "baseline"
        ]


{-| The possible values for the [`<content-distribution>`](https://drafts.csswg.org/css-align-3/#typedef-content-distribution) data type.

Value definition syntax:
`space-between | space-around | space-evenly | stretch`

-}
type ContentDistribution
    = SpaceBetween
    | SpaceAround
    | SpaceEvenly
    | Stretch


contentDistribution : Parser ContentDistribution
contentDistribution =
    oneOf
        [ keyword "space-between" |> map (always SpaceBetween)
        , keyword "space-around" |> map (always SpaceAround)
        , keyword "space-evenly" |> map (always SpaceEvenly)
        , keyword "stretch" |> map (always Stretch)
        ]


{-| The possible values for the [`<overflow-position>`](https://drafts.csswg.org/css-align-3/#typedef-overflow-position) data type.

Value definition syntax:
`unsafe | safe`

-}
type OverflowPosition
    = Safe
    | Unsafe


overflowPosition : Parser OverflowPosition
overflowPosition =
    oneOf
        [ keyword "safe" |> map (always Safe)
        , keyword "unsafe" |> map (always Unsafe)
        ]


{-| The possible values for the [`<content-position>`](https://drafts.csswg.org/css-align-3/#typedef-content-position) data type.

Value definition syntax:
`center | start | end | flex-start | flex-end`

-}
type ContentPosition
    = Center
    | Start
    | End
    | FlexStart
    | FlexEnd


contentPosition : Parser ContentPosition
contentPosition =
    oneOf
        [ keyword "center" |> map (always Center)
        , keyword "start" |> map (always Start)
        , keyword "end" |> map (always End)
        , keyword "flex-start" |> map (always FlexStart)
        , keyword "flex-end" |> map (always FlexEnd)
        ]


{-| The possible values for the [`justify-self`](https://drafts.csswg.org/css-align-3/#propdef-justify-self) property.

Value definition syntax:
`auto | normal | stretch | <baseline-position> | <overflow-position>? [ <self-position> | left | right ]`

-}
type JustifySelf
    = JSAuto
    | JSNormal
    | JSStretch
    | JSBP BaselinePosition
    | JSSP (Maybe OverflowPosition) JustifySelfPosition


justifySelf : Parser JustifySelf
justifySelf =
    oneOf
        [ keyword "auto" |> map (always JSAuto)
        , keyword "normal" |> map (always JSNormal)
        , keyword "stretch" |> map (always JSStretch)
        , baselinePosition |> map JSBP
        , maybeOverflowPositionHelp justifySelfPosition JSSP
        ]


{-| The possible values for the [`justify-self`](https://drafts.csswg.org/css-align-3/#propdef-justify-self) self-position.

Value definition syntax:
`[ <self-position> | left | right ]`

-}
type JustifySelfPosition
    = JSP SelfPosition
    | JSPLeft
    | JSPRight


justifySelfPosition : Parser JustifySelfPosition
justifySelfPosition =
    oneOf
        [ selfPosition |> map JSP
        , keyword "left" |> map (always JSPLeft)
        , keyword "right" |> map (always JSPRight)
        ]


{-| The possible values for the [`align-self`](https://drafts.csswg.org/css-align-3/#propdef-align-self) property.

Value definition syntax:
`auto | normal | stretch | <baseline-position> | <overflow-position>? <self-position>`

-}
type AlignSelf
    = ASAuto
    | ASNormal
    | ASStretch
    | ASBP BaselinePosition
    | ASP (Maybe OverflowPosition) SelfPosition


alignSelf : Parser AlignSelf
alignSelf =
    oneOf
        [ keyword "auto" |> map (always ASAuto)
        , keyword "normal" |> map (always ASNormal)
        , keyword "stretch" |> map (always ASStretch)
        , baselinePosition |> map ASBP
        , maybeOverflowPositionHelp selfPosition ASP
        ]


placeSelf : Parser Align
placeSelf =
    succeed PlaceSelf
        |= alignSelf
        |. whitespace
        |= oneOf
            [ justifySelf |> map Just
            , succeed Nothing
            ]


{-| The possible values for the [`self-position`](https://drafts.csswg.org/css-align-3/#typedef-self-position) data type.

Value definition syntax:
`center | start | end | self-start | self-end | flex-start | flex-end`

-}
type SelfPosition
    = SPCenter
    | SPStart
    | SPEnd
    | SelfStart
    | SelfEnd
    | SPFlexStart
    | SPFlexEnd


selfPosition : Parser SelfPosition
selfPosition =
    oneOf
        [ keyword "center" |> map (always SPCenter)
        , keyword "start" |> map (always SPStart)
        , keyword "end" |> map (always SPEnd)
        , keyword "self-start" |> map (always SelfStart)
        , keyword "self-end" |> map (always SelfEnd)
        , keyword "flex-start" |> map (always SPFlexStart)
        , keyword "flex-end" |> map (always SPFlexEnd)
        ]


{-| The possible values for the [`justify-items`](https://drafts.csswg.org/css-align-3/#propdef-justify-items) property.

Value definition syntax:
`normal | stretch | <baseline-position> | [ <overflow-position>? <self-position> ] | [ legacy || [ left | right | center ] ]`

-}
type JustifyItems
    = JINormal
    | JIStretch
    | JIBP BaselinePosition
    | JISP (Maybe OverflowPosition) SelfPosition
    | JIP JustifyItemsPosition


justifyItems : Parser JustifyItems
justifyItems =
    oneOf
        [ keyword "normal" |> map (always JINormal)
        , keyword "stretch" |> map (always JIStretch)
        , baselinePosition |> map JIBP
        , maybeOverflowPositionHelp selfPosition JISP
        , justifyItemsPosition |> map JIP
        ]


type JustifyItemsPosition
    = LegacyOnly
    | PositionOnly JustifyItemsPositionSet
    | LegacyAndPosition JustifyItemsPositionSet


justifyItemsPosition : Parser JustifyItemsPosition
justifyItemsPosition =
    oneOf
        [ succeed
            (Maybe.map LegacyAndPosition
                >> Maybe.withDefault LegacyOnly
            )
            |. keyword "legacy"
            |. whitespace
            |= oneOf
                [ justifyItemsPositionSet |> map Just
                , succeed Nothing
                ]
        , succeed
            (\pos hasLegacy ->
                if hasLegacy then
                    LegacyAndPosition pos
                else
                    PositionOnly pos
            )
            |= justifyItemsPositionSet
            |. whitespace
            |= oneOf
                [ keyword "legacy" |> map (always True)
                , succeed False
                ]
        ]


type JustifyItemsPositionSet
    = JIPLeft
    | JIPRight
    | JIPCenter


justifyItemsPositionSet : Parser JustifyItemsPositionSet
justifyItemsPositionSet =
    oneOf
        [ keyword "left" |> map (always JIPLeft)
        , keyword "right" |> map (always JIPRight)
        , keyword "center" |> map (always JIPCenter)
        ]


{-| The possible values for the [`align-items`](https://drafts.csswg.org/css-align-3/#propdef-align-items) property.

Value definition syntax:
`normal | stretch | <baseline-position> | [ <overflow-position>? <self-position> ]`

-}
type AlignItems
    = AINormal
    | AIStretch
    | AIBP BaselinePosition
    | AISP (Maybe OverflowPosition) SelfPosition


alignItems : Parser AlignItems
alignItems =
    oneOf
        [ keyword "normal" |> map (always AINormal)
        , keyword "stretch" |> map (always AIStretch)
        , baselinePosition |> map AIBP
        , maybeOverflowPositionHelp selfPosition AISP
        ]


placeItems : Parser Align
placeItems =
    succeed PlaceItems
        |= placeItemsValue
        |. whitespace
        |= oneOf
            [ placeItemsValue |> map Just
            , succeed Nothing
            ]


{-| The possible values for the [`place-items`](https://drafts.csswg.org/css-align-3/#propdef-place-items) property.

Value definition syntax:
`normal | stretch | <baseline-position> | <self-position>`

-}
type PlaceItemsValue
    = PINormal
    | PItretch
    | PIBP BaselinePosition
    | PISP SelfPosition


placeItemsValue : Parser PlaceItemsValue
placeItemsValue =
    oneOf
        [ keyword "normal" |> map (always PINormal)
        , keyword "stretch" |> map (always PItretch)
        , baselinePosition |> map PIBP
        , selfPosition |> map PISP
        ]


{-| The possible values for the [`row-gap`](https://drafts.csswg.org/css-align-3/#propdef-row-gap) and [`column-gap`](https://drafts.csswg.org/css-align-3/#propdef-column-gap) properties.

Value definition syntax:
`normal | <length-percentage>`

-}
type GapValue
    = GapNormal
    | GapLP LengthPercentage


gapValue : Parser GapValue
gapValue =
    oneOf
        [ keyword "normal" |> map (always GapNormal)
        , lengthPercentage |> map GapLP
        ]


gap : Parser Align
gap =
    succeed Gap
        |= gapValue
        |. whitespace
        |= oneOf
            [ gapValue |> map Just
            , succeed Nothing
            ]



-- HELPERS --


maybeOverflowPositionHelp : Parser b -> (Maybe OverflowPosition -> b -> a) -> Parser a
maybeOverflowPositionHelp parser type_ =
    oneOf
        [ succeed (\n x -> type_ (Just n) x)
            |= overflowPosition
            |. whitespace
            |= parser
        , parser |> map (type_ Nothing)
        ]
