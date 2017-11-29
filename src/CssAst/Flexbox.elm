module CssAst.Flexbox
    exposing
        ( Declaration(..)
        , declarations
        , AlignContent(..)
        , AlignItems(..)
        , AlignSelf(..)
        , Flex(..)
        , FactorsBasis
        , FlexBasis(..)
        , FlexDirection(..)
        , FlexFlow(..)
        , FlexWrap(..)
        , JustifyContent(..)
        )

{-| [CSS Flexible Box Layout](https://drafts.csswg.org/css-flexbox-1/)

@docs Declaration, AlignContent, AlignItems, AlignSelf, Flex, FactorsBasis, FlexBasis, FlexDirection, FlexFlow, FlexWrap, JustifyContent, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Values as V
import CssAst.Box as Box
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


{-| The Flexbox [declarations](https://drafts.csswg.org/css-flexbox-1/#property-index) type.
-}
type Declaration
    = AlignContent AlignContent
    | AlignItems AlignItems
    | AlignSelf AlignSelf
    | Flex Flex
    | FlexBasis FlexBasis
    | FlexDirection FlexDirection
    | FlexFlow FlexFlow
    | FlexGrow Float
    | FlexShrink Float
    | FlexWrap FlexWrap
    | JustifyContent JustifyContent
    | Order Int


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Declaration )
declarations =
    [ ( "align-content", alignContent |> map AlignContent )
    , ( "align-items", alignItems |> map AlignItems )
    , ( "align-self", alignSelf |> map AlignSelf )
    , ( "flex", flex |> map Flex )
    , ( "flex-basis", flexBasis |> map FlexBasis )
    , ( "flex-direction", flexDirection |> map FlexDirection )
    , ( "flex-flow", flexFlow |> map FlexFlow )
    , ( "flex-grow", V.number |> map FlexGrow )
    , ( "flex-shrink", V.number |> map FlexShrink )
    , ( "flex-wrap", flexWrap |> map FlexWrap )
    , ( "justify-content", justifyContent |> map JustifyContent )
    , ( "order", V.integer |> map Order )
    ]


{-| The [`align-content`](https://drafts.csswg.org/css-flexbox-1/#propdef-align-content) property type.

Value definition syntax:

    flex-start | flex-end | center | space-between | space-around | stretch

-}
type AlignContent
    = ACFlexStart
    | ACFlexEnd
    | ACCenter
    | ACSpaceBetween
    | ACSpaceAround
    | ACStretch


alignContent : Parser AlignContent
alignContent =
    keywordsToType
        [ ( "flex-start", ACFlexStart )
        , ( "flex-end", ACFlexEnd )
        , ( "center", ACCenter )
        , ( "space-between", ACSpaceBetween )
        , ( "space-around", ACSpaceAround )
        , ( "stretch", ACStretch )
        ]


{-| The [`align-items`](https://drafts.csswg.org/css-flexbox-1/#propdef-align-items) property type.

Value definition syntax:

    flex-start | flex-end | center | baseline | stretch

-}
type AlignItems
    = AIFlexStart
    | AIFlexEnd
    | AICenter
    | AIBaseline
    | AIStretch


alignItems : Parser AlignItems
alignItems =
    keywordsToType
        [ ( "flex-start", AIFlexStart )
        , ( "flex-end", AIFlexEnd )
        , ( "center", AICenter )
        , ( "baseline", AIBaseline )
        , ( "stretch", AIStretch )
        ]


{-| The [`align-self`](https://drafts.csswg.org/css-flexbox-1/#propdef-align-self) property type.

Value definition syntax:

    auto | flex-start | flex-end | center | baseline | stretch

-}
type AlignSelf
    = ASAuto
    | ASFlexStart
    | ASFlexEnd
    | ASCenter
    | ASBaseline
    | ASStretch


alignSelf : Parser AlignSelf
alignSelf =
    keywordsToType
        [ ( "auto", ASAuto )
        , ( "flex-start", ASFlexStart )
        , ( "flex-end", ASFlexEnd )
        , ( "center", ASCenter )
        , ( "baseline", ASBaseline )
        , ( "stretch", ASStretch )
        ]


{-| The [`flex`](https://drafts.csswg.org/css-flexbox-1/#propdef-flex) property type.

Value definition syntax:

    none | [ <‘flex-grow’> <‘flex-shrink’>? || <‘flex-basis’> ]

-}
type Flex
    = None
    | FlexFactorsBasis FactorsBasis


flex : Parser Flex
flex =
    oneOf
        [ keyword "none" |> map (always None)
        , factorBasis |> map FlexFactorsBasis
        ]


{-| The `FactorsBasis` helper type alias.

Value definition syntax:

    <‘flex-grow’> <‘flex-shrink’>? || <‘flex-basis’>

-}
type alias FactorsBasis =
    { grow : Maybe Float
    , shrink : Maybe Float
    , basis : Maybe FlexBasis
    }


factorBasis : Parser FactorsBasis
factorBasis =
    oneOf
        [ succeed FactorsBasis
            |= (V.number |> map Just)
            |. whitespace
            |= toMaybe V.number
            |. whitespace
            |= toMaybe flexBasis
        , succeed (\a b c -> FactorsBasis b c a)
            |= (flexBasis |> map Just)
            |. whitespace
            |= toMaybe V.number
            |. whitespace
            |= toMaybe V.number
        ]


{-| The [`flex-basis`](https://drafts.csswg.org/css-flexbox-1/#propdef-flex-basis) property type.

Value definition syntax:

    content | <‘width’>

-}
type FlexBasis
    = Content
    | Width Box.Width


flexBasis : Parser FlexBasis
flexBasis =
    oneOf
        [ keyword "content" |> map (always Content)
        , Box.width |> map Width
        ]


{-| The [`flex-direction`](https://drafts.csswg.org/css-flexbox-1/#propdef-flex-direction) property type.

Value definition syntax:

    row | row-reverse | column | column-reverse

-}
type FlexDirection
    = Row
    | RowReverse
    | Column
    | ColumnReverse


flexDirection : Parser FlexDirection
flexDirection =
    keywordsToType
        [ ( "row", Row )
        , ( "row-reverse", RowReverse )
        , ( "column", Column )
        , ( "column-reverse", ColumnReverse )
        ]


{-| The [`flex-flow`](https://drafts.csswg.org/css-flexbox-1/#propdef-flex-flow) property type.

Value definition syntax:

    <flex-direction> || <flex-wrap>

-}
type FlexFlow
    = OnlyDirection FlexDirection
    | OnlyWrap FlexWrap
    | Both FlexDirection FlexWrap


flexFlow : Parser FlexFlow
flexFlow =
    oneOf
        [ succeed
            (\a b ->
                Maybe.map (Both a) b
                    |> Maybe.withDefault (OnlyDirection a)
            )
            |= flexDirection
            |. whitespace
            |= toMaybe flexWrap
        , succeed
            (\a b ->
                Maybe.map (\c -> Both c a) b
                    |> Maybe.withDefault (OnlyWrap a)
            )
            |= flexWrap
            |. whitespace
            |= toMaybe flexDirection
        ]


{-| The [`flex-wrap`](https://drafts.csswg.org/css-flexbox-1/#propdef-flex-wrap) property type.

Value definition syntax:

    nowrap | wrap | wrap-reverse

-}
type FlexWrap
    = Nowrap
    | Wrap
    | WrapReverse


flexWrap : Parser FlexWrap
flexWrap =
    keywordsToType
        [ ( "nowrap", Nowrap )
        , ( "wrap", Wrap )
        , ( "wrap-reverse", WrapReverse )
        ]


{-| The [`justify-content`](https://drafts.csswg.org/css-flexbox-1/#propdef-justify-content) property type.

Value definition syntax:

    flex-start | flex-end | center | space-between | space-around

-}
type JustifyContent
    = JCFlexStart
    | JCFlexEnd
    | JCCenter
    | JCSpaceBetween
    | JCSpaceAround


justifyContent : Parser JustifyContent
justifyContent =
    keywordsToType
        [ ( "flex-start", JCFlexStart )
        , ( "flex-end", JCFlexEnd )
        , ( "center", JCCenter )
        , ( "space-between", JCSpaceBetween )
        , ( "space-around", JCSpaceAround )
        ]
