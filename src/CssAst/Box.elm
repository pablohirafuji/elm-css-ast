module CssAst.Box
    exposing
        ( Box(..)
        , declarations
        , MarginValue(..)
        , Width(..)
        , width
        , Height(..)
        , MinWidthOrHeight(..)
        , MaxWidthOrHeight(..)
        , MinMaxWHBoxValue(..)
        , Float(..)
        , Clear(..)
        , ClearAfter(..)
        , OverflowXY(..)
        , Visibility(..)
        , FloatDisplace(..)
        , FloatDisplace3(..)
        , FloatDisplace3Block(..)
        , IndentEdgeReset(..)
        )

{-| [CSS basic box model](https://drafts.csswg.org/css-box-3/)

@docs Box,MarginValue,Width,Height,MinWidthOrHeight,MaxWidthOrHeight,MinMaxWHBoxValue,Float,Clear,ClearAfter,OverflowXY,Visibility,FloatDisplace,FloatDisplace3,FloatDisplace3Block, IndentEdgeReset, declarations, width

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail)
import CssAst.Helpers exposing (whitespace, toMaybe, anyOrder2)
import CssAst.Values exposing (Length, Percentage, LengthPercentage, length, percentage, lengthPercentage)


{-| The Box type.

[Source](https://drafts.csswg.org/css-box-3/#property-index).

-}
type Box
    = Padding Length (Maybe Length) (Maybe Length) (Maybe Length)
    | PaddingTop Length
    | PaddingRight Length
    | PaddingBottom Length
    | PaddingLeft Length
    | Margin MarginValue (Maybe MarginValue) (Maybe MarginValue) (Maybe MarginValue)
    | MarginTop MarginValue
    | MarginRight MarginValue
    | MarginBottom MarginValue
    | MarginLeft MarginValue
    | Width Width
    | Height Height
    | MinWidth MinWidthOrHeight
    | MinHeight MinWidthOrHeight
    | MaxWidth MaxWidthOrHeight
    | MaxHeight MaxWidthOrHeight
    | Float Float
    | Clear Clear
    | ClearAfter ClearAfter
    | OverflowX OverflowXY
    | OverflowY OverflowXY
    | Overflow OverflowXY (Maybe OverflowXY)
    | Visibility Visibility
    | FloatDisplace FloatDisplace
    | FloatDisplace2 FloatDisplace
    | FloatDisplace3 FloatDisplace3
    | IndentEdgeReset IndentEdgeReset


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Box )
declarations =
    [ ( "padding", padding )
    , ( "padding-top", map PaddingTop length )
    , ( "padding-right", map PaddingRight length )
    , ( "padding-bottom", map PaddingBottom length )
    , ( "padding-left", map PaddingLeft length )
    , ( "margin", margin )
    , ( "margin-top", map MarginTop marginValue )
    , ( "margin-right", map MarginRight marginValue )
    , ( "margin-bottom", map MarginBottom marginValue )
    , ( "margin-left", map MarginLeft marginValue )
    , ( "width", map Width width )
    , ( "height", map Height height )
    , ( "min-width", map MinWidth minWidthOrHeight )
    , ( "min-height", map MinHeight minWidthOrHeight )
    , ( "max-width", map MaxWidth maxWidthOrHeight )
    , ( "max-height", map MaxHeight maxWidthOrHeight )
    , ( "float", map Float float )
    , ( "clear", map Clear clear )
    , ( "clear-after", map ClearAfter clearAfter )
    , ( "overflow-x", map OverflowX overflowXY )
    , ( "overflow-y", map OverflowY overflowXY )
    , ( "overflow", overflow )
    , ( "visibility", map Visibility visibility )
    , ( "float-displace", map FloatDisplace floatDisplace )
    , ( "float-displace--alternative-2", map FloatDisplace2 floatDisplace )
    , ( "float-displace--alternative-3", map FloatDisplace3 floatDisplace3 )
    , ( "indent-edge-reset", map IndentEdgeReset indentEdgeReset )
    ]


padding : Parser Box
padding =
    succeed Padding
        |= length
        |. whitespace
        |= toMaybe length
        |. whitespace
        |= toMaybe length
        |. whitespace
        |= toMaybe length


{-| The [`margin value`](https://drafts.csswg.org/css-box-3/#propdef-margin) helper type.

Value definition syntax:
`<length> | <percentage> | auto`

-}
type MarginValue
    = MVL Length
    | MVP Percentage
    | MVAuto


margin : Parser Box
margin =
    succeed Margin
        |= marginValue
        |. whitespace
        |= toMaybe marginValue
        |. whitespace
        |= toMaybe marginValue
        |. whitespace
        |= toMaybe marginValue


marginValue : Parser MarginValue
marginValue =
    oneOf
        [ keyword "auto" |> map (always MVAuto)
        , percentage |> map MVP
        , length |> map MVL
        ]


{-| The possible values for the [`width`](https://drafts.csswg.org/css-box-3/#propdef-width) property.

Value definition syntax:
`<length> | <percentage> | available | min-content | max-content | fit-content | auto`

-}
type Width
    = WL Length
    | WP Percentage
    | WAvailable
    | WMinContent
    | WMaxContent
    | WFitContent
    | WAuto


{-| Exposing for internal use only.
-}
width : Parser Width
width =
    oneOf
        [ length |> map WL
        , percentage |> map WP
        , keyword "available" |> map (always WAvailable)
        , keyword "min-content" |> map (always WMinContent)
        , keyword "max-content" |> map (always WMaxContent)
        , keyword "fit-content" |> map (always WFitContent)
        , keyword "auto" |> map (always WAuto)
        ]


{-| The possible values for the [`height`](https://drafts.csswg.org/css-box-3/#propdef-height) property.

Value definition syntax:
`<length> | <percentage> | available | min-content | max-content | fit-content | complex | auto`

-}
type Height
    = HL Length
    | HP Percentage
    | HAvailable
    | HMinContent
    | HMaxContent
    | HFitContent
    | HComplex
    | HAuto


height : Parser Height
height =
    oneOf
        [ length |> map HL
        , percentage |> map HP
        , keyword "available" |> map (always HAvailable)
        , keyword "min-content" |> map (always HMinContent)
        , keyword "max-content" |> map (always HMaxContent)
        , keyword "fit-content" |> map (always HFitContent)
        , keyword "complex" |> map (always HComplex)
        , keyword "auto" |> map (always HAuto)
        ]


{-| The possible values for the [`min-width`](https://drafts.csswg.org/css-box-3/#propdef-min-width) and [`min-height`](https://drafts.csswg.org/css-box-3/#propdef-min-height) properties.

Value definition syntax:
`[ [<length> | <percentage>] && [border-box | content-box]? ] | available | min-content | max-content | fit-content`

-}
type MinWidthOrHeight
    = MinWBox LengthPercentage (Maybe MinMaxWHBoxValue)
    | MinWHAvaiable
    | MinWHMinContent
    | MinWHMaxContent
    | MinWHFitContent


minWidthOrHeight : Parser MinWidthOrHeight
minWidthOrHeight =
    oneOf
        [ keyword "available" |> map (always MinWHAvaiable)
        , keyword "min-content" |> map (always MinWHMinContent)
        , keyword "max-content" |> map (always MinWHMaxContent)
        , keyword "fit-content" |> map (always MinWHFitContent)
        , anyOrder2 MinWBox lengthPercentage (toMaybe minMaxWHBoxValue)
        ]


{-| The possible values for the [`max-width`](https://drafts.csswg.org/css-box-3/#propdef-max-width) and [`max-height`](https://drafts.csswg.org/css-box-3/#propdef-max-height) properties.

Value definition syntax:
`[ [<length> | <percentage>] && [border-box | content-box]? ] | available | min-content | max-content | fit-content | none`

-}
type MaxWidthOrHeight
    = MaxWBox LengthPercentage (Maybe MinMaxWHBoxValue)
    | MaxWHAvailable
    | MaxWHMinContent
    | MaxWHMaxContent
    | MaxWHFitContent
    | MaxWHNone


maxWidthOrHeight : Parser MaxWidthOrHeight
maxWidthOrHeight =
    oneOf
        [ keyword "available" |> map (always MaxWHAvailable)
        , keyword "min-content" |> map (always MaxWHMinContent)
        , keyword "max-content" |> map (always MaxWHMaxContent)
        , keyword "fit-content" |> map (always MaxWHFitContent)
        , keyword "none" |> map (always MaxWHNone)
        , anyOrder2 MaxWBox lengthPercentage (toMaybe minMaxWHBoxValue)
        ]


type MinMaxWHBoxValue
    = BorderBox
    | ContentBox


minMaxWHBoxValue : Parser MinMaxWHBoxValue
minMaxWHBoxValue =
    oneOf
        [ keyword "border-box" |> map (always BorderBox)
        , keyword "content-box" |> map (always ContentBox)
        ]


{-| The possible values for the [`float`](https://drafts.csswg.org/css-box-3/#propdef-float) property.

Value definition syntax:
`left | right | top | bottom | start | end | none | <page-floats>`

Didn't find `<page-floats>` definition.

-}
type Float
    = FLeft
    | FRight
    | FTop
    | FBottom
    | FStart
    | FEnd
    | FNone


float : Parser Float
float =
    oneOf
        [ keyword "left" |> map (always FLeft)
        , keyword "right" |> map (always FRight)
        , keyword "top" |> map (always FTop)
        , keyword "bottom" |> map (always FBottom)
        , keyword "start" |> map (always FStart)
        , keyword "end" |> map (always FEnd)
        , keyword "none" |> map (always FNone)
        ]


{-| The possible values for the [`clear`](https://drafts.csswg.org/css-box-3/#propdef-clear) property.

Value definition syntax:
`none | left | right | both`

-}
type Clear
    = CNone
    | CLeft
    | CRight
    | CBoth


clear : Parser Clear
clear =
    oneOf
        [ keyword "none" |> map (always CNone)
        , keyword "left" |> map (always CLeft)
        , keyword "right" |> map (always CRight)
        , keyword "both" |> map (always CBoth)
        ]


{-| The possible values for the [`clear-after`](https://drafts.csswg.org/css-box-3/#propdef-clear-after) property.

Value definition syntax:
`none | left | right | top | bottom | inside | outside | start | end | both | descendants`

-}
type ClearAfter
    = CANone
    | CALeft
    | CARight
    | CATop
    | CABottom
    | CAInside
    | CAOutside
    | CAStart
    | CAEnd
    | CABoth
    | CADescendants


clearAfter : Parser ClearAfter
clearAfter =
    oneOf
        [ keyword "none" |> map (always CANone)
        , keyword "left" |> map (always CALeft)
        , keyword "right" |> map (always CARight)
        , keyword "top" |> map (always CATop)
        , keyword "bottom" |> map (always CABottom)
        , keyword "inside" |> map (always CAInside)
        , keyword "outside" |> map (always CAOutside)
        , keyword "start" |> map (always CAStart)
        , keyword "end" |> map (always CAEnd)
        , keyword "both" |> map (always CABoth)
        , keyword "descendants" |> map (always CADescendants)
        ]


{-| The possible values for the [`overflow-x`](https://drafts.csswg.org/css-box-3/#propdef-overflow-x) and [`overflow-y`](https://drafts.csswg.org/css-box-3/#propdef-overflow-y) properties.

Value definition syntax:
`visible | hidden | scroll | auto | no-display | no-content`

-}
type OverflowXY
    = OXYVisible
    | OXYHidden
    | OXYScroll
    | OXYAuto
    | OXYNoDisplay
    | OXYNoContent


overflowXY : Parser OverflowXY
overflowXY =
    oneOf
        [ keyword "visible" |> map (always OXYVisible)
        , keyword "hidden" |> map (always OXYHidden)
        , keyword "scroll" |> map (always OXYScroll)
        , keyword "auto" |> map (always OXYAuto)
        , keyword "no-display" |> map (always OXYNoDisplay)
        , keyword "no-content" |> map (always OXYNoContent)
        ]


overflow : Parser Box
overflow =
    succeed Overflow
        |= overflowXY
        |. whitespace
        |= oneOf
            [ overflowXY |> map Just
            , succeed Nothing
            ]


{-| The possible values for the [`visibility`](https://drafts.csswg.org/css-box-3/#propdef-visibility) property.

Value definition syntax:
`visible | hidden | collapse`

-}
type Visibility
    = Visible
    | Hidden
    | Collapse


visibility : Parser Visibility
visibility =
    oneOf
        [ keyword "visible" |> map (always Visible)
        , keyword "hidden" |> map (always Hidden)
        , keyword "collapse" |> map (always Collapse)
        ]


{-| The possible values for the [`float-displace`](https://drafts.csswg.org/css-box-3/#propdef-float-displace) property.

Value definition syntax:
`line | indent | block | block-within-page`

-}
type FloatDisplace
    = Line
    | Indent
    | Block
    | BlockWithinPage


floatDisplace : Parser FloatDisplace
floatDisplace =
    oneOf
        [ keyword "line" |> map (always Line)
        , keyword "indent" |> map (always Indent)
        , keyword "block-within-page" |> map (always BlockWithinPage)
        , keyword "block" |> map (always Block)
        ]


{-| The possible values for the [`float-displace--alternative-3`](https://drafts.csswg.org/css-box-3/#propdef-float-displace--alternative-3) property.

Value definition syntax:
`auto | <length> && [ block | block-within-page ]?`

-}
type FloatDisplace3
    = FD3Auto
    | FDL Length (Maybe FloatDisplace3Block)


floatDisplace3 : Parser FloatDisplace3
floatDisplace3 =
    oneOf
        [ keyword "auto" |> map (always FD3Auto)
        , succeed FDL
            |= length
            |. whitespace
            |= toMaybe floatDisplace3Block
        , succeed (\a b -> FDL b a)
            |= toMaybe floatDisplace3Block
            |. whitespace
            |= length
        ]


type FloatDisplace3Block
    = FD3Block
    | FD3BlockWithinPage


floatDisplace3Block : Parser FloatDisplace3Block
floatDisplace3Block =
    oneOf
        [ keyword "block" |> map (always FD3Block)
        , keyword "block-within-page"
            |> map (always FD3BlockWithinPage)
        ]


{-| The possible values for the [`indent-edge-reset`](https://drafts.csswg.org/css-box-3/#propdef-indent-edge-reset) property.

Value definition syntax:
`none | margin-edge | border-edge | padding-edge | content-edge`

-}
type IndentEdgeReset
    = IERNone
    | MarginEdge
    | BorderEdge
    | PaddingEdge
    | ContentEdge


indentEdgeReset : Parser IndentEdgeReset
indentEdgeReset =
    oneOf
        [ keyword "none" |> map (always IERNone)
        , keyword "margin-edge" |> map (always MarginEdge)
        , keyword "border-edge" |> map (always BorderEdge)
        , keyword "padding-edge" |> map (always PaddingEdge)
        , keyword "content-edge" |> map (always ContentEdge)
        ]
