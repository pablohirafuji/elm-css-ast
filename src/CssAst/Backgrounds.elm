module CssAst.Backgrounds
    exposing
        ( Background(..)
        , declarations
        , FinalBgLayer
        , Attachment(..)
        , Box(..)
        , BgImage(..)
        , BgPosition
        , BgPositionValue(..)
        , RepeatStyle(..)
        , RepeatStyleValue(..)
        , BgSize(..)
        , CustomBgSizeValue(..)
        , BorderSH
        , LineWidth(..)
        , LineStyle(..)
        , BorderImageSH
        , BorderImageSlice
        , BorderImageWidth
        , BorderImageWidthValue(..)
        , BorderImageOutset
        , NumberLength(..)
        , BorderImageRepeat
        , BorderImageRepeatValue(..)
        )

{-| [CSS Backgrounds and Borders](https://drafts.csswg.org/css-backgrounds-3/).

@docs Background, FinalBgLayer, Attachment, Box, BgImage, BgPosition, BgPositionValue, RepeatStyle, RepeatStyleValue, BgSize, CustomBgSizeValue, BorderSH, LineWidth, LineStyle, BorderImageSH, BorderImageSlice, BorderImageWidth, BorderImageWidthValue, BorderImageOutset, BorderImageRepeat, BorderImageRepeatValue, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe, anyOrder2)
import CssAst.Color as Color
import CssAst.Images as Images
import CssAst.Values as V exposing (Side(..))


{-| The `Background` type.

[Source](https://drafts.csswg.org/css-backgrounds-3/#property-index).

-}
type Background
    = Background FinalBgLayer (List FinalBgLayer)
    | Attachment Attachment (List Attachment)
    | Clip Box (List Box)
    | Color Color.Value
    | Image BgImage (List BgImage)
    | Origin Box (List Box)
    | Position BgPosition (List BgPosition)
    | Repeat RepeatStyle (List RepeatStyle)
    | Size BgSize (List BgSize)
    | Border BorderSH
    | BorderDir Side BorderSH
    | BorderColor Color.Value (Maybe Color.Value) (Maybe Color.Value) (Maybe Color.Value)
    | BorderDirColor Side Color.Value
    | BorderStyle LineStyle (Maybe LineStyle) (Maybe LineStyle) (Maybe LineStyle)
    | BorderDirStyle Side LineStyle
    | BorderWidth LineWidth (Maybe LineWidth) (Maybe LineWidth) (Maybe LineWidth)
    | BorderDirWidth Side LineWidth
    | BorderRadius V.LengthPercentage (Maybe V.LengthPercentage) (Maybe V.LengthPercentage) (Maybe V.LengthPercentage) (Maybe ( V.LengthPercentage, Maybe V.LengthPercentage, Maybe V.LengthPercentage, Maybe V.LengthPercentage ))
    | BorderDirRadius Side V.LengthPercentage (Maybe V.LengthPercentage)
    | BorderImage BorderImageSH
    | BorderBgImage BgImage
    | BorderImageSlice BorderImageSlice
    | BorderImageWidth BorderImageWidth
    | BorderImageOutset BorderImageOutset
    | BorderImageRepeat BorderImageRepeat


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Background )
declarations =
    [ ( "background"
      , oneOrMoreCommaList Background finalBgLayer
      )
    , ( "background-attachment"
      , oneOrMoreCommaList Attachment attachment
      )
    , ( "background-clip", oneOrMoreCommaList Clip box )
    , ( "background-color", map Color Color.value )
    , ( "background-image", oneOrMoreCommaList Image bgImage )
    , ( "background-origin", oneOrMoreCommaList Origin box )
    , ( "background-position", oneOrMoreCommaList Position bgPosition )
    , ( "background-repeat", oneOrMoreCommaList Repeat repeatStyle )
    , ( "background-size", oneOrMoreCommaList Size bgSize )
    , ( "border", map Border (borderSH "border") )
    , ( "border-top", map (BorderDir Top) (borderSH "border-top") )
    , ( "border-right", map (BorderDir Right) (borderSH "border-right") )
    , ( "border-bottom", map (BorderDir Bottom) (borderSH "border-bottom") )
    , ( "border-left", map (BorderDir Left) (borderSH "border-left") )
    , ( "border-color", borderColor )
    , ( "border-top-color", map (BorderDirColor Top) Color.value )
    , ( "border-right-color", map (BorderDirColor Right) Color.value )
    , ( "border-bottom-color", map (BorderDirColor Bottom) Color.value )
    , ( "border-left-color", map (BorderDirColor Left) Color.value )
    , ( "border-style", borderStyle )
    , ( "border-top-style", map (BorderDirStyle Top) lineStyle )
    , ( "border-right-style", map (BorderDirStyle Right) lineStyle )
    , ( "border-bottom-style", map (BorderDirStyle Bottom) lineStyle )
    , ( "border-left-style", map (BorderDirStyle Left) lineStyle )
    , ( "border-width", borderWidth )
    , ( "border-top-width", map (BorderDirWidth Top) lineWidth )
    , ( "border-right-width", map (BorderDirWidth Right) lineWidth )
    , ( "border-bottom-width", map (BorderDirWidth Bottom) lineWidth )
    , ( "border-left-width", map (BorderDirWidth Left) lineWidth )
    , ( "border-radius", borderRadius )
    , ( "border-top-left-radius", cornerRadius (BorderDirRadius Top) )
    , ( "border-top-right-radius", cornerRadius (BorderDirRadius Right) )
    , ( "border-bottom-right-radius", cornerRadius (BorderDirRadius Bottom) )
    , ( "border-bottom-left-radius", cornerRadius (BorderDirRadius Left) )
    , ( "border-image", map BorderImage borderImageSH )
    , ( "border-image-source", map BorderBgImage bgImage )
    , ( "border-image-slice", map BorderImageSlice borderImageSlice )
    , ( "border-image-width", map BorderImageWidth borderImageWidth )
    , ( "border-image-outset", map BorderImageOutset borderImageOutset )
    , ( "border-image-repeat", map BorderImageRepeat borderImageRepeat )
    ]


{-| The [`<final-bg-layer>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-final-bg-layer) data type alias.

Value definition syntax:
`<color> || <bg-image> || <bg-position> [ / <bg-size> ]? || <repeat-style> || <attachment> || <box> || <box>`

-}
type alias FinalBgLayer =
    { color : Maybe Color.Value
    , image : Maybe BgImage
    , position : Maybe BgPosition
    , size : Maybe BgSize
    , repeat : Maybe RepeatStyle
    , attachment : Maybe Attachment
    , origin : Maybe Box
    , clip : Maybe Box
    }


finalBgLayer : Parser FinalBgLayer
finalBgLayer =
    finalBgLayerHelp [] finalBgLayerParsers emptyFinalBgLayer


finalBgLayerHelp : List (FinalBgLayer -> Parser FinalBgLayer) -> List (FinalBgLayer -> Parser FinalBgLayer) -> FinalBgLayer -> Parser FinalBgLayer
finalBgLayerHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (finalBgLayerHelp [] (nsHead ++ tail))
                , finalBgLayerHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n.color == Nothing && n.image == Nothing && n.position == Nothing && n.repeat == Nothing && n.attachment == Nothing && n.origin == Nothing && n.clip == Nothing then
                fail "Empty `animation` property value."
            else
                succeed n


finalBgLayerParsers : List (FinalBgLayer -> Parser FinalBgLayer)
finalBgLayerParsers =
    [ \r -> map (\n -> { r | color = Just n }) Color.value
    , \r -> map (\n -> { r | image = Just n }) bgImage
    , \r ->
        map (\( n, m ) -> { r | position = Just n, size = m })
            (succeed (,)
                |= bgPosition
                |. whitespace
                |= toMaybe
                    (succeed identity
                        |. symbol "/"
                        |. whitespace
                        |= bgSize
                    )
            )
    , \r -> map (\n -> { r | repeat = Just n }) repeatStyle
    , \r -> map (\n -> { r | attachment = Just n }) attachment
    , \r -> map (\n -> { r | origin = Just n }) box
    , \r -> map (\n -> { r | clip = Just n }) box
    ]


emptyFinalBgLayer : FinalBgLayer
emptyFinalBgLayer =
    { color = Nothing
    , image = Nothing
    , position = Nothing
    , size = Nothing
    , repeat = Nothing
    , attachment = Nothing
    , origin = Nothing
    , clip = Nothing
    }


{-| The [`<attachment>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-attachment) data type.

Value definition syntax:
`scroll | fixed | local`

-}
type Attachment
    = Scroll
    | Fixed
    | Local


attachment : Parser Attachment
attachment =
    keywordsToType
        [ ( "scroll", Scroll )
        , ( "fixed", Fixed )
        , ( "local", Local )
        ]


{-| The [`<box>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-box) data type.

Value definition syntax:
`border-box | padding-box | content-box`

-}
type Box
    = BorderBox
    | PaddingBox
    | ContentBox


box : Parser Box
box =
    keywordsToType
        [ ( "border-box", BorderBox )
        , ( "padding-box", PaddingBox )
        , ( "content-box", ContentBox )
        ]


{-| The [`<bg-image>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-bg-image) data and [`border-image-source`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border-image-source) property type.

Value definition syntax:
`none | <image>`

-}
type BgImage
    = NoImage
    | BgImage Images.Value


bgImage : Parser BgImage
bgImage =
    oneOf
        [ keyword "none" |> map (always NoImage)
        , Images.value |> map BgImage
        ]


{-| The [`<bg-position>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-bg-position) data type.

Value definition syntax:

    [
      [ left | center | right | top | bottom | <length-percentage> ]
    |
      [ left | center | right | <length-percentage> ]
      [ top | center | bottom | <length-percentage> ]
    |
      [ center | [ left | right ] <length-percentage>? ] &&
      [ center | [ top | bottom ] <length-percentage>? ]
    ]

-}
type alias BgPosition =
    ( BgPositionValue, Maybe BgPositionValue )


bgPosition : Parser BgPosition
bgPosition =
    succeed (,)
        |= bgPositionValue
        |. whitespace
        |= toMaybe bgPositionValue
        |> andThen
            (\( p, m ) ->
                case ( p, m ) of
                    ( Direction dir (Just lp), Nothing ) ->
                        succeed
                            ( Direction dir Nothing
                            , Just (LengthPercentage lp)
                            )

                    ( Direction dir1 _, Just (Direction dir2 _) ) ->
                        if (dir1 == Left || dir1 == Right) && (dir2 == Left || dir2 == Right) then
                            fail "Invalid `background-position` property value."
                        else if (dir1 == Top || dir1 == Bottom) && (dir2 == Top || dir2 == Bottom) then
                            fail "Invalid `background-position` property value."
                        else
                            succeed ( p, m )

                    _ ->
                        succeed ( p, m )
            )


type BgPositionValue
    = Center
    | Direction Side (Maybe V.LengthPercentage)
    | LengthPercentage V.LengthPercentage


bgPositionValue : Parser BgPositionValue
bgPositionValue =
    oneOf
        [ keyword "center" |> map (always Center)
        , V.lengthPercentage |> map LengthPercentage
        , succeed Direction
            |= V.side
            |. whitespace
            |= toMaybe V.lengthPercentage
        ]


{-| The [`<repeat-style>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-repeat-style) data type.

Value definition syntax:
`repeat-x | repeat-y | [repeat | space | round | no-repeat]{1,2}`

-}
type RepeatStyle
    = RepeatX
    | RepeatY
    | CustomRepeat RepeatStyleValue (Maybe RepeatStyleValue)


repeatStyle : Parser RepeatStyle
repeatStyle =
    oneOf
        [ keyword "repeat-x" |> map (always RepeatX)
        , keyword "repeat-y" |> map (always RepeatY)
        , succeed CustomRepeat
            |= repeatStyleValue
            |. whitespace
            |= toMaybe repeatStyleValue
        ]


type RepeatStyleValue
    = RepeatValue
    | Space
    | Round
    | NoRepeat


repeatStyleValue : Parser RepeatStyleValue
repeatStyleValue =
    keywordsToType
        [ ( "repeat", RepeatValue )
        , ( "space", Space )
        , ( "round", Round )
        , ( "no-repeat", NoRepeat )
        ]


{-| The [`<bg-size>`](https://drafts.csswg.org/css-backgrounds-3/#propdef-background-size) data type.

Value definition syntax:
`[ <length-percentage> | auto ]{1,2} | cover | contain`

-}
type BgSize
    = Cover
    | Contain
    | CustomBgSize CustomBgSizeValue (Maybe CustomBgSizeValue)


bgSize : Parser BgSize
bgSize =
    oneOf
        [ keyword "contain" |> map (always Contain)
        , keyword "cover" |> map (always Cover)
        , succeed CustomBgSize
            |= customBgSizeValue
            |. whitespace
            |= toMaybe customBgSizeValue
        ]


{-| `BgSize` custom value.

Value definition syntax:
`<length-percentage> | auto`

-}
type CustomBgSizeValue
    = Auto
    | CBSVLP V.LengthPercentage


customBgSizeValue : Parser CustomBgSizeValue
customBgSizeValue =
    oneOf
        [ keyword "auto" |> map (always Auto)
        , V.lengthPercentage |> map CBSVLP
        ]


{-| The [`border`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border), [`border-top`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border-top), [`border-right`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border-right), [`border-bottom`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border-bottom) and [`border-left`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border-left) shorthand properties type alias.

Value definition syntax:
`<line-width> || <line-style> || <color>`

-}
type alias BorderSH =
    { lineWidth : Maybe LineWidth
    , lineStyle : Maybe LineStyle
    , color : Maybe Color.Value
    }


borderSH : String -> Parser BorderSH
borderSH propertyName =
    borderSHHelp propertyName [] borderSHParsers emptyBorderSH


borderSHHelp : String -> List (BorderSH -> Parser BorderSH) -> List (BorderSH -> Parser BorderSH) -> BorderSH -> Parser BorderSH
borderSHHelp pN nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (borderSHHelp pN [] (nsHead ++ tail))
                , borderSHHelp pN (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n.lineWidth == Nothing && n.lineStyle == Nothing && n.color == Nothing then
                fail ("Empty " ++ pN ++ " property value.")
            else
                succeed n


borderSHParsers : List (BorderSH -> Parser BorderSH)
borderSHParsers =
    [ \r -> map (\n -> { r | lineWidth = Just n }) lineWidth
    , \r -> map (\n -> { r | lineStyle = Just n }) lineStyle
    , \r -> map (\n -> { r | color = Just n }) Color.value
    ]


emptyBorderSH : BorderSH
emptyBorderSH =
    { lineWidth = Nothing
    , lineStyle = Nothing
    , color = Nothing
    }


{-| The [`<line-width>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-line-width) data type.

Value definition syntax:
`<length> | thin | medium | thick`

-}
type LineWidth
    = LineWidthLength V.Length
    | Thin
    | Medium
    | Thick


lineWidth : Parser LineWidth
lineWidth =
    oneOf
        [ V.length |> map LineWidthLength
        , keyword "thin" |> map (always Thin)
        , keyword "medium" |> map (always Medium)
        , keyword "thick" |> map (always Thick)
        ]


{-| The [`<line-style>`](https://drafts.csswg.org/css-backgrounds-3/#typedef-line-style) data type.

Value definition syntax:
`none | hidden | dotted | dashed | solid | double | groove | ridge | inset | outset`

-}
type LineStyle
    = None
    | Hidden
    | Dotted
    | Dashed
    | Solid
    | Double
    | Groove
    | Ridge
    | Inset
    | Outset


lineStyle : Parser LineStyle
lineStyle =
    keywordsToType
        [ ( "none ", None )
        , ( "hidden", Hidden )
        , ( "dotted", Dotted )
        , ( "dashed", Dashed )
        , ( "solid", Solid )
        , ( "double", Double )
        , ( "groove", Groove )
        , ( "ridge", Ridge )
        , ( "inset", Inset )
        , ( "outset", Outset )
        ]


borderColor : Parser Background
borderColor =
    succeed BorderColor
        |= Color.value
        |. whitespace
        |= toMaybe Color.value
        |. whitespace
        |= toMaybe Color.value
        |. whitespace
        |= toMaybe Color.value


borderStyle : Parser Background
borderStyle =
    succeed BorderStyle
        |= lineStyle
        |. whitespace
        |= toMaybe lineStyle
        |. whitespace
        |= toMaybe lineStyle
        |. whitespace
        |= toMaybe lineStyle


borderWidth : Parser Background
borderWidth =
    succeed BorderWidth
        |= lineWidth
        |. whitespace
        |= toMaybe lineWidth
        |. whitespace
        |= toMaybe lineWidth
        |. whitespace
        |= toMaybe lineWidth


borderRadius : Parser Background
borderRadius =
    succeed BorderRadius
        |= V.lengthPercentage
        |. whitespace
        |= toMaybe V.lengthPercentage
        |. whitespace
        |= toMaybe V.lengthPercentage
        |. whitespace
        |= toMaybe V.lengthPercentage
        |. whitespace
        |= oneOf
            [ succeed (,,,)
                |. symbol "/"
                |. whitespace
                |= V.lengthPercentage
                |. whitespace
                |= toMaybe V.lengthPercentage
                |. whitespace
                |= toMaybe V.lengthPercentage
                |. whitespace
                |= toMaybe V.lengthPercentage
                |> map Just
            , succeed Nothing
            ]


cornerRadius : (V.LengthPercentage -> Maybe V.LengthPercentage -> Background) -> Parser Background
cornerRadius type_ =
    succeed type_
        |= V.lengthPercentage
        |. whitespace
        |= toMaybe V.lengthPercentage


{-| The [`border-image`](https://drafts.csswg.org/css-backgrounds-3/#propdef-border-image) shorthand property type alias.

Value definition syntax:
`<‘border-image-source’> || <‘border-image-slice’> [ / <‘border-image-width’> | / <‘border-image-width’>? / <‘border-image-outset’> ]? || <‘border-image-repeat’>`

-}
type alias BorderImageSH =
    { source : Maybe BgImage
    , slice : Maybe BorderImageSlice
    , width : Maybe BorderImageWidth
    , outset : Maybe BorderImageOutset
    , repeat : Maybe BorderImageRepeat
    }


borderImageSH : Parser BorderImageSH
borderImageSH =
    borderImageSHHelp [] borderImageSHParsers emptyBorderImageSH


borderImageSHHelp : List (BorderImageSH -> Parser BorderImageSH) -> List (BorderImageSH -> Parser BorderImageSH) -> BorderImageSH -> Parser BorderImageSH
borderImageSHHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (borderImageSHHelp [] (nsHead ++ tail))
                , borderImageSHHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n.source == Nothing && n.slice == Nothing && n.width == Nothing && n.outset == Nothing && n.repeat == Nothing then
                fail "Empty `animation` property value."
            else
                succeed n


borderImageSHParsers : List (BorderImageSH -> Parser BorderImageSH)
borderImageSHParsers =
    [ \r -> map (\n -> { r | source = Just n }) bgImage
    , \r ->
        map (\( n, m, o ) -> { r | slice = Just n, width = m, outset = o })
            (succeed (,,)
                |= borderImageSlice
                |. whitespace
                |= toMaybe
                    (succeed identity
                        |. symbol "/"
                        |. whitespace
                        |= borderImageWidth
                    )
                |. whitespace
                |= toMaybe
                    (succeed identity
                        |. symbol "/"
                        |. whitespace
                        |= borderImageOutset
                    )
            )
    , \r -> map (\n -> { r | repeat = Just n }) borderImageRepeat
    ]


emptyBorderImageSH : BorderImageSH
emptyBorderImageSH =
    { source = Nothing
    , slice = Nothing
    , width = Nothing
    , outset = Nothing
    , repeat = Nothing
    }


type alias BorderImageSlice =
    ( ( V.NumberPercentage, Maybe V.NumberPercentage, Maybe V.NumberPercentage, Maybe V.NumberPercentage ), Bool )


borderImageSlice : Parser BorderImageSlice
borderImageSlice =
    let
        parsr =
            succeed (,,,)
                |= V.numberPercentage
                |. whitespace
                |= toMaybe V.numberPercentage
                |. whitespace
                |= toMaybe V.numberPercentage
                |. whitespace
                |= toMaybe V.numberPercentage

        fill =
            oneOf
                [ keyword "fill" |> map (always True)
                , succeed False
                ]
    in
        anyOrder2 (,) parsr fill


type alias BorderImageWidth =
    ( BorderImageWidthValue, Maybe BorderImageWidthValue, Maybe BorderImageWidthValue, Maybe BorderImageWidthValue )


borderImageWidth : Parser BorderImageWidth
borderImageWidth =
    succeed (,,,)
        |= borderImageWidthValue
        |. whitespace
        |= toMaybe borderImageWidthValue
        |. whitespace
        |= toMaybe borderImageWidthValue
        |. whitespace
        |= toMaybe borderImageWidthValue


type BorderImageWidthValue
    = BIWLength
    | BIWPercentage
    | BIWNumber
    | BIWAuto


borderImageWidthValue : Parser BorderImageWidthValue
borderImageWidthValue =
    oneOf
        [ keyword "auto" |> map (always BIWAuto)
        , V.percentage |> map (always BIWPercentage)
        , V.length |> map (always BIWLength)
        , V.number |> map (always BIWNumber)
        ]


type alias BorderImageOutset =
    ( NumberLength, Maybe NumberLength, Maybe NumberLength, Maybe NumberLength )


borderImageOutset : Parser BorderImageOutset
borderImageOutset =
    succeed (,,,)
        |= numberLength
        |. whitespace
        |= toMaybe numberLength
        |. whitespace
        |= toMaybe numberLength
        |. whitespace
        |= toMaybe numberLength


type NumberLength
    = Number Float
    | Length V.Length


numberLength : Parser NumberLength
numberLength =
    oneOf
        [ V.length |> map Length
        , V.number |> map Number
        ]


type alias BorderImageRepeat =
    ( BorderImageRepeatValue, Maybe BorderImageRepeatValue )


borderImageRepeat : Parser BorderImageRepeat
borderImageRepeat =
    succeed (,)
        |= borderImageRepeatValue
        |. whitespace
        |= toMaybe borderImageRepeatValue


type BorderImageRepeatValue
    = BIRStretch
    | BIRRepeat
    | BIRRound
    | BIRSpace


borderImageRepeatValue : Parser BorderImageRepeatValue
borderImageRepeatValue =
    keywordsToType
        [ ( "stretch", BIRStretch )
        , ( "repeat", BIRRepeat )
        , ( "round", BIRRound )
        , ( "space", BIRSpace )
        ]
