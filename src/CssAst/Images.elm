module CssAst.Images
    exposing
        ( Image(..)
        , declarations
        , Value(..)
        , value
        , ImageOrientation(..)
        , ImageRendering(..)
        , ObjectFit(..)
        )

{-| [CSS Images](https://drafts.csswg.org/css-images-3/).

@docs Image, Value, ImageOrientation, ImageRendering, ObjectFit, declarations, value

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, delayedCommitMap)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe, anyOrder2)
import CssAst.Color as Color
import CssAst.Values as V


{-| The `Image` type with all properties.

[Source](https://drafts.csswg.org/css-images-3/#property-index).

-}
type Image
    = ImageOrientation ImageOrientation
    | ImageRendering ImageRendering
    | ObjectFit ObjectFit
    | ObjectPosition V.Position


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Image )
declarations =
    [ ( "image-orientation", imageOrientation |> map ImageOrientation )
    , ( "image-rendering", imageRendering |> map ImageRendering )
    , ( "object-fit", objectFit |> map ObjectFit )
    , ( "object-position", V.position |> map ObjectPosition )
    ]


{-| The [`<image>`](https://drafts.csswg.org/css-images-3/#typedef-image) data type.

Value definition syntax:
`<url> | <cross-fade()> | <gradient>`

-}
type Value
    = Url String
    | CrossFade CrossFadeValue
    | Gradient Gradient


value : Parser Value
value =
    oneOf
        [ V.url |> map Url
        , gradient |> map Gradient
        , Parser.lazy (\_ -> crossFadeValue |> map CrossFade)
        ]


{-| The [`cross-fade()`](https://drafts.csswg.org/css-images-3/#funcdef-cross-fade) function type.

Value definition syntax:
`cross-fade( <cf-mixing-image> , <cf-final-image>? )`

-}
type alias CrossFadeValue =
    ( CfMixingImage, Maybe CfFinalImage )


crossFadeValue : Parser CrossFadeValue
crossFadeValue =
    succeed (,)
        |. keyword "cross-fade("
        |. whitespace
        |= cfMixingImage
        |. whitespace
        |= oneOf
            [ succeed Just
                |. symbol ","
                |. whitespace
                |= cfFinalImage
            , succeed Nothing
            ]


{-| The [`<cf-mixing-image>`](https://drafts.csswg.org/css-images-3/#typedef-cf-mixing-image) data type.

Value definition syntax:
`<percentage>? && <image>`

-}
type alias CfMixingImage =
    ( Value, Maybe V.Percentage )


cfMixingImage : Parser CfMixingImage
cfMixingImage =
    anyOrder2 (,)
        value
        (toMaybe V.percentage)


{-| The [`<cf-final-image>`](https://drafts.csswg.org/css-images-3/#typedef-cf-final-image) data type.

Value definition syntax:
`<image> | <color>`

-}
type CfFinalImage
    = CFIImage Value
    | CFIColor Color.Value


cfFinalImage : Parser CfFinalImage
cfFinalImage =
    oneOf
        [ Color.value |> map CFIColor
        , value |> map CFIImage
        ]


{-| The [`<gradient>`](https://drafts.csswg.org/css-images-3/#funcdef-linear-gradient) data type.

Value definition syntax:
`<linear-gradient()> | <repeating-linear-gradient()> | <radial-gradient()> | <repeating-radial-gradient()>`

-}
type Gradient
    = Linear LinearGradient
    | RepeatingLinear LinearGradient
    | Radial RadialGradient
    | RepeatingRadial RadialGradient


gradient : Parser Gradient
gradient =
    oneOf
        [ succeed identity
            |. keyword "linear-gradient("
            |. whitespace
            |= linearGradient
            |. whitespace
            |. symbol ")"
            |> map Linear
        , succeed identity
            |. keyword "radial-gradient("
            |. whitespace
            |= radialGradient
            |. whitespace
            |. symbol ")"
            |> map Radial
        , succeed identity
            |. keyword "repeating-linear-gradient("
            |. whitespace
            |= linearGradient
            |. whitespace
            |. symbol ")"
            |> map RepeatingLinear
        , succeed identity
            |. keyword "repeating-radial-gradient("
            |. whitespace
            |= radialGradient
            |. whitespace
            |. symbol ")"
            |> map RepeatingRadial
        ]


{-| The [`linear-gradient()`](https://drafts.csswg.org/css-images-3/#funcdef-linear-gradient) function type alias.

Value definition syntax:
`linear-gradient( [ <angle> | to <side-or-corner> ]? ,  <color-stop-list> )`

-}
type alias LinearGradient =
    ( Maybe GradientLine, List ColorStop )


linearGradient : Parser LinearGradient
linearGradient =
    oneOf
        [ succeed (,)
            |= (gradientLine |> map Just)
            |. whitespace
            |. symbol ","
            |. whitespace
            |= colorStopList
        , colorStopList |> map ((,) Nothing)
        ]


colorStopList : Parser (List ColorStop)
colorStopList =
    succeed (::)
        |= colorStop
        |. whitespace
        |= repeat oneOrMore
            (succeed identity
                |. symbol ","
                |. whitespace
                |= colorStop
                |. whitespace
            )


{-| The [`gradient line`](https://drafts.csswg.org/css-images-3/#gradient-line) helper type.

Value definition syntax:
`<angle> | to <side-or-corner>`

-}
type GradientLine
    = Angle V.Angle
    | SideOrCorner V.Side (Maybe V.Side)


gradientLine : Parser GradientLine
gradientLine =
    oneOf
        [ V.angle |> map Angle
        , succeed SideOrCorner
            |. keyword "to"
            |. whitespace
            |= V.side
            |. whitespace
            |= toMaybe V.side
        ]


{-| The [`<color-stop>`](https://drafts.csswg.org/css-images-3/#typedef-color-stop) data type.

Value definition syntax:
`<color> <length-percentage>?`

-}
type alias ColorStop =
    ( Color.Value, Maybe V.LengthPercentage )


colorStop : Parser ColorStop
colorStop =
    succeed (,)
        |= Color.value
        |. whitespace
        |= toMaybe V.lengthPercentage


{-| The [`radial-gradient()`](https://drafts.csswg.org/css-images-3/#funcdef-radial-gradient) function type alias.

Value definition syntax:

    radial-gradient(
      [ <ending-shape> || <size> ]? [ at <position> ]? , <color-stop-list>
    )

-}
type alias RadialGradient =
    ( Maybe EndingShape, Maybe RadialGradientSize, Maybe V.Position, List ColorStop )


radialGradient : Parser RadialGradient
radialGradient =
    succeed (\( a, b ) c d -> ( a, b, c, d ))
        |= oneOf
            [ succeed (,)
                |= (endingShape |> map Just)
                |. whitespace
                |= oneOf
                    [ radialGradientSize |> map Just
                    , succeed Nothing
                    ]
            , succeed (flip (,))
                |= (radialGradientSize |> map Just)
                |. whitespace
                |= oneOf
                    [ endingShape |> map Just
                    , succeed Nothing
                    ]
            , succeed ( Nothing, Nothing )
            ]
        |. whitespace
        |= oneOf
            [ succeed Just
                |. keyword "at"
                |. whitespace
                |= V.position
            , succeed Nothing
            ]
        |. whitespace
        |. oneOf
            [ symbol ","
            , whitespace
            ]
        |. whitespace
        |= colorStopList


{-| The radial gradient [`<ending-shape>`](https://drafts.csswg.org/css-images-3/#valdef-radial-gradient-ending-shape) data type.

Value definition syntax:
`circle | ellipse`

-}
type EndingShape
    = Circle
    | Ellipse


endingShape : Parser EndingShape
endingShape =
    keywordsToType
        [ ( "circle", Circle )
        , ( "ellipse", Ellipse )
        ]


{-| The radial gradient [`<size>`](https://drafts.csswg.org/css-images-3/#valdef-radial-gradient-size) data type.

Value definition syntax:
`<extent-keyword> | <length> | <length-percentage>{2}`

-}
type RadialGradientSize
    = RGSExtentKeyword ExtentKeyword
    | RGSLength V.Length
    | RGSLengthPercentage V.LengthPercentage V.LengthPercentage


radialGradientSize : Parser RadialGradientSize
radialGradientSize =
    oneOf
        [ extentKeyword |> map RGSExtentKeyword
        , delayedCommitMap RGSLengthPercentage
            (V.lengthPercentage |. whitespace)
            (V.lengthPercentage)
        , V.length |> map RGSLength
        ]


{-| The radial gradient [`<extent-keyword>`](https://drafts.csswg.org/css-images-3/#typedef-extent-keyword) data type.

Value definition syntax:
`closest-corner | closest-side | farthest-corner | farthest-side`

-}
type ExtentKeyword
    = ClosestCorner
    | ClosestSide
    | FarthestCorner
    | FarthestSide


extentKeyword : Parser ExtentKeyword
extentKeyword =
    keywordsToType
        [ ( "closest-corner", ClosestCorner )
        , ( "closest-side", ClosestSide )
        , ( "farthest-corner", FarthestCorner )
        , ( "farthest-side", FarthestSide )
        ]


{-| The [`image-orientation`](https://drafts.csswg.org/css-images-3/#propdef-image-orientation) property type.

Value definition syntax:

    from-image | <angle> | [ <angle>? flip ]

-}
type ImageOrientation
    = FromImage
    | ImageOrientationAngle V.Angle
    | FlippedAngle V.Angle
    | Flipped


imageOrientation : Parser ImageOrientation
imageOrientation =
    oneOf
        [ keyword "from-image" |> map (always FromImage)
        , keyword "flip" |> map (always Flipped)
        , succeed (\n m -> m n)
            |= V.angle
            |. whitespace
            |= oneOf
                [ keyword "flip" |> map (always FlippedAngle)
                , succeed ImageOrientationAngle
                ]
        ]


{-| The [`image-rendering`](https://drafts.csswg.org/css-images-3/#propdef-image-rendering) property type.

Value definition syntax:

    auto | smooth | high-quality | crisp-edges | pixelated

-}
type ImageRendering
    = Auto
    | Smooth
    | HighQuality
    | CrispEdges
    | Pixelated


imageRendering : Parser ImageRendering
imageRendering =
    keywordsToType
        [ ( "auto", Auto )
        , ( "smooth", Smooth )
        , ( "high-quality", HighQuality )
        , ( "crisp-edges", CrispEdges )
        , ( "pixelated", Pixelated )
        ]


{-| The [`object-fit`](https://drafts.csswg.org/css-images-3/#propdef-object-fit) property type.

Value definition syntax:

    fill | contain | cover | none | scale-down

-}
type ObjectFit
    = Fill
    | Contain
    | Cover
    | None
    | ScaleDown


objectFit : Parser ObjectFit
objectFit =
    keywordsToType
        [ ( "fill", Fill )
        , ( "contain", Contain )
        , ( "cover", Cover )
        , ( "none", None )
        , ( "scale-down", ScaleDown )
        ]
