module CssAst.Color
    exposing
        ( Color(..)
        , declarations
        , Value(..)
        , ColorAdjust(..)
        , AlphaValue(..)
        , Rgb(..)
        , Hue(..)
        , value
        )

{-| [CSS Color](https://drafts.csswg.org/css-color-4/)

@docs Color,Value,ColorAdjust,AlphaValue,Rgb,Hue, value, declarations

TODO: device-cmyk(), color-mod()

-}

import Char
import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, delayedCommitMap)
import CssAst.Values as V
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


{-| The Color type.

[Source](https://drafts.csswg.org/css-color-4/#property-index).

-}
type Color
    = Color Value
    | ColorAdjust ColorAdjust
    | Opacity AlphaValue


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Color )
declarations =
    [ ( "color", map Color value )
    , ( "color-adjust", map ColorAdjust colorAdjust )
    , ( "opacity", map Opacity alphaValue )
    ]


{-| The possible values for the [`<color>`](https://drafts.csswg.org/css-color-4/#typedef-color) data type.

Value definition syntax:
`<rgb()> | <rgba()> | <hsl()> | <hsla()> | <hwb()> | <gray()> | <device-cmyk()> | <color-mod()> | <hex-color> | <named-color> | currentcolor | <deprecated-system-color>`

-}
type Value
    = Rgb Rgb
    | Hsl Hue V.Percentage V.Percentage (Maybe AlphaValue)
    | Hwb Hue V.Percentage V.Percentage (Maybe AlphaValue)
    | Gray Float (Maybe AlphaValue)
      --| DeviceCmyk
      --| ColorMod
    | Hex String
    | Named String
    | Current
    | Transparent


value : Parser Value
value =
    oneOf
        [ succeed Rgb
            |. oneOf
                [ keyword "rgb("
                , keyword "rgba("
                ]
            |. whitespace
            |= rgbValue
            |. whitespace
            |. symbol ")"
        , succeed identity
            |. oneOf
                [ keyword "hsl("
                , keyword "hsla("
                ]
            |. whitespace
            |= hslOrHwb Hsl
            |. whitespace
            |. symbol ")"
        , succeed identity
            |. keyword "hwb("
            |. whitespace
            |= hslOrHwb Hwb
            |. whitespace
            |. symbol ")"
        , succeed Gray
            |. keyword "gray("
            |. whitespace
            |= V.number
            |. whitespace
            |= oneOf
                [ succeed Just
                    |. symbol "/"
                    |. whitespace
                    |= alphaValue
                , succeed Nothing
                ]
            |. whitespace
            |. symbol ")"
        , delayedCommit (symbol "#") (keep oneOrMore Char.isHexDigit)
            |> andThen
                (\n ->
                    let
                        hLength =
                            String.length n
                    in
                        if hLength == 3 || hLength == 4 || hLength == 6 || hLength == 8 then
                            succeed (Hex n)
                        else
                            fail ("Invalid hex value: " ++ n ++ "`.")
                )
        , keyword "currentcolor" |> map (always Current)
        , keyword "transparent" |> map (always Transparent)
        , delayedCommitMap (\n m -> n)
            (V.identifier
                |> andThen
                    (\n ->
                        if Set.member n namedColors then
                            succeed (Named n)
                        else
                            fail ("Invalid color name: `" ++ n ++ "`.")
                    )
            )
            (succeed ())
        ]


{-| The possible values for the [`rgb()`](https://drafts.csswg.org/css-color-4/#funcdef-rgb) function.

Value definition syntax:
`rgb( <percentage>{3} [ / <alpha-value> ]? ) | rgb( <number>{3} [ / <alpha-value> ]? )`

Legacy value definition syntax:
`rgb( <percentage>#{3} , <alpha-value>? ) | rgb( <number>#{3} , <alpha-value>? )`

-}
type Rgb
    = RgbNumber Float Float Float (Maybe AlphaValue)
    | RgbPercentage V.Percentage V.Percentage V.Percentage (Maybe AlphaValue)


rgbValue : Parser Rgb
rgbValue =
    let
        spaceSep t p =
            succeed t
                |= p
                |. whitespace
                |= p
                |. whitespace
                |= p
                |. whitespace
                |= oneOf
                    [ succeed Just
                        |. symbol "/"
                        |. whitespace
                        |= alphaValue
                    , succeed Nothing
                    ]

        commaSep t p =
            succeed t
                |= p
                |. whitespace
                |. symbol ","
                |. whitespace
                |= p
                |. whitespace
                |. symbol ","
                |. whitespace
                |= p
                |. whitespace
                |= oneOf
                    [ succeed Just
                        |. symbol ","
                        |. whitespace
                        |= alphaValue
                    , succeed Nothing
                    ]
    in
        oneOf
            [ delayedCommitMap (\n _ -> n)
                (spaceSep RgbPercentage V.percentage)
                (succeed ())
            , delayedCommitMap (\n _ -> n)
                (spaceSep RgbNumber V.number)
                (succeed ())
            , delayedCommitMap (\n _ -> n)
                (commaSep RgbPercentage V.percentage)
                (succeed ())
            , delayedCommitMap (\n _ -> n)
                (commaSep RgbNumber V.number)
                (succeed ())
            ]


{-| The possible values for the [`<hue>`](https://drafts.csswg.org/css-color-4/#typedef-hue) data type.

Value definition syntax:
`<number> | <angle>`

-}
type Hue
    = HueNumber Float
    | HueAngle V.Angle


hue : Parser Hue
hue =
    oneOf
        [ V.angle |> map HueAngle
        , V.number |> map HueNumber
        ]


hslOrHwb : (Hue -> V.Percentage -> V.Percentage -> Maybe AlphaValue -> Value) -> Parser Value
hslOrHwb t =
    let
        spaceSep =
            succeed t
                |= hue
                |. whitespace
                |= V.percentage
                |. whitespace
                |= V.percentage
                |. whitespace
                |= oneOf
                    [ succeed Just
                        |. symbol "/"
                        |. whitespace
                        |= alphaValue
                    , succeed Nothing
                    ]

        commaSep =
            succeed t
                |= hue
                |. whitespace
                |. symbol ","
                |. whitespace
                |= V.percentage
                |. whitespace
                |. symbol ","
                |. whitespace
                |= V.percentage
                |. whitespace
                |= oneOf
                    [ succeed Just
                        |. symbol ","
                        |. whitespace
                        |= alphaValue
                    , succeed Nothing
                    ]
    in
        oneOf
            [ delayedCommitMap (\n _ -> n)
                spaceSep
                (succeed ())
            , delayedCommitMap (\n _ -> n)
                commaSep
                (succeed ())
            ]


{-| The possible values for the [`color-adjust`](https://drafts.csswg.org/css-color-4/#propdef-color-adjust) property.

Value definition syntax:
`economy | exact`

-}
type ColorAdjust
    = Economy
    | Exact


colorAdjust : Parser ColorAdjust
colorAdjust =
    oneOf
        [ keyword "economy" |> map (always Economy)
        , keyword "exact" |> map (always Exact)
        ]


{-| The possible values for the [`<alpha-value>`](https://drafts.csswg.org/css-color-4/#typedef-alpha-value) data type.

Value definition syntax:
`<number> | <percentage>`

-}
type AlphaValue
    = Number Float
    | Percentage V.Percentage


alphaValue : Parser AlphaValue
alphaValue =
    oneOf
        [ V.percentage |> map Percentage
        , V.number |> map Number
        ]



-- Source: https://drafts.csswg.org/css-color-4/#named-colors


namedColors : Set String
namedColors =
    Set.fromList
        [ "antiquewhite"
        , "aqua"
        , "aquamarine"
        , "azure"
        , "beige"
        , "bisque"
        , "black"
        , "blanchedalmond"
        , "blue"
        , "blueviolet"
        , "brown"
        , "burlywood"
        , "cadetblue"
        , "chartreuse"
        , "chocolate"
        , "coral"
        , "cornflowerblue"
        , "cornsilk"
        , "crimson"
        , "cyan"
        , "darkblue"
        , "darkcyan"
        , "darkgoldenrod"
        , "darkgray"
        , "darkgreen"
        , "darkgrey"
        , "darkkhaki"
        , "darkmagenta"
        , "darkolivegreen"
        , "darkorange"
        , "darkorchid"
        , "darkred"
        , "darksalmon"
        , "darkseagreen"
        , "darkslateblue"
        , "darkslategray"
        , "darkslategrey"
        , "darkturquoise"
        , "darkviolet"
        , "deeppink"
        , "deepskyblue"
        , "dimgray"
        , "dimgrey"
        , "dodgerblue"
        , "firebrick"
        , "floralwhite"
        , "forestgreen"
        , "fuchsia"
        , "gainsboro"
        , "ghostwhite"
        , "gold"
        , "goldenrod"
        , "gray"
        , "green"
        , "greenyellow"
        , "grey"
        , "honeydew"
        , "hotpink"
        , "indianred"
        , "indigo"
        , "ivory"
        , "khaki"
        , "lavender"
        , "lavenderblush"
        , "lawngreen"
        , "lemonchiffon"
        , "lightblue"
        , "lightcoral"
        , "lightcyan"
        , "lightgoldenrodyellow"
        , "lightgray"
        , "lightgreen"
        , "lightgrey"
        , "lightpink"
        , "lightsalmon"
        , "lightseagreen"
        , "lightskyblue"
        , "lightslategray"
        , "lightslategrey"
        , "lightsteelblue"
        , "lightyellow"
        , "lime"
        , "limegreen"
        , "linen"
        , "magenta"
        , "maroon"
        , "mediumaquamarine"
        , "mediumblue"
        , "mediumorchid"
        , "mediumpurple"
        , "mediumseagreen"
        , "mediumslateblue"
        , "mediumspringgreen"
        , "mediumturquoise"
        , "mediumvioletred"
        , "midnightblue"
        , "mintcream"
        , "mistyrose"
        , "moccasin"
        , "navajowhite"
        , "navy"
        , "oldlace"
        , "olive"
        , "olivedrab"
        , "orange"
        , "orangered"
        , "orchid"
        , "palegoldenrod"
        , "palegreen"
        , "paleturquoise"
        , "palevioletred"
        , "papayawhip"
        , "peachpuff"
        , "peru"
        , "pink"
        , "plum"
        , "powderblue"
        , "purple"
        , "rebeccapurple"
        , "red"
        , "rosybrown"
        , "royalblue"
        , "saddlebrown"
        , "salmon"
        , "sandybrown"
        , "seagreen"
        , "seashell"
        , "sienna"
        , "silver"
        , "skyblue"
        , "slateblue"
        , "slategray"
        , "slategrey"
        , "snow"
        , "springgreen"
        , "steelblue"
        , "tan"
        , "teal"
        , "thistle"
        , "tomato"
        , "turquoise"
        , "violet"
        , "wheat"
        , "white"
        , "whitesmoke"
        , "yellow"
        , "yellowgreen"
        ]
