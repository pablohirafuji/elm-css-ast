module CssAst.Values exposing (..)

import Dict exposing (Dict)
import Parser exposing (Parser, symbol, oneOf, map, andThen, succeed, delayedCommitMap)
import CssAst.Helpers exposing (identifier)


type WideKeyword
    = Inherit
    | Initial
    | Unset


integer : Parser Int
integer =
    Parser.int


number : Parser Float
number =
    Parser.float


type alias Percentage =
    Float


percentage : Parser Float
percentage =
    Parser.delayedCommitMap (\a _ -> a)
        number
        (symbol "%")


type LengthPercentage
    = LPLength Length
    | LPPercentage Float


lengthPercentage : Parser LengthPercentage
lengthPercentage =
    oneOf
        [ percentage |> map LPPercentage
        , length |> map LPLength
        ]



-- https://drafts.csswg.org/css-values-4/#length-value


type alias Length =
    ( Float, LengthUnit )


type LengthUnit
    = Em
    | Ex
    | Cap
    | Ch
    | Ic
    | Rem
    | Lh
    | Rlh
    | Vw
    | Vh
    | Vi
    | Vb
    | Vmin
    | Vmax
    | Cm
    | Mm
    | Q
    | In
    | Pc
    | Pt
    | Px


lengthUnits : Dict String LengthUnit
lengthUnits =
    Dict.fromList
        [ ( "em", Em )
        , ( "ex", Ex )
        , ( "cap", Cap )
        , ( "ch", Ch )
        , ( "ic", Ic )
        , ( "rem", Rem )
        , ( "lh", Lh )
        , ( "rlh", Rlh )
        , ( "vw", Vw )
        , ( "vh", Vh )
        , ( "vi", Vi )
        , ( "vb", Vb )
        , ( "vmin", Vmin )
        , ( "vmax", Vmax )
        , ( "cm", Cm )
        , ( "mm", Mm )
        , ( "Q", Q )
        , ( "in", In )
        , ( "pc", Pc )
        , ( "pt", Pt )
        , ( "px", Px )
        ]


length : Parser Length
length =
    delayedCommitMap (,)
        number
        (identifier
            |> andThen
                (\n ->
                    case Dict.get n lengthUnits of
                        Just unit ->
                            succeed unit

                        Nothing ->
                            Parser.fail "Invalid length unit"
                 --TODO: better error msg (add valid units)
                )
        )



-- https://drafts.csswg.org/css-values-4/#angle-value


type Angle
    = Deg
    | Grad
    | Rad
    | Turn


angleUnits : Dict String Angle
angleUnits =
    Dict.fromList
        [ ( "deg", Deg )
        , ( "grad", Grad )
        , ( "rad", Rad )
        , ( "turn", Turn )
        ]



-- https://drafts.csswg.org/css-values-4/#time-value


type Time
    = S Float
    | Ms Float


time : Parser Time
time =
    delayedCommitMap (\a b -> b a)
        number
        (oneOf
            [ symbol "s" |> map (always S)
            , symbol "ms" |> map (always Ms)
            ]
        )



-- https://drafts.csswg.org/css-values-4/#frequency-value


type Frequency
    = Hz
    | Khz


frequencyUnits : Dict String Frequency
frequencyUnits =
    Dict.fromList
        [ ( "Hz", Hz )
        , ( "hz", Hz )
        , ( "kHz", Khz )
        , ( "khz", Khz )
        ]



-- https://drafts.csswg.org/css-values-4/#resolution-value


type Resolution
    = Dpi
    | Dpcm
    | Dppx


resolutionUnits : Dict String Resolution
resolutionUnits =
    Dict.fromList
        [ ( "dpi", Dpi )
        , ( "dpcm", Dpcm )
        , ( "dppx", Dppx )
        , ( "x", Dppx )
        ]
