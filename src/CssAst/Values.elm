module CssAst.Values exposing (..)

import Char
import Dict exposing (Dict)
import Parser exposing (Parser, symbol, oneOf, map, andThen, succeed, delayedCommitMap, ignore, Count(..), source, zeroOrMore, (|.), keyword, keep, (|=), fail)
import CssAst.Helpers exposing (isWhitespace, whitespace, keywordsToType, toMaybe, resultToParser)


{-| The [CSS-wide keywords](https://drafts.csswg.org/css-values-4/#common-keywords) type.
-}
type WideKeyword
    = Inherit
    | Initial
    | Unset


{-| The [`<ident-token>`](https://drafts.csswg.org/css-syntax-3/#typedef-ident-token) parser.

    import Parser

    Parser.run identifier "class"
    --> Result.Ok "class"

    Parser.run identifier "my-class"
    --> Result.Ok "my-class"

    Parser.run identifier "0class" |> Result.mapError .problem
    --> Result.Err (Parser.BadOneOf [Parser.ExpectingSymbol "--",Parser.ExpectingSymbol "-",Parser.BadRepeat])

    Parser.run identifier "class()"
    --> Result.Ok "class"

-}
identifier : Parser String
identifier =
    oneOf
        [ symbol "--"
            |. ignore zeroOrMore isIdentifierBodyChar
        , symbol "-"
            |. identifierHelp
        , identifierHelp
        ]
        |> source


identifierHelp : Parser ()
identifierHelp =
    ignore (Exactly 1) isIdentifierStartChar
        |. ignore zeroOrMore isIdentifierBodyChar


isIdentifierStartChar : Char -> Bool
isIdentifierStartChar c =
    Char.isUpper c || Char.isLower c || c == '_' || c == '\\' || Char.toCode c > 127


isIdentifierBodyChar : Char -> Bool
isIdentifierBodyChar c =
    isIdentifierStartChar c || Char.isDigit c || c == '-'


{-| The [`<string>`](https://drafts.csswg.org/css-values-4/#strings) type parser.

    import Parser

    Parser.run string "\"this is a 'string'.\""
    --> Result.Ok "this is a 'string'."

    Parser.run string "\"this is a \\\"string\\\".\""
    --> Result.Ok "this is a \\\"string\\\"."

    Parser.run string "'this is a \"string\".'"
    --> Result.Ok "this is a \"string\"."

    Parser.run string "'this is a \\'string\\'.'"
    --> Result.Ok "this is a \\'string\\'."

    Parser.run string "my string" |> Result.mapError .problem
    --> Result.Err (Parser.BadOneOf [Parser.BadRepeat, Parser.BadRepeat])

-}
string : Parser String
string =
    oneOf
        [ stringHelp '"'
        , stringHelp '\''
        ]


stringHelp : Char -> Parser String
stringHelp c =
    succeed identity
        |. ignore (Exactly 1) ((==) c)
        |= stringHelp_ c ""
        |. ignore (Exactly 1) ((==) c)


stringHelp_ : Char -> String -> Parser String
stringHelp_ c n =
    keep zeroOrMore (\c_ -> not (c_ == c || c_ == '\\'))
        |> andThen
            (\n_ ->
                oneOf
                    [ succeed ()
                        |. ignore (Exactly 1) ((==) '\\')
                        |. ignore (Exactly 1) (always True)
                        |> source
                        |> andThen ((++) (n ++ n_) >> stringHelp_ c)
                    , succeed (n ++ n_)
                    ]
            )


{-| The [`<url>`](https://drafts.csswg.org/css-values-4/#url-value) type parser.

    import Parser

    Parser.run url "url(  http://elm-lang.org/  )"
    --> Result.Ok "http://elm-lang.org/"

    Parser.run url "url(\"http://elm-lang.org/ url\")"
    --> Result.Ok "http://elm-lang.org/ url"

    Parser.run url "url(http://elm-lang.org/ url)" |> Result.mapError .problem
    --> Result.Err (Parser.ExpectingSymbol ")")

-}
url : Parser String
url =
    succeed identity
        |. keyword "url("
        |. whitespace
        |= oneOf
            [ string
            , keep zeroOrMore (\c -> not (c == '"' || c == '\'' || c == '(' || c == ')' || c == '\\' || isWhitespace c))
            ]
        |. whitespace
        |. symbol ")"


integer : Parser Int
integer =
    Parser.int


number : Parser Float
number =
    Parser.float


type alias Percentage =
    Float


{-| The [`<percentage>`](https://drafts.csswg.org/css-syntax-3/#typedef-percentage-token) type parser.

    import Parser

    Parser.run percentage "10%"
    --> Result.Ok 10

    Parser.run percentage "10.5%"
    --> Result.Ok 10.5

    Parser.run percentage "10" |> Result.mapError .problem
    --> Result.Err (Parser.ExpectingSymbol "%")

-}
percentage : Parser Float
percentage =
    Parser.delayedCommitMap (\a _ -> a)
        number
        (symbol "%")


{-| The [`<length-percentage>`](https://drafts.csswg.org/css-values-4/#typedef-length-percentage) data type.

Value definition syntax:

    [ <length> | <percentage> ]

-}
type LengthPercentage
    = LPLength Length
    | LPPercentage Float


{-| The [`<length-percentage>`](https://drafts.csswg.org/css-syntax-3/#typedef-percentage-token) parser.

    import Parser

    Parser.run lengthPercentage "10%"
    --> Result.Ok (LPPercentage 10)

    Parser.run lengthPercentage "10.5%"
    --> Result.Ok (LPPercentage 10.5)

    Parser.run lengthPercentage "10px"
    --> Result.Ok (LPLength (10,Px))

-}
lengthPercentage : Parser LengthPercentage
lengthPercentage =
    oneOf
        [ percentage |> map LPPercentage
        , length |> map LPLength
        ]


type NumberPercentage
    = NPNumber Float
    | NPPercentage Float


numberPercentage : Parser NumberPercentage
numberPercentage =
    oneOf
        [ percentage |> map NPPercentage
        , number |> map NPNumber
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
    = Deg Float
    | Grad Float
    | Rad Float
    | Turn Float


angle : Parser Angle
angle =
    delayedCommitMap (\a b -> b a)
        number
        (oneOf
            [ keyword "deg" |> map (always Deg)
            , keyword "grad" |> map (always Grad)
            , keyword "rad" |> map (always Rad)
            , keyword "turn" |> map (always Turn)
            ]
        )



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


{-| The [`<position>`](https://drafts.csswg.org/css-values-4/#typedef-position) data type.

Value definition syntax:

    [
      [ left | center | right ] || [ top | center | bottom ]
    |
      [ left | center | right | <length-percentage> ]
      [ top | center | bottom | <length-percentage> ]?
    |
      [ [ left | right ] <length-percentage> ] &&
      [ [ top | bottom ] <length-percentage> ]
    ]

-}
type alias Position =
    ( PositionValue, Maybe PositionValue )


{-| A `Position` parser.

    import Parser

    Parser.run position "left"
    --> Result.Ok ( SidePosition Left Nothing, Nothing )

    Parser.run position "left center"
    --> Result.Ok ( SidePosition Left Nothing, Just CenterPosition )

    Parser.run position "bottom right"
    --> Result.Ok ( SidePosition Bottom Nothing, Just (SidePosition Right Nothing) )

    Parser.run position "10% bottom"
    --> Result.Ok (LengthPercentagePosition (LPPercentage 10),Just (SidePosition Bottom Nothing))

    Parser.run position "left 20% top 50%"
    --> Result.Ok (SidePosition Left (Just (LPPercentage 20)),Just (SidePosition Top (Just (LPPercentage 50))))

    Parser.run position "left right" |> Result.mapError .problem
    --> Result.Err (Parser.Fail "Invalid `<position>` data value.")

-}
position : Parser Position
position =
    succeed identity
        |= positionValue
        |. whitespace
        |> andThen
            (\n ->
                case n of
                    SidePosition _ (Just _) ->
                        succeed SidePosition
                            |= side
                            |. whitespace
                            |= (lengthPercentage |> map Just)
                            |> map (Just >> (,) n)

                    _ ->
                        succeed ((,) n)
                            |= oneOf
                                [ keyword "center" |> map (\_ -> Just CenterPosition)
                                , lengthPercentage |> map (LengthPercentagePosition >> Just)
                                , succeed SidePosition
                                    |= side
                                    |= succeed Nothing
                                    |> map Just
                                , succeed Nothing
                                ]
            )
        |> map validatePosition
        |> andThen resultToParser


validatePosition : Position -> Result String Position
validatePosition ( p, m ) =
    case ( p, m ) of
        ( SidePosition _ (Just _), Nothing ) ->
            Result.Err "Invalid `<position>` data value."

        ( SidePosition _ (Just _), Just (SidePosition _ Nothing) ) ->
            Result.Err "Invalid `<position>` data value."

        ( SidePosition _ Nothing, Just (SidePosition _ (Just _)) ) ->
            Result.Err "Invalid `<position>` data value."

        ( SidePosition dir1 _, Just (SidePosition dir2 _) ) ->
            if
                ((dir1 == Left || dir1 == Right) && (dir2 == Left || dir2 == Right))
                    || ((dir1 == Top || dir1 == Bottom) && (dir2 == Top || dir2 == Bottom))
            then
                Result.Err "Invalid `<position>` data value."
            else
                Result.Ok ( p, m )

        _ ->
            Result.Ok ( p, m )


type PositionValue
    = CenterPosition
    | SidePosition Side (Maybe LengthPercentage)
    | LengthPercentagePosition LengthPercentage


positionValue : Parser PositionValue
positionValue =
    oneOf
        [ keyword "center" |> map (always CenterPosition)
        , lengthPercentage |> map LengthPercentagePosition
        , succeed SidePosition
            |= side
            |. whitespace
            |= toMaybe lengthPercentage
        ]


{-| The `side` helper type.

Value definition syntax:

    top | right | bottom | left

-}
type Side
    = Top
    | Right
    | Bottom
    | Left


{-| A `Side` parser.

    import Parser

    Parser.run side "top"
    --> Result.Ok Top

    Parser.run side "right"
    --> Result.Ok Right

    Parser.run side "bottom"
    --> Result.Ok Bottom

    Parser.run side "left"
    --> Result.Ok Left

-}
side : Parser Side
side =
    keywordsToType
        [ ( "top", Top )
        , ( "right", Right )
        , ( "bottom", Bottom )
        , ( "left", Left )
        ]
