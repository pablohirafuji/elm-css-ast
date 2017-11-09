module CssAst.Contain
    exposing
        ( Contain(..)
        , declarations
        , ContainValue(..)
        , CustomContainValue
        )

{-| [CSS Containment](https://drafts.csswg.org/css-contain-1/)

@docs Contain, ContainValue, CustomContainValue, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


{-| The Contain type.

[Source](https://drafts.csswg.org/css-contain-1/#property-index).

-}
type Contain
    = Contain ContainValue


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Contain )
declarations =
    [ ( "contain", containValue |> map Contain )
    ]


{-| The [`contain`](https://drafts.csswg.org/css-contain-1/#contain-property) property type.

Value definition syntax:

    none | strict | content | [ size || layout || style || paint ]

-}
type ContainValue
    = None
    | Strict
    | Content
    | Custom CustomContainValue


containValue : Parser ContainValue
containValue =
    oneOf
        [ keywordsToType
            [ ( "none", None )
            , ( "strict", Strict )
            , ( "content", Content )
            ]
        , customContainValue |> map Custom
        ]


{-| The [`contain`](https://drafts.csswg.org/css-contain-1/#contain-property) property custom type alias.

Value definition syntax:

    size || layout || style || paint

-}
type alias CustomContainValue =
    { size : Bool
    , layout : Bool
    , style : Bool
    , paint : Bool
    }


customContainValue : Parser CustomContainValue
customContainValue =
    customContainValueHelp [] customContainValueList emptyCustomContainValue


customContainValueHelp : List (CustomContainValue -> Parser CustomContainValue) -> List (CustomContainValue -> Parser CustomContainValue) -> CustomContainValue -> Parser CustomContainValue
customContainValueHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (customContainValueHelp [] (nsHead ++ tail))
                , customContainValueHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n.size == False && n.layout == False && n.style == False && n.paint == False then
                fail "Empty `contain` property value."
            else
                succeed n


customContainValueList : List (CustomContainValue -> Parser CustomContainValue)
customContainValueList =
    [ \r -> map (always { r | size = True }) (keyword "size")
    , \r -> map (always { r | layout = True }) (keyword "layout")
    , \r -> map (always { r | style = True }) (keyword "style")
    , \r -> map (always { r | paint = True }) (keyword "paint")
    ]


emptyCustomContainValue : CustomContainValue
emptyCustomContainValue =
    { size = False
    , layout = False
    , style = False
    , paint = False
    }
