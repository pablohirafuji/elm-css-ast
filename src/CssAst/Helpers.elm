module CssAst.Helpers
    exposing
        ( whitespace
        , oneOrMoreWhitespace
        , toMaybe
        , anyOrder2
        , keywordsToType
        , oneOrMoreCommaList
        , isWhitespace
        , resultToParser
        , function
        )

import Set exposing (Set)
import Parser exposing (Parser, zeroOrMore, oneOrMore, ignore, Count(..), source, (|.), (|=), oneOf, succeed, map, keyword, symbol, delayedCommit, repeat, fail)


whitespace : Parser ()
whitespace =
    whitespaceHelp
        |> repeat zeroOrMore
        |> map (always ())


oneOrMoreWhitespace : Parser ()
oneOrMoreWhitespace =
    whitespaceHelp
        |> repeat oneOrMore
        |> map (always ())


whitespaceHelp : Parser ()
whitespaceHelp =
    oneOf
        [ symbol "/*"
            |. Parser.ignoreUntil "*/"
        , ignore oneOrMore isWhitespace
        ]


toMaybe : Parser a -> Parser (Maybe a)
toMaybe parser =
    oneOf
        [ parser |> map Just
        , succeed Nothing
        ]


anyOrder2 : (b -> c -> a) -> Parser b -> Parser c -> Parser a
anyOrder2 type_ b c =
    oneOf
        [ succeed type_
            |= b
            |. whitespace
            |= c
        , succeed (\c_ b_ -> type_ b_ c_)
            |= c
            |. whitespace
            |= b
        ]


keywordsToType : List ( String, a ) -> Parser a
keywordsToType =
    List.map (\( n, t ) -> keyword n |> map (always t))
        >> oneOf


oneOrMoreCommaList : Parser a -> Parser (List a)
oneOrMoreCommaList n =
    succeed (::)
        |= n
        |. whitespace
        |= repeat zeroOrMore
            (succeed identity
                |. symbol ","
                |. whitespace
                |= n
                |. whitespace
            )


isWhitespace : Char -> Bool
isWhitespace c =
    Set.member c whitespaceSet


whitespaceSet : Set Char
whitespaceSet =
    Set.fromList [ ' ', '\n', '\t', '\x0D', '\x0C' ]



{- When validating parsers, using Result enable validating only the AST in the future. This function transform a result into a parser. -}


resultToParser : Result String a -> Parser a
resultToParser result =
    case result of
        Result.Err err ->
            fail err

        Result.Ok ok ->
            succeed ok


function : String -> Parser a -> Parser a
function str parser =
    succeed identity
        |. keyword (str ++ "(")
        |. whitespace
        |= parser
        |. whitespace
        |. symbol ")"
