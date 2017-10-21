module CssAst.Helpers exposing (whitespace, toMaybe, anyOrder2, keywordsToType, oneOrMoreCommaList)

import Parser exposing (Parser, zeroOrMore, oneOrMore, ignore, Count(..), source, (|.), (|=), oneOf, succeed, map, keyword, symbol, delayedCommit, repeat)
import Parser.LanguageKit as LanguageKit


whitespace : Parser ()
whitespace =
    LanguageKit.whitespace
        { allowTabs = False
        , lineComment = LanguageKit.NoLineComment
        , multiComment = LanguageKit.UnnestableComment "/*" "*/"
        }


toMaybe : Parser a -> Parser (Maybe a)
toMaybe parser =
    oneOf
        [ parser |> map Just
        , succeed Nothing
        ]


anyOrder2 : (b -> c -> a) -> Parser b -> Parser c -> Parser a
anyOrder2 type_ b c =
    oneOf
        [ succeed (\c_ b_ -> type_ b_ c_)
            |= c
            |. whitespace
            |= b
        , succeed type_
            |= b
            |. whitespace
            |= c
        ]


keywordsToType : List ( String, a ) -> Parser a
keywordsToType =
    List.map (\( n, t ) -> keyword n |> map (always t))
        >> oneOf


oneOrMoreCommaList : (a -> List a -> b) -> Parser a -> Parser b
oneOrMoreCommaList fn n =
    succeed fn
        |= n
        |. whitespace
        |= repeat zeroOrMore
            (succeed identity
                |. symbol ","
                |. whitespace
                |= n
                |. whitespace
            )



--zeroOrMoreWhitespaces : Parser ()
--zeroOrMoreWhitespaces =
--    ignore zeroOrMore isWhitespace
--oneOrMoreWhitespaces : Parser ()
--oneOrMoreWhitespaces =
--    ignore oneOrMore isWhitespace
--isWhitespace : Char -> Bool
--isWhitespace c =
--    Set.member c whitespaceSet
--whitespaceSet : Set Char
--whitespaceSet =
--    Set.fromList [ ' ', '\n', '\t', '\x0D', '\x0C' ]
