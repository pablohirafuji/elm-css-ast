module CssAst exposing (..)

import Dict exposing (Dict)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail)
import Parser.LanguageKit as LanguageKit
import CssAst.Helpers exposing (whitespace)
import CssAst.Values exposing (identifier)
import CssAst.Align as Align exposing (Align)
import CssAst.Animations as Animations exposing (Animation)
import CssAst.Backgrounds as Bg exposing (Background)
import CssAst.Box as Box exposing (Box)
import CssAst.Break as Break exposing (Break)
import CssAst.Cascade as Cascade exposing (Cascade)
import CssAst.Color as Color exposing (Color)
import CssAst.Contain as Contain exposing (Contain)
import CssAst.Content as Content exposing (Content)
import CssAst.Display as Display
import CssAst.Exclusions as Exclusions
import CssAst.Images as Images exposing (Image)


---- TYPES ----


type alias StyleSheet =
    List Statement


type Statement
    = AtRule AtRule
    | Rule Rule



-- AT-RULE --


type AtRule
    = Import
    | Charset



-- RULE --


type alias Rule =
    ( List Selector, List Declaration )



-- SELECTOR --
-- https://drafts.csswg.org/selectors-4/


type alias Selector =
    ( CompoundSelector, List ComplexSelector )


type alias ComplexSelector =
    ( Combinator, CompoundSelector )


type Combinator
    = Descendant
    | Child
    | NextSibling
    | SubsequentSibling
    | Column


type alias CompoundSelector =
    ( ElementalSelector, List AttributeSelector )


type ElementalSelector
    = Universal (Maybe AttributeSelector)
    | TagName String


type alias AttributeSelector =
    ( AttributeName, Maybe ( AttributeMatcher, AttributeValue, CaseSensitivity ) )


type alias AttributeName =
    String


type AttributeMatcher
    = Equal
    | ContainsWord
    | Prefix
    | Suffix
    | Substring
    | HyphenatedPrefix


type alias AttributeValue =
    String


type CaseSensitivity
    = CaseSensitive
    | CaseInsensitive



---- PARSER ----


run : String -> Result Error StyleSheet
run =
    Parser.run stylesheet


stylesheet : Parser StyleSheet
stylesheet =
    succeed identity
        |. whitespace
        |= repeat zeroOrMore statement


statement : Parser Statement
statement =
    oneOf
        [ atRule |> map AtRule
        , rule |> map Rule
        ]



-- AT-RULE --


atRule : Parser AtRule
atRule =
    symbol "@"
        |> map (always Import)



-- RULE --


rule : Parser Rule
rule =
    succeed (,)
        |= selectorList
        |= declarationBlock
        |. whitespace


selectorList : Parser (List Selector)
selectorList =
    selector
        |> andThen (\n -> selectorListHelp [ n ])


selectorListHelp : List Selector -> Parser (List Selector)
selectorListHelp revSelectors =
    oneOf
        [ nextSelector
            |> andThen (\n -> selectorListHelp (n :: revSelectors))
        , succeed (List.reverse revSelectors)
        ]


nextSelector : Parser Selector
nextSelector =
    delayedCommit whitespace <|
        succeed identity
            |. symbol ","
            |. whitespace
            |= selector


selector : Parser Selector
selector =
    succeed (,)
        |= compoundSelector
        |. whitespace
        |= repeat zeroOrMore complexSelector


complexSelector : Parser ComplexSelector
complexSelector =
    oneOf
        [ succeed (,)
            |= combinator
            |. whitespace
            |= compoundSelector
            |. whitespace
        , succeed ((,) Descendant)
            |= compoundSelector
            |. whitespace
        ]


combinator : Parser Combinator
combinator =
    oneOf
        [ symbol ">>" |> map (always Descendant)
        , symbol ">" |> map (always Child)
        , symbol "+" |> map (always NextSibling)
        , symbol "~" |> map (always SubsequentSibling)
        , symbol "||" |> map (always Column)
        ]


compoundSelector : Parser CompoundSelector
compoundSelector =
    Parser.map2 (,)
        elementalSelector
        (repeat zeroOrMore attributeSelector)


elementalSelector : Parser ElementalSelector
elementalSelector =
    oneOf
        [ identifier |> map TagName
        , symbol "*" |> map (always (Universal Nothing))
        , attributeSelector |> map (Just >> Universal)
        ]


attributeSelector : Parser AttributeSelector
attributeSelector =
    oneOf
        [ Parser.map2
            (\_ n -> ( "id", Just ( Equal, n, CaseSensitive ) ))
            (symbol "#")
            identifier
        , Parser.map2
            (\_ n -> ( "class", Just ( ContainsWord, n, CaseSensitive ) ))
            (symbol ".")
            identifier
        , succeed (,)
            |. symbol "["
            |. whitespace
            |= identifier
            |. whitespace
            |= attributeSelectorHelp
            |. whitespace
            |. symbol "]"
        , symbol "::"
            |> map (always ( "as", Just ( Equal, "TODO", CaseSensitive ) ))
        , symbol ":"
            |> map (always ( "as", Just ( Equal, "TODO", CaseSensitive ) ))
        ]


attributeSelectorHelp : Parser (Maybe ( AttributeMatcher, String, CaseSensitivity ))
attributeSelectorHelp =
    oneOf
        [ succeed (\m s c -> Just ( m, s, c ))
            |= attributeMatcher
            |. whitespace
            |= identifier
            |. whitespace
            |= (oneOf
                    [ symbol "i" |> map (always CaseInsensitive)
                    , succeed CaseSensitive
                    ]
               )
        , succeed Nothing
        ]


attributeMatcher : Parser AttributeMatcher
attributeMatcher =
    oneOf
        [ symbol "~=" |> map (always ContainsWord)
        , symbol "|=" |> map (always HyphenatedPrefix)
        , symbol "^=" |> map (always Prefix)
        , symbol "$=" |> map (always Suffix)
        , symbol "*=" |> map (always Substring)
        , symbol "=" |> map (always Equal)
        ]



---- DECLARATION ----


type Declaration
    = Align Align
    | Animation Animation
    | Background Background
    | Box Box
    | Break Break
    | Cascade Cascade
    | Color Color
    | Contain Contain
    | Content Content
    | Display Display.Declaration
    | Exclusion Exclusions.Declaration
    | Image Image
    | Custom String String


declarationBlock : Parser (List Declaration)
declarationBlock =
    LanguageKit.sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = whitespace
        , item = declaration
        , trailing = LanguageKit.Optional
        }


declaration : Parser Declaration
declaration =
    property
        |> andThen
            (\n ->
                case Dict.get n declarationsDict of
                    Just parser ->
                        oneOf
                            [ oneOf
                                [ keyword "inherit"
                                , keyword "initial"
                                , keyword "unset"
                                ]
                                |> source
                                |> map (Custom n)
                            , parser
                            ]

                    Nothing ->
                        fail ("Invalid property: `" ++ n ++ "`.")
            )


property : Parser String
property =
    succeed identity
        |= identifier
        |. whitespace
        |. symbol ":"
        |. whitespace


declarationsDict : Dict String (Parser Declaration)
declarationsDict =
    Dict.fromList declarations


declarations : List ( String, Parser Declaration )
declarations =
    List.map (Tuple.mapSecond (map Align)) Align.declarations
        ++ List.map (Tuple.mapSecond (map Animation)) Animations.declarations
        ++ List.map (Tuple.mapSecond (map Background)) Bg.declarations
        ++ List.map (Tuple.mapSecond (map Box)) Box.declarations
        ++ List.map (Tuple.mapSecond (map Break)) Break.declarations
        ++ List.map (Tuple.mapSecond (map Cascade)) Cascade.declarations
        ++ List.map (Tuple.mapSecond (map Color)) Color.declarations
        ++ List.map (Tuple.mapSecond (map Contain)) Contain.declarations
        ++ List.map (Tuple.mapSecond (map Content)) Content.declarations
        ++ List.map (Tuple.mapSecond (map Display)) Display.declarations
        ++ List.map (Tuple.mapSecond (map Exclusion)) Exclusions.declarations
        ++ List.map (Tuple.mapSecond (map Image)) Images.declarations
