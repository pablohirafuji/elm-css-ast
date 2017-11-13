module CssAst.Display
    exposing
        ( Declaration(..)
        , declarations
        , Display(..)
        , DisplayOutside(..)
        , DisplayInside(..)
        , DisplayListitem(..)
        , FlowOrFlowRoot(..)
        , DisplayInternal(..)
        , DisplayBox(..)
        , DisplayLegacy(..)
        )

{-| [CSS Display](https://drafts.csswg.org/css-display-3/)

@docs Declaration, Display, DisplayOutside, DisplayInside, DisplayListitem, FlowOrFlowRoot, DisplayInternal, DisplayBox, DisplayLegacy, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


{-| The Display [declarations](https://drafts.csswg.org/css-display-3/#property-index) type.
-}
type Declaration
    = Display Display


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Declaration )
declarations =
    [ ( "display", display |> map Display )
    ]


{-| The [`display`](https://drafts.csswg.org/css-display-3/#propdef-display) property type.

Value definition syntax:

    [ <display-outside> || <display-inside> ] | <display-listitem> | <display-internal> | <display-box> | <display-legacy>

-}
type Display
    = DisplayOutsideInside (Maybe DisplayOutside) (Maybe DisplayInside)
    | DisplayListitem DisplayListitem
    | DisplayInternal DisplayInternal
    | DisplayBox DisplayBox
    | DisplayLegacy DisplayLegacy


display : Parser Display
display =
    oneOf
        [ displayListitem |> map DisplayListitem
        , displayInternal |> map DisplayInternal
        , displayBox |> map DisplayBox
        , displayLegacy |> map DisplayLegacy
        , succeed DisplayOutsideInside
            |= (displayOutside |> map Just)
            |. whitespace
            |= toMaybe displayInside
        , succeed (flip DisplayOutsideInside)
            |= (displayInside |> map Just)
            |. whitespace
            |= toMaybe displayOutside
        ]


{-| The [`<display-outside>`](https://drafts.csswg.org/css-display-3/#typedef-display-outside) data type.

Value definition syntax:

    block | inline | run-in

-}
type DisplayOutside
    = Block
    | Inline
    | RunIn


displayOutside : Parser DisplayOutside
displayOutside =
    keywordsToType
        [ ( "block", Block )
        , ( "inline", Inline )
        , ( "run-in", RunIn )
        ]


{-| The [`<display-inside>`](https://drafts.csswg.org/css-display-3/#typedef-display-inside) data type.

Value definition syntax:

    flow | flow-root | table | flex | grid | ruby

-}
type DisplayInside
    = Flow
    | FlowRoot
    | Table
    | Flex
    | Grid
    | Ruby


displayInside : Parser DisplayInside
displayInside =
    keywordsToType
        [ ( "flow", Flow )
        , ( "flow-root", FlowRoot )
        , ( "table", Table )
        , ( "flex", Flex )
        , ( "grid", Grid )
        , ( "ruby", Ruby )
        ]


{-| The [`<display-listitem>`](https://drafts.csswg.org/css-display-3/#typedef-display-listitem) data type.

Value definition syntax:

    list-item && <display-outside>? && [ flow | flow-root ]?

-}
type DisplayListitem
    = Listitem ( Maybe DisplayOutside, Maybe FlowOrFlowRoot )


type alias DisplayListitemHelp =
    { hasListitem : Bool
    , displayOutside : Maybe DisplayOutside
    , flow : Maybe FlowOrFlowRoot
    }


displayListitem : Parser DisplayListitem
displayListitem =
    Parser.delayedCommitMap (\a _ -> a)
        (displayListitemHelp [] displayListitemHelpParsers emptyDisplayListitemHelp)
        (succeed ())


displayListitemHelp : List (DisplayListitemHelp -> Parser DisplayListitemHelp) -> List (DisplayListitemHelp -> Parser DisplayListitemHelp) -> DisplayListitemHelp -> Parser DisplayListitem
displayListitemHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (displayListitemHelp [] (nsHead ++ tail))
                , displayListitemHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n.hasListitem then
                succeed (Listitem ( n.displayOutside, n.flow ))
            else
                fail "Missing `list-item` value."


displayListitemHelpParsers : List (DisplayListitemHelp -> Parser DisplayListitemHelp)
displayListitemHelpParsers =
    [ \r -> map (\n -> { r | hasListitem = True }) (keyword "list-item")
    , \r -> map (\n -> { r | displayOutside = Just n }) displayOutside
    , \r -> map (\n -> { r | flow = Just n }) flowOrFlowRoot
    ]


emptyDisplayListitemHelp : DisplayListitemHelp
emptyDisplayListitemHelp =
    { hasListitem = False
    , displayOutside = Nothing
    , flow = Nothing
    }


{-| The `FlowOrFlowRoot` helper type.

Value definition syntax:

    flow | flow-root

-}
type FlowOrFlowRoot
    = FFlow
    | FFlowRoot


flowOrFlowRoot : Parser FlowOrFlowRoot
flowOrFlowRoot =
    keywordsToType
        [ ( "flow", FFlow )
        , ( "flow-root", FFlowRoot )
        ]


{-| The [`<display-internal>`](https://drafts.csswg.org/css-display-3/#typedef-display-internal) data type.

Value definition syntax:

    table-row-group | table-header-group |
    table-footer-group | table-row | table-cell |
    table-column-group | table-column | table-caption |
    ruby-base | ruby-text | ruby-base-container |
    ruby-text-container

-}
type DisplayInternal
    = TableRowGroup
    | TableHeaderGroup
    | TableFooterGroup
    | TableRow
    | TableCell
    | TableColumnGroup
    | TableColumn
    | TableCaption
    | RubyBase
    | RubyText
    | RubyBaseContainer
    | RubyTextContainer


displayInternal : Parser DisplayInternal
displayInternal =
    keywordsToType
        [ ( "table-row-group", TableRowGroup )
        , ( "table-header-group", TableHeaderGroup )
        , ( "table-footer-group", TableFooterGroup )
        , ( "table-row", TableRow )
        , ( "table-cell", TableCell )
        , ( "table-column-group", TableColumnGroup )
        , ( "table-column", TableColumn )
        , ( "table-caption", TableCaption )
        , ( "ruby-base", RubyBase )
        , ( "ruby-text", RubyText )
        , ( "ruby-base-container", RubyBaseContainer )
        , ( "ruby-text-container", RubyTextContainer )
        ]


{-| The [`<display-box>`](https://drafts.csswg.org/css-display-3/#typedef-display-box) data type.

Value definition syntax:

    contents | none

-}
type DisplayBox
    = Contents
    | None


displayBox : Parser DisplayBox
displayBox =
    keywordsToType
        [ ( "contents", Contents )
        , ( "none", None )
        ]


{-| The [`<display-legacy>`](https://drafts.csswg.org/css-display-3/#typedef-display-legacy) data type.

Value definition syntax:

    inline-block | inline-table | inline-flex | inline-grid

-}
type DisplayLegacy
    = InlineBlock
    | InlineTable
    | InlineFlex
    | InlineGrid


displayLegacy : Parser DisplayLegacy
displayLegacy =
    keywordsToType
        [ ( "inline-block", InlineBlock )
        , ( "inline-table", InlineTable )
        , ( "inline-flex", InlineFlex )
        , ( "inline-grid", InlineGrid )
        ]
