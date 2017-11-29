module CssAst.Content
    exposing
        ( Content(..)
        , declarations
        , ContentListItem(..)
        , QuoteValue(..)
        , BookmarkLevel(..)
        , BookmarkState(..)
        , ContentValue(..)
        , Quotes(..)
        , StringSet(..)
        )

{-| [CSS Generated Content](https://drafts.csswg.org/css-content-3/)

@docs Content, ContentListItem, QuoteValue, BookmarkLevel, BookmarkState, ContentValue, Quotes, StringSet, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Values as V
import CssAst.Images as Image
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


{-| The Content properties type.

[Source](https://drafts.csswg.org/css-content-3/#property-index).

-}
type Content
    = BookmarkLabel (List ContentListItem)
    | BookmarkLevel BookmarkLevel
    | BookmarkState BookmarkState
    | Content ContentValue
    | Quotes Quotes
    | StringSet StringSet


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Content )
declarations =
    [ ( "bookmark-label", contentList |> map BookmarkLabel )
    , ( "bookmark-level", bookmarkLevel |> map BookmarkLevel )
    , ( "bookmark-state", bookmarkState |> map BookmarkState )
    , ( "content", contentValue |> map Content )
    , ( "quotes", quotes |> map Quotes )
    , ( "string-set", stringSet |> map StringSet )
    ]


{-| The [`ContentListItem`](https://drafts.csswg.org/css-content-3/#typedef-content-content-list) helper type.

Value definition syntax:

    <string> | contents | <image> | <quote> | <target> | <leader()>

-}
type ContentListItem
    = String String
    | Contents
    | Image Image.Value
    | Quote QuoteValue


contentListItem : Parser ContentListItem
contentListItem =
    oneOf
        [ V.string |> map String
        , keyword "contents" |> map (always Contents)
        , Image.value |> map Image
        , quoteValue |> map Quote
        ]


contentList : Parser (List ContentListItem)
contentList =
    repeat oneOrMore (contentListItem |. whitespace)


{-| The [`<quote>`](https://drafts.csswg.org/css-content-3/#typedef-quote) data type.

Value definition syntax:

    open-quote | close-quote | no-open-quote | no-close-quote

-}
type QuoteValue
    = OpenQuote
    | CloseQuote
    | NoOpenQuote
    | NoCloseQuote


quoteValue : Parser QuoteValue
quoteValue =
    keywordsToType
        [ ( "open-quote", OpenQuote )
        , ( "close-quote", CloseQuote )
        , ( "no-open-quote", NoOpenQuote )
        , ( "no-close-quote", NoCloseQuote )
        ]


{-| The [`bookmark-level`](https://drafts.csswg.org/css-content-3/#propdef-bookmark-level) property type.

Value definition syntax:

    none | <integer>

-}
type BookmarkLevel
    = NoLevel
    | Integer Int


bookmarkLevel : Parser BookmarkLevel
bookmarkLevel =
    oneOf
        [ keyword "none" |> map (always NoLevel)
        , V.integer |> map Integer
        ]


{-| The [`bookmark-state`](https://drafts.csswg.org/css-content-3/#propdef-bookmark-state) property type.

Value definition syntax:

    open | closed

-}
type BookmarkState
    = Open
    | Closed


bookmarkState : Parser BookmarkState
bookmarkState =
    keywordsToType
        [ ( "open", Open )
        , ( "closed", Closed )
        ]


{-| The [`content`](https://drafts.csswg.org/css-content-3/#propdef-content) property type.

Value definition syntax:

    normal | none | [ <content-replacement> | <content-list> ] [/ <string> ]?

-}
type ContentValue
    = Normal
    | NoContent
    | ContentList (List ContentListItem) (Maybe String)


contentValue : Parser ContentValue
contentValue =
    oneOf
        [ keyword "normal" |> map (always Normal)
        , keyword "none" |> map (always NoContent)
        , succeed ContentList
            |= contentList
            |. symbol "/"
            |. whitespace
            |= toMaybe V.string
        ]


{-| The [`quotes`](https://drafts.csswg.org/css-content-3/#propdef-quotes) property type.

Value definition syntax:

    none | [ <string> <string> ]+

-}
type Quotes
    = NoQuotes
    | QuotesList (List ( String, String ))


quotes : Parser Quotes
quotes =
    oneOf
        [ keyword "none" |> map (always NoQuotes)
        , repeat oneOrMore
            (succeed (,)
                |= V.string
                |. whitespace
                |= V.string
                |. whitespace
            )
            |> map QuotesList
        ]


{-| The [`string-set`](https://drafts.csswg.org/css-content-3/#propdef-string-set) property type.

Value definition syntax:

    none | [ <custom-ident> <string>+ ]#

-}
type StringSet
    = NoSet
    | Set (List ( String, List String ))


stringSet : Parser StringSet
stringSet =
    oneOf
        [ keyword "none" |> map (always NoSet)
        , oneOrMoreCommaList
            (succeed (,)
                |= V.identifier
                |. whitespace
                |= (repeat oneOrMore (V.string |. whitespace))
            )
            |> map Set
        ]
