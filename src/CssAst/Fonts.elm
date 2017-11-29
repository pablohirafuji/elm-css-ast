module CssAst.Fonts
    exposing
        ( Declaration(..)
        , declarations
        , FontFamily(..)
        , GenericFamily(..)
        , FontWeight(..)
        , FontStretch(..)
        , FontStyle(..)
        , FontSize(..)
        , FontSizeAdjust(..)
        , FontSynthesis(..)
        , FontKerning(..)
        , FontVariantLigatures(..)
        , FontVariantPosition(..)
        , FontVariantAlternates(..)
        , FontVariantEastAsian(..)
        , FontVariant(..)
        , FontFeatureSettings(..)
        , FontLanguageOverride(..)
        )

{-| [CSS Fonts](https://drafts.csswg.org/css-fonts-3/)

@docs Declaration, FontFamily, GenericFamily, FontWeight, FontStretch, FontStyle, FontSize, FontSizeAdjust, FontSynthesis, FontKerning, FontVariantLigatures, FontVariantPosition, FontVariantNumeric, FontVariantAlternates, FontVariantEastAsian, FontVariant, FontFeatureSettings, FontLanguageOverride, declarations

-}

import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Values as V
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe, function)


{-| The Fonts [declarations](https://drafts.csswg.org/css-fonts-3/#property-index) type.
-}
type Declaration
    = FontFamily (List FontFamily)
    | FontWeight FontWeight
    | FontStretch FontStretch
    | FontStyle FontStyle
    | FontSize FontSize
    | FontSizeAdjust FontSizeAdjust
    | FontSynthesis FontSynthesis
    | FontKerning FontKerning
    | FontVariantLigatures FontVariantLigatures
    | FontVariantPosition FontVariantPosition
    | FontVariantNumeric FontVariantNumeric
    | FontVariantAlternates FontVariantAlternates
    | FontVariantEastAsian FontVariantEastAsian
    | FontVariant FontVariant
    | FontFeatureSettings FontFeatureSettings
    | FontLanguageOverride FontLanguageOverride


{-| Exposing for internal use only.
-}
declarations : List ( String, Parser Declaration )
declarations =
    [ ( "font-family"
      , oneOrMoreCommaList fontFamily |> map FontFamily
      )
    , ( "font-weight", fontWeight |> map FontWeight )
    , ( "font-stretch", fontStretch |> map FontStretch )
    , ( "font-style", fontStyle |> map FontStyle )
    , ( "font-size", fontSize |> map FontSize )
    , ( "font-size-adjust", fontSizeAdjust |> map FontSizeAdjust )
    , ( "font-synthesis", fontSynthesis |> map FontSynthesis )
    , ( "font-kerning", fontKerning |> map FontKerning )
    , ( "font-variant-ligatures", fontVariantLigatures |> map FontVariantLigatures )
    , ( "font-variant-position", fontVariantPosition |> map FontVariantPosition )
    , ( "font-variant-numeric", fontVariantNumeric |> map FontVariantNumeric )
    , ( "font-variant-alternates", fontVariantAlternates |> map FontVariantAlternates )
    , ( "font-variant-east-asian", fontVariantEastAsian |> map FontVariantEastAsian )
    , ( "font-variant", fontVariant |> map FontVariant )
    , ( "font-feature-settings", fontFeatureSettings |> map FontFeatureSettings )
    , ( "font-language-override", fontLanguageOverride |> map FontLanguageOverride )
    ]


{-| The [`font-family`](https://drafts.csswg.org/css-fonts-3/#propdef-font-family) property type.

Value definition syntax:

    <family-name> | <generic-family>

-}
type FontFamily
    = GenericFamily GenericFamily
    | FamilyName String


fontFamily : Parser FontFamily
fontFamily =
    oneOf
        [ genericFamily |> map GenericFamily
        , V.string |> map FamilyName
        , V.identifier |> map FamilyName
        ]


{-| The [`<generic-family>`](https://drafts.csswg.org/css-fonts-3/#typedef-generic-family) data type.

Value definition syntax:

    serif | sans-serif | cursive | fantasy | monospace

-}
type GenericFamily
    = Serif
    | SansSerif
    | Cursive
    | Fantasy
    | Monospace


genericFamily : Parser GenericFamily
genericFamily =
    keywordsToType
        [ ( "serif", Serif )
        , ( "sans-serif", SansSerif )
        , ( "cursive", Cursive )
        , ( "fantasy", Fantasy )
        , ( "monospace", Monospace )
        ]


{-| The [`font-weight`](https://drafts.csswg.org/css-fonts-3/#propdef-font-weight) property type.

Value definition syntax:

    normal | bold | bolder | lighter | 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900

-}
type FontWeight
    = NormalFontWeight
    | Bold
    | Bolder
    | Lighter
    | N100
    | N200
    | N300
    | N400
    | N500
    | N600
    | N700
    | N800
    | N900


fontWeight : Parser FontWeight
fontWeight =
    keywordsToType
        [ ( "normal", NormalFontWeight )
        , ( "bold", Bold )
        , ( "bolder", Bolder )
        , ( "lighter", Lighter )
        , ( "100", N100 )
        , ( "200", N200 )
        , ( "300", N300 )
        , ( "400", N400 )
        , ( "500", N500 )
        , ( "600", N600 )
        , ( "700", N700 )
        , ( "800", N800 )
        , ( "900", N900 )
        ]


{-| The [`font-stretch`](https://drafts.csswg.org/css-fonts-3/#propdef-font-stretch) property type.

Value definition syntax:

    normal | ultra-condensed | extra-condensed | condensed | semi-condensed | semi-expanded | expanded | extra-expanded | ultra-expanded

-}
type FontStretch
    = NormalFontStretch
    | UltraCondensed
    | ExtraCondensed
    | Condensed
    | SemiCondensed
    | SemiExpanded
    | Expanded
    | ExtraExpanded
    | UltraExpanded


fontStretch : Parser FontStretch
fontStretch =
    keywordsToType
        [ ( "normal", NormalFontStretch )
        , ( "ultra-condensed", UltraCondensed )
        , ( "extra-condensed", ExtraCondensed )
        , ( "condensed", Condensed )
        , ( "semi-condensed", SemiCondensed )
        , ( "semi-expanded", SemiExpanded )
        , ( "expanded", Expanded )
        , ( "extra-expanded", ExtraExpanded )
        , ( "ultra-expanded", UltraExpanded )
        ]


{-| The [`font-style`](https://drafts.csswg.org/css-fonts-3/#propdef-font-style) property type.

Value definition syntax:

    normal | italic | oblique

-}
type FontStyle
    = NormalFontStyle
    | Italic
    | Oblique


fontStyle : Parser FontStyle
fontStyle =
    keywordsToType
        [ ( "normal", NormalFontStyle )
        , ( "italic", Italic )
        , ( "oblique", Oblique )
        ]


{-| The [`font-size`](https://drafts.csswg.org/css-fonts-3/#propdef-font-size) property type.

Value definition syntax:

    xx-small | x-small | small | medium | large | x-large | xx-large | larger | smaller | <length> | <percentage>

-}
type FontSize
    = XXSmall
    | XSmall
    | Small
    | Medium
    | Large
    | XLarge
    | XXLarge
    | Larger
    | Smaller
    | Length V.Length
    | Percentage V.Percentage


fontSize : Parser FontSize
fontSize =
    oneOf
        [ keywordsToType
            [ ( "xx-small", XXSmall )
            , ( "x-small", XSmall )
            , ( "small", Small )
            , ( "medium", Medium )
            , ( "large", Large )
            , ( "x-large", XLarge )
            , ( "xx-large", XXLarge )
            , ( "larger", Larger )
            , ( "smaller", Smaller )
            ]
        , V.length |> map Length
        , V.percentage |> map Percentage
        ]


{-| The [`font-size-adjust`](https://drafts.csswg.org/css-fonts-3/#propdef-font-size-adjust) property type.

Value definition syntax:

    none | <number>

-}
type FontSizeAdjust
    = NoFontSizeAdjust
    | Number Float


fontSizeAdjust : Parser FontSizeAdjust
fontSizeAdjust =
    oneOf
        [ keyword "none" |> map (always NoFontSizeAdjust)
        , V.number |> map Number
        ]


{-| The [`font`](https://drafts.csswg.org/css-fonts-3/#propdef-font) property type.

Value definition syntax:

    [ [ <‘font-style’> || <font-variant-css21> || <‘font-weight’> || <‘font-stretch’> ]? <‘font-size’> [ / <‘line-height’> ]? <‘font-family’> ] | caption | icon | menu | message-box | small-caption | status-bar

TODO: Add line-height

-}
type Font
    = CustomFont FontOptions FontSize (List FontFamily)
    | Caption
    | Icon
    | Menu
    | MessageBox
    | SmallCaption
    | StatusBar


font : Parser Font
font =
    oneOf
        [ keywordsToType
            [ ( "caption", Caption )
            , ( "icon", Icon )
            , ( "menu", Menu )
            , ( "message-box", MessageBox )
            , ( "small-caption", SmallCaption )
            , ( "status-bar", StatusBar )
            ]
        , succeed CustomFont
            |= fontOptions
            |. whitespace
            |= fontSize
            --|. whitespace
            --|= toMaybe
            --    (succeed identity
            --        |. symbol "/"
            --        |. whitespace
            --    )
            |. whitespace
            |= oneOrMoreCommaList fontFamily
        ]


{-| The `FontOptions` helper type alias.

Value definition syntax:

    <‘font-style’> || <font-variant-css21> || <‘font-weight’> || <‘font-stretch’>

-}
type alias FontOptions =
    { style : Maybe FontStyle
    , variantCss21 : Maybe FontVariantCss21
    , weight : Maybe FontWeight
    , stretch : Maybe FontStretch
    }


emptyFontOptions : FontOptions
emptyFontOptions =
    { style = Nothing
    , variantCss21 = Nothing
    , weight = Nothing
    , stretch = Nothing
    }


fontOptions : Parser FontOptions
fontOptions =
    fontOptionsHelp [] fontOptionsParsers emptyFontOptions


fontOptionsHelp : List (FontOptions -> Parser FontOptions) -> List (FontOptions -> Parser FontOptions) -> FontOptions -> Parser FontOptions
fontOptionsHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (fontOptionsHelp [] (nsHead ++ tail))
                , fontOptionsHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            succeed n


fontOptionsParsers : List (FontOptions -> Parser FontOptions)
fontOptionsParsers =
    [ \r -> map (\n -> { r | style = Just n }) fontStyle
    , \r -> map (\n -> { r | variantCss21 = Just n }) fontVariantCss21
    , \r -> map (\n -> { r | weight = Just n }) fontWeight
    , \r -> map (\n -> { r | stretch = Just n }) fontStretch
    ]


{-| The [`<font-variant-css21>`](https://drafts.csswg.org/css-fonts-3/#typedef-font-variant-css21) data type.

Value definition syntax:

    normal | small-caps

-}
type FontVariantCss21
    = FVC21Normal
    | FVC21SmallCaps


fontVariantCss21 : Parser FontVariantCss21
fontVariantCss21 =
    keywordsToType
        [ ( "normal", FVC21Normal )
        , ( "small-caps", FVC21SmallCaps )
        ]


{-| The [`font-synthesis`](https://drafts.csswg.org/css-fonts-3/#propdef-font-synthesis) property type.

Value definition syntax:

    none | [ weight || style ]

-}
type FontSynthesis
    = NoFontSynthesis
    | OnlyWeight
    | OnlyStyle
    | WeightAndStyle


fontSynthesis : Parser FontSynthesis
fontSynthesis =
    oneOf
        [ keyword "none" |> map (always NoFontSynthesis)
        , succeed
            (\n ->
                case n of
                    Just _ ->
                        WeightAndStyle

                    Nothing ->
                        OnlyWeight
            )
            |. keyword "weight"
            |. whitespace
            |= toMaybe (keyword "style")
        , succeed
            (\n ->
                case n of
                    Just _ ->
                        WeightAndStyle

                    Nothing ->
                        OnlyStyle
            )
            |. keyword "style"
            |. whitespace
            |= toMaybe (keyword "weight")
        ]


{-| The [`font-kerning`](https://drafts.csswg.org/css-fonts-3/#propdef-font-kerning) property type.

Value definition syntax:

    auto | normal | none

-}
type FontKerning
    = AutoKerning
    | NormalKerning
    | NoKerning


fontKerning : Parser FontKerning
fontKerning =
    keywordsToType
        [ ( "auto", AutoKerning )
        , ( "normal", NormalKerning )
        , ( "none", NoKerning )
        ]


{-| The [`font-variant-ligatures`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant-ligatures) property type.

Value definition syntax:

    normal | none | <common-lig-values> | <discretionary-lig-values> | <historical-lig-values> | <contextual-alt-values>

-}
type FontVariantLigatures
    = FVLNormal
    | FVLNone
    | FVLCommonLigValues CommonLigValues
    | FVLDiscretionaryLigValues DiscretionaryLigValues
    | FVLHistoricalLigValues HistoricalLigValues
    | FVLContextualAltValues ContextualAltValues


fontVariantLigatures : Parser FontVariantLigatures
fontVariantLigatures =
    oneOf
        [ keyword "normal" |> map (always FVLNormal)
        , keyword "none" |> map (always FVLNone)
        , commonLigValues |> map FVLCommonLigValues
        , discretionaryLigValues |> map FVLDiscretionaryLigValues
        , historicalLigValues |> map FVLHistoricalLigValues
        , contextualAltValues |> map FVLContextualAltValues
        ]


{-| The [`<common-lig-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-common-lig-values) data type.

Value definition syntax:

    common-ligatures | no-common-ligatures

-}
type CommonLigValues
    = CommonLigatures
    | NoCommonLigatures


commonLigValues : Parser CommonLigValues
commonLigValues =
    keywordsToType
        [ ( "common-ligatures", CommonLigatures )
        , ( "no-common-ligatures", NoCommonLigatures )
        ]


{-| The [`<discretionary-lig-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-discretionary-lig-values) data type.

Value definition syntax:

    discretionary-ligatures | no-discretionary-ligatures

-}
type DiscretionaryLigValues
    = DiscretionaryLigatures
    | NoDiscretionaryLigatures


discretionaryLigValues : Parser DiscretionaryLigValues
discretionaryLigValues =
    keywordsToType
        [ ( "discretionary-ligatures", DiscretionaryLigatures )
        , ( "no-discretionary-ligatures", NoDiscretionaryLigatures )
        ]


{-| The [`<historical-lig-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-historical-lig-values) data type.

Value definition syntax:

    historical-ligatures | no-historical-ligatures

-}
type HistoricalLigValues
    = HistoricalLigatures
    | NoHistoricalLigatures


historicalLigValues : Parser HistoricalLigValues
historicalLigValues =
    keywordsToType
        [ ( "historical-ligatures", HistoricalLigatures )
        , ( "no-historical-ligatures", NoHistoricalLigatures )
        ]


{-| The [`<contextual-alt-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-contextual-alt-values) data type.

Value definition syntax:

    contextual | no-contextual

-}
type ContextualAltValues
    = Contextual
    | NoContextual


contextualAltValues : Parser ContextualAltValues
contextualAltValues =
    keywordsToType
        [ ( "contextual", Contextual )
        , ( "no-contextual", NoContextual )
        ]


{-| The [`font-variant-position`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant-position) property type.

Value definition syntax:

    normal | sub | super

-}
type FontVariantPosition
    = NormalVariantPosition
    | Sub
    | Super


fontVariantPosition : Parser FontVariantPosition
fontVariantPosition =
    keywordsToType
        [ ( "normal", NormalVariantPosition )
        , ( "sub", Sub )
        , ( "super", Super )
        ]


{-| The [`font-variant-caps`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant-caps) property type.

Value definition syntax:

    normal | small-caps | all-small-caps | petite-caps | all-petite-caps | unicase | titling-caps

-}
type FontVariantCaps
    = FVCNormal
    | FVCCustom VariantCaps


fontVariantCaps : Parser FontVariantCaps
fontVariantCaps =
    oneOf
        [ keyword "normal" |> map (always FVCNormal)
        , variantCaps |> map FVCCustom
        ]


{-| The [`font-variant-numeric`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant-numeric) property type.

Value definition syntax:

    normal | [ <numeric-figure-values> || <numeric-spacing-values> || <numeric-fraction-values> || ordinal || slashed-zero ]

-}
type FontVariantNumeric
    = NormalVariantNumeric
    | NumericFigure NumericFigureValues
    | NumericSpacing NumericSpacingValues
    | NumericFraction NumericFractionValues
    | Ordinal
    | SlashedZero


fontVariantNumeric : Parser FontVariantNumeric
fontVariantNumeric =
    oneOf
        [ keyword "normal" |> map (always NormalVariantNumeric)
        , numericFigureValues |> map NumericFigure
        , numericSpacingValues |> map NumericSpacing
        , numericFractionValues |> map NumericFraction
        , keyword "ordinal" |> map (always Ordinal)
        , keyword "slashed-zero" |> map (always SlashedZero)
        ]


{-| The [`<numeric-figure-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-numeric-figure-values) data type.

Value definition syntax:

    lining-nums | oldstyle-nums

-}
type NumericFigureValues
    = LiningNums
    | OldstyleNums


numericFigureValues : Parser NumericFigureValues
numericFigureValues =
    keywordsToType
        [ ( "lining-nums", LiningNums )
        , ( "oldstyle-nums", OldstyleNums )
        ]


{-| The [`<numeric-spacing-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-numeric-spacing-values) data type.

Value definition syntax:

    proportional-nums | tabular-nums

-}
type NumericSpacingValues
    = ProportionalNums
    | TabularNums


numericSpacingValues : Parser NumericSpacingValues
numericSpacingValues =
    keywordsToType
        [ ( "proportional-nums", ProportionalNums )
        , ( "tabular-nums", TabularNums )
        ]


{-| The [`<numeric-fraction-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-numeric-fraction-values) data type.

Value definition syntax:

    diagonal-fractions | stacked-fractions

-}
type NumericFractionValues
    = DiagonalFractions
    | StackedFractions


numericFractionValues : Parser NumericFractionValues
numericFractionValues =
    keywordsToType
        [ ( "diagonal-fractions", DiagonalFractions )
        , ( "stacked-fractions", StackedFractions )
        ]


{-| The [`font-variant-alternates`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant-alternates) property type.

Value definition syntax:

    normal | [ stylistic(<feature-value-name>) || historical-forms || styleset(<feature-value-name>#) || character-variant(<feature-value-name>#) || swash(<feature-value-name>) || ornaments(<feature-value-name>) || annotation(<feature-value-name>) ]

-}
type FontVariantAlternates
    = FVANormal
    | FVACustom CustomFVA


fontVariantAlternates : Parser FontVariantAlternates
fontVariantAlternates =
    oneOf
        [ keyword "normal" |> map (always FVANormal)
        , customFVA |> map FVACustom
        ]


type alias CustomFVA =
    { stylistic : Maybe String
    , historicalForms : Bool
    , styleset : List String
    , characterVariant : List String
    , swash : Maybe String
    , ornaments : Maybe String
    , annotation : Maybe String
    }


emptyCustomFVA : CustomFVA
emptyCustomFVA =
    { stylistic = Nothing
    , historicalForms = False
    , styleset = []
    , characterVariant = []
    , swash = Nothing
    , ornaments = Nothing
    , annotation = Nothing
    }


customFVA : Parser CustomFVA
customFVA =
    customFVAHelp [] customFVAParsers emptyCustomFVA


customFVAHelp : List (CustomFVA -> Parser CustomFVA) -> List (CustomFVA -> Parser CustomFVA) -> CustomFVA -> Parser CustomFVA
customFVAHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (customFVAHelp [] (nsHead ++ tail))
                , customFVAHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n == emptyCustomFVA then
                fail "Empty `font-variant-alternates` property value."
            else
                succeed n


customFVAParsers : List (CustomFVA -> Parser CustomFVA)
customFVAParsers =
    [ \r ->
        function "stylistic" V.identifier
            |> map (\n -> { r | stylistic = Just n })
    , \r ->
        keyword "historical-forms"
            |> map (\n -> { r | historicalForms = True })
    , \r ->
        function "styleset" (oneOrMoreCommaList V.identifier)
            |> map (\n -> { r | styleset = n })
    , \r ->
        function "character-variant" (oneOrMoreCommaList V.identifier)
            |> map (\n -> { r | characterVariant = n })
    , \r ->
        function "swash" V.identifier
            |> map (\n -> { r | swash = Just n })
    , \r ->
        function "ornaments" V.identifier
            |> map (\n -> { r | ornaments = Just n })
    , \r ->
        function "annotation" V.identifier
            |> map (\n -> { r | annotation = Just n })
    ]


{-| The [`font-variant-east-asian`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant-east-asian) property type.

Value definition syntax:

    normal | [ <east-asian-variant-values> || <east-asian-width-values> || ruby ]

-}
type FontVariantEastAsian
    = FVEANormal
    | FVEACustom CustomFVEA


fontVariantEastAsian : Parser FontVariantEastAsian
fontVariantEastAsian =
    oneOf
        [ keyword "normal" |> map (always FVEANormal)
        , customFVEA |> map FVEACustom
        ]


type alias CustomFVEA =
    { eastAsianVariantValues : Maybe EastAsianVariantValues
    , eastAsianWidthValues : Maybe EastAsianWidthValues
    , ruby : Bool
    }


emptyCustomFVEA : CustomFVEA
emptyCustomFVEA =
    { eastAsianVariantValues = Nothing
    , eastAsianWidthValues = Nothing
    , ruby = False
    }


customFVEA : Parser CustomFVEA
customFVEA =
    customFVEAHelp [] customFVEAParsers emptyCustomFVEA


customFVEAHelp : List (CustomFVEA -> Parser CustomFVEA) -> List (CustomFVEA -> Parser CustomFVEA) -> CustomFVEA -> Parser CustomFVEA
customFVEAHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (customFVEAHelp [] (nsHead ++ tail))
                , customFVEAHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n == emptyCustomFVEA then
                fail "Empty `font-variant-east-asian` property value."
            else
                succeed n


customFVEAParsers : List (CustomFVEA -> Parser CustomFVEA)
customFVEAParsers =
    [ \r -> map (\n -> { r | eastAsianVariantValues = Just n }) eastAsianVariantValues
    , \r -> map (\n -> { r | eastAsianWidthValues = Just n }) eastAsianWidthValues
    , \r -> map (\n -> { r | ruby = True }) (keyword "ruby")
    ]


{-| The [`<east-asian-variant-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-east-asian-variant-values) data type.

Value definition syntax:

    jis78 | jis83 | jis90 | jis04 | simplified | traditional

-}
type EastAsianVariantValues
    = Jis78
    | Jis83
    | Jis90
    | Jis04
    | Simplified
    | Traditional


eastAsianVariantValues : Parser EastAsianVariantValues
eastAsianVariantValues =
    keywordsToType
        [ ( "jis78", Jis78 )
        , ( "jis83", Jis83 )
        , ( "jis90", Jis90 )
        , ( "jis04", Jis04 )
        , ( "simplified", Simplified )
        , ( "traditional", Traditional )
        ]


{-| The [`<east-asian-width-values>`](https://drafts.csswg.org/css-fonts-3/#typedef-east-asian-width-values) data type.

Value definition syntax:

    full-width | proportional-width

-}
type EastAsianWidthValues
    = FullWidth
    | ProportionalWidth


eastAsianWidthValues : Parser EastAsianWidthValues
eastAsianWidthValues =
    keywordsToType
        [ ( "full-width", FullWidth )
        , ( "proportional-width", ProportionalWidth )
        ]


{-| The [`font-variant`](https://drafts.csswg.org/css-fonts-3/#propdef-font-variant) property type.

Value definition syntax:

    normal | none | [ <common-lig-values> || <discretionary-lig-values> || <historical-lig-values> || <contextual-alt-values> || stylistic(<feature-value-name>) || historical-forms || styleset(<feature-value-name>#) || character-variant(<feature-value-name>#) || swash(<feature-value-name>) || ornaments(<feature-value-name>) || annotation(<feature-value-name>) || [ small-caps | all-small-caps | petite-caps | all-petite-caps | unicase | titling-caps ] || <numeric-figure-values> || <numeric-spacing-values> || <numeric-fraction-values> || ordinal || slashed-zero || <east-asian-variant-values> || <east-asian-width-values> || ruby ]

-}
type FontVariant
    = FVNormal
    | FVNone
    | FVCustom CustomFontVariant


fontVariant : Parser FontVariant
fontVariant =
    oneOf
        [ keyword "normal" |> map (always FVNormal)
        , keyword "none" |> map (always FVNone)
        , customFontVariant |> map FVCustom
        ]


{-| The `CustomFontVariant` helper type alias.

Value definition syntax:

    <common-lig-values> || <discretionary-lig-values> || <historical-lig-values> || <contextual-alt-values> || stylistic(<feature-value-name>) || historical-forms || styleset(<feature-value-name>#) || character-variant(<feature-value-name>#) || swash(<feature-value-name>) || ornaments(<feature-value-name>) || annotation(<feature-value-name>) || [ small-caps | all-small-caps | petite-caps | all-petite-caps | unicase | titling-caps ] || <numeric-figure-values> || <numeric-spacing-values> || <numeric-fraction-values> || ordinal || slashed-zero || <east-asian-variant-values> || <east-asian-width-values> || ruby

-}
type alias CustomFontVariant =
    { commonLigValues : Maybe CommonLigValues
    , discretionaryLigValues : Maybe DiscretionaryLigValues
    , historicalLigValues : Maybe HistoricalLigValues
    , contextualAltValues : Maybe ContextualAltValues
    , stylistic : Maybe String
    , historicalForms : Bool
    , styleset : List String
    , characterVariant : List String
    , swash : Maybe String
    , ornaments : Maybe String
    , annotation : Maybe String
    , variantCaps : Maybe VariantCaps
    , numericFigureValues : Maybe NumericFigureValues
    , numericSpacingValues : Maybe NumericSpacingValues
    , numericFractionValues : Maybe NumericFractionValues
    , ordinal : Bool
    , slashedZero : Bool
    , eastAsianVariantValues : Maybe EastAsianVariantValues
    , eastAsianWidthValues : Maybe EastAsianWidthValues
    , ruby : Bool
    }


emptyCustomFontVariant : CustomFontVariant
emptyCustomFontVariant =
    { commonLigValues = Nothing
    , discretionaryLigValues = Nothing
    , historicalLigValues = Nothing
    , contextualAltValues = Nothing
    , stylistic = Nothing
    , historicalForms = False
    , styleset = []
    , characterVariant = []
    , swash = Nothing
    , ornaments = Nothing
    , annotation = Nothing
    , variantCaps = Nothing
    , numericFigureValues = Nothing
    , numericSpacingValues = Nothing
    , numericFractionValues = Nothing
    , ordinal = False
    , slashedZero = False
    , eastAsianVariantValues = Nothing
    , eastAsianWidthValues = Nothing
    , ruby = False
    }


customFontVariant : Parser CustomFontVariant
customFontVariant =
    customFontVariantHelp [] customFontVariantParsers emptyCustomFontVariant


customFontVariantHelp : List (CustomFontVariant -> Parser CustomFontVariant) -> List (CustomFontVariant -> Parser CustomFontVariant) -> CustomFontVariant -> Parser CustomFontVariant
customFontVariantHelp nsHead nsTail n =
    case nsTail of
        head :: tail ->
            oneOf
                [ succeed identity
                    |= head n
                    |. whitespace
                    |> andThen (customFontVariantHelp [] (nsHead ++ tail))
                , customFontVariantHelp (nsHead ++ [ head ]) tail n
                ]

        [] ->
            if n == emptyCustomFontVariant then
                fail "Empty `font-variant` property value."
            else
                succeed n


customFontVariantParsers : List (CustomFontVariant -> Parser CustomFontVariant)
customFontVariantParsers =
    [ \r -> map (\n -> { r | commonLigValues = Just n }) commonLigValues
    , \r -> map (\n -> { r | discretionaryLigValues = Just n }) discretionaryLigValues
    , \r -> map (\n -> { r | historicalLigValues = Just n }) historicalLigValues
    , \r -> map (\n -> { r | contextualAltValues = Just n }) contextualAltValues
    , \r ->
        function "stylistic" V.identifier
            |> map (\n -> { r | stylistic = Just n })
    , \r ->
        keyword "historical-forms"
            |> map (\n -> { r | historicalForms = True })
    , \r ->
        function "styleset" (oneOrMoreCommaList V.identifier)
            |> map (\n -> { r | styleset = n })
    , \r ->
        function "character-variant" (oneOrMoreCommaList V.identifier)
            |> map (\n -> { r | characterVariant = n })
    , \r ->
        function "swash" V.identifier
            |> map (\n -> { r | swash = Just n })
    , \r ->
        function "ornaments" V.identifier
            |> map (\n -> { r | ornaments = Just n })
    , \r ->
        function "annotation" V.identifier
            |> map (\n -> { r | annotation = Just n })
    , \r -> map (\n -> { r | variantCaps = Just n }) variantCaps
    , \r -> map (\n -> { r | numericFigureValues = Just n }) numericFigureValues
    , \r -> map (\n -> { r | numericSpacingValues = Just n }) numericSpacingValues
    , \r -> map (\n -> { r | numericFractionValues = Just n }) numericFractionValues
    , \r -> map (\n -> { r | ordinal = True }) (keyword "ordinal")
    , \r -> map (\n -> { r | slashedZero = True }) (keyword "slashed-zero")
    , \r -> map (\n -> { r | eastAsianVariantValues = Just n }) eastAsianVariantValues
    , \r -> map (\n -> { r | eastAsianWidthValues = Just n }) eastAsianWidthValues
    , \r -> map (\n -> { r | ruby = True }) (keyword "ruby")
    ]


{-| The `VariantCaps` helper type.

Value definition syntax:

    small-caps | all-small-caps | petite-caps | all-petite-caps | unicase | titling-caps

-}
type VariantCaps
    = SmallCaps
    | AllSmallCaps
    | PetiteCaps
    | AllPetiteCaps
    | Unicase
    | TitlingCaps


variantCaps : Parser VariantCaps
variantCaps =
    keywordsToType
        [ ( "small-caps", SmallCaps )
        , ( "all-small-caps", AllSmallCaps )
        , ( "petite-caps", PetiteCaps )
        , ( "all-petite-caps", AllPetiteCaps )
        , ( "unicase", Unicase )
        , ( "titling-caps", TitlingCaps )
        ]


{-| The [`font-feature-settings`](https://drafts.csswg.org/css-fonts-3/#propdef-font-feature-settings) property type.

Value definition syntax:

    normal | <feature-tag-value>#

-}
type FontFeatureSettings
    = FFSNormal
    | FeatureTag (List ( String, Maybe FeatureTagOption ))


fontFeatureSettings : Parser FontFeatureSettings
fontFeatureSettings =
    oneOf
        [ keyword "normal" |> map (always FFSNormal)
        , oneOrMoreCommaList
            (succeed (,)
                |= V.string
                |. whitespace
                |= toMaybe featureTagOption
            )
            |> map FeatureTag
        ]


{-| The `FeatureTagOption` helper type.

Value definition syntax:

    <integer> | on | off

-}
type FeatureTagOption
    = Integer Int
    | On
    | Off


featureTagOption : Parser FeatureTagOption
featureTagOption =
    oneOf
        [ V.integer |> map Integer
        , keyword "on" |> map (always On)
        , keyword "off" |> map (always Off)
        ]


{-| The [`font-language-override`](https://drafts.csswg.org/css-fonts-3/#propdef-font-language-override) property type.

Value definition syntax:

    normal | <string>

-}
type FontLanguageOverride
    = FLONormal
    | FLOString String


fontLanguageOverride : Parser FontLanguageOverride
fontLanguageOverride =
    oneOf
        [ keyword "normal" |> map (always FLONormal)
        , V.string |> map FLOString
        ]
