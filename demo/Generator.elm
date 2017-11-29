module Generator exposing (..)

import Html exposing (Html, div, text, p, textarea, pre, code, option, select, label, ul, li, input, button)
import Html.Events exposing (onClick, onInput, onCheck)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed, delayedCommit, fail, map2)
import CssAst.Values as V
import CssAst.Helpers exposing (whitespace, keywordsToType, oneOrMoreCommaList, toMaybe)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Type
    = Property String
    | Data String


onlyKeywords : Parser ( Type, List String )
onlyKeywords =
    succeed (,)
        |= oneOf
            [ succeed Data
                |. symbol "<"
                |= keep oneOrMore ((/=) '>')
                |. symbol ">"
            , V.identifier
                |> map Property
            ]
        |. whitespace
        |. symbol "="
        |. whitespace
        |= repeat oneOrMore
            (succeed identity
                |= V.identifier
                |. whitespace
                |. oneOf [ symbol "|", succeed () ]
                |. whitespace
            )



-- MODEL


type alias Model =
    { textarea : String
    }


initModel : Model
initModel =
    { textarea = ""
    }



-- UPDATE


type Msg
    = Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input str ->
            ( { model | textarea = str }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput Input ] []
        , pre []
            [ code []
                [ Parser.run onlyKeywords model.textarea
                    |> Result.map onlyKeywordsString
                    |> Result.withDefault "Error"
                    |> text
                ]
            ]
        ]


toUpperCamelCase : String -> String
toUpperCamelCase str =
    String.split "-" str
        |> List.map (\n -> String.toUpper (String.left 1 n) ++ String.dropLeft 1 n)
        |> String.concat


toLowerCamelCase : String -> String
toLowerCamelCase str =
    toUpperCamelCase str
        |> (\n -> String.toLower (String.left 1 n) ++ String.dropLeft 1 n)


onlyKeywordsString : ( Type, List String ) -> String
onlyKeywordsString ( type_, strs ) =
    let
        ( identifier, name, typeStr ) =
            case type_ of
                Property name ->
                    ( name, name, "property" )

                Data name ->
                    ( name, "<" ++ name ++ ">", "data" )
    in
        "{-| The [`"
            ++ name
            ++ "`]() "
            ++ typeStr
            ++ " type.\n\n"
            ++ "Value definition syntax:\n\n    "
            ++ String.join " | " strs
            ++ "\n\n-}\ntype "
            ++ toUpperCamelCase identifier
            ++ "\n    = "
            ++ String.join "\n    | " (List.map toUpperCamelCase strs)
            ++ "\n\n\n"
            ++ toLowerCamelCase identifier
            ++ " : Parser "
            ++ toUpperCamelCase identifier
            ++ "\n"
            ++ toLowerCamelCase identifier
            ++ " =\n    keywordsToType\n        [ "
            ++ String.join "\n        , " (List.map (\n -> "( \"" ++ n ++ "\", " ++ toUpperCamelCase n ++ " )") strs)
            ++ "\n        ]"
