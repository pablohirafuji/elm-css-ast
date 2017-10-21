module Main exposing (..)

import Html exposing (Html, div, text, p, textarea, pre, code, option, select, label, ul, li, input, button)
import Html.Attributes exposing (defaultValue, id, class, value, spellcheck, selected, style, type_, placeholder, checked, classList)
import Html.Events exposing (onClick, onInput, onCheck)
import CssAst exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL --


type alias Model =
    { textarea : String
    }


initModel : Model
initModel =
    { textarea = ""
    }



-- UPDATE --


type Msg
    = InputTextarea String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTextarea n ->
            { model | textarea = n } ! []



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ onInput InputTextarea ]
            []
        , CssAst.run model.textarea
            |> toString
            |> Html.text
        ]
