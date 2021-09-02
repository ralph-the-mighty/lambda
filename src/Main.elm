module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import String.Interpolate exposing(interpolate)
import LambdaCalc exposing (..)


type alias Model =
    { expr : Expr }


initialModel : Model
initialModel =
    { expr = App (store "Y") (Var "func") }


type Msg
    = Apply


update : Msg -> Model -> Model
update msg model =
    case msg of
        Apply ->
            { model | expr = (apply model.expr) }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Apply ] [ text "reduce" ]
        , div [] [ text <| (printExpr model.expr) ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }