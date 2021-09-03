module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import String.Interpolate exposing(interpolate)
import LambdaCalc exposing (..)
import LambdaCalc exposing (outermostfirst)


type alias Model =
    { expr : Expr }


initialModel : Model
initialModel =
    { expr = App (store "Y") (Var "func") }


type Msg
    = ReduceOld
    | ReduceOutermost


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReduceOld ->
          { model | expr = (apply model.expr) }
        ReduceOutermost ->
          let reduction = outermostfirst model.expr in
            case reduction of
              Just newexpr
                -> {model | expr = newexpr}
              Nothing
                -> model


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ReduceOld ] [ text "reduce (old)" ]
        , button [ onClick ReduceOutermost ] [ text "reduce (outermost first)" ]
        , div [] [ text <| (printExpr model.expr) ]
        , div [] [ text <| (printTypes model.expr) ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }