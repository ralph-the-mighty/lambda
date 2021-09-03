module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String.Interpolate exposing(interpolate)
import LambdaCalc exposing (..)
import LambdaCalc exposing (outermostfirst)


type alias Model =
    { expr : Expr
    , text : String
    }


initialModel : Model
initialModel =
    { expr = App (store "Y") (Var "func")
    , text = ""
    }


type Msg
    = ReduceOld
    | ReduceOutermost
    | TextUpdate String
    | Parse


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
        TextUpdate s
          -> {model | text = s}
        Parse
          -> {model | expr = parse model.text }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ReduceOld ] [ text "reduce (old)" ]
        , button [ onClick ReduceOutermost ] [ text "reduce (outermost first)" ]
        , div [] [ text <| (printExpr model.expr) ]
        , div [] [ text <| (printTypes model.expr) ]
        , input [ placeholder "Type Lambda Expression here", value model.text, onInput TextUpdate ] []
        , button [ onClick Parse] [text "parse"]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }