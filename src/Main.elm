module Main exposing (..)

import Browser
import Html exposing (Html, span, div, text, input, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)




main = Browser.sandbox { init = init, update = update, view = view }




type Expr = Value Int
          | Name String
          | Abstraction String Expr
          | Application Expr Expr




type alias Model = Expr


init : Model
init = Application (Abstraction ("x" Name "x") Value 2)


type Msg 
  = TextUpdate String


update: Msg -> Model -> Model
update msg model =
  case msg of
    TextUpdate s -> s


view : Model -> Html Msg
view model =
  div [] 
  [ button [onInput TextUpdate] [ text "Evaluate"]
  , span [] [text (printExpr model.expr)]
  ]


printExpr: Expr -> String
printExpr expr =
  "This is the expression"