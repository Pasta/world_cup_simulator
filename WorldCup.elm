module WorldCup exposing (..)

import Dom
import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- import Html.Keyed as Keyed
-- import Html.Lazy exposing (lazy, lazy2)

main : Program Never Model Msg
main =
  program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

type alias Model =
  { teams: List Team }
  
type alias Team =
  String

type Msg
  = NoOp

init : ( Model, Cmd Msg )
init =
    ( { teams = ["Hello World Cup"] }, Cmd.none )
    
    
view : Model -> Html Msg
view model =
  div
    "Hello World Cup"
  
  
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
              
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
  
  
