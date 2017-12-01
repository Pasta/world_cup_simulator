module WorldCup exposing (..)

import Html exposing (text)

type alias Model =
  { teams: List Team }
  
type alias Team =
  String

type Msg
  = NoOp

init : ( Model, Cmd Msg )
init =
    ( { teams = ["Hello World Cup"] }, Cmd.none )