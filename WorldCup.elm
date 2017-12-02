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

emptyModel : Model
emptyModel =
    { teams = ["France",
      "Germany",
      "Egypt",
      "Morocco",
      "Nigeria",
      "Senegal",
      "Tunisia",
      "Australia",
      "Iran",
      "Japan",
      "Korea Republic",
      "Saudi Arabia",
      "Belgium",
      "Croatia",
      "Denmark",
      "England",
      "Germany",
      "Iceland",
      "Poland",
      "Portugal",
      "Russia",
      "Serbia",
      "Spain",
      "Sweden",
      "Switzerland",
      "Costa Rica",
      "Mexico",
      "Panama",
      "Argentina",
      "Brazil",
      "Colombia",
      "Peru",
      "Uruguay"] }

type Msg
  = NoOp

init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


viewTeam: Team -> Html Msg
viewTeam team =
  li [] [text team]

view : Model -> Html Msg
view model =
  ul [] <|
    List.map viewTeam model.teams


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
