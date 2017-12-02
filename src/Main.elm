module Main exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing(interpolate)
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
  { name: String
  , flagImageUrl: String
  }

emptyModel : Model
emptyModel =
    { teams =
      { name = "France", flagImageUrl = "" }
      :: { name = "Germany", flagImageUrl = "" }
      :: { name = "Egypt", flagImageUrl = "" }
      :: { name = "Morocco", flagImageUrl = "" }
      :: { name = "Nigeria", flagImageUrl = "" }
      :: { name = "Senegal", flagImageUrl = "" }
      :: { name = "Tunisia", flagImageUrl = "" }
      :: { name = "Australia", flagImageUrl = "" }
      :: { name = "Iran", flagImageUrl = "" }
      :: { name = "Japan", flagImageUrl = "" }
      :: { name = "Korea Republic", flagImageUrl = "http://img.freeflagicons.com/preview/korea_south.png" }
      :: { name = "Saudi Arabia", flagImageUrl = "http://img.freeflagicons.com/preview/saudi_arabia.png" }
      :: { name = "Belgium", flagImageUrl = "" }
      :: { name = "Croatia", flagImageUrl = "" }
      :: { name = "Denmark", flagImageUrl = "" }
      :: { name = "England", flagImageUrl = "" }
      :: { name = "Germany", flagImageUrl = "" }
      :: { name = "Iceland", flagImageUrl = "" }
      :: { name = "Poland", flagImageUrl = "" }
      :: { name = "Portugal", flagImageUrl = "" }
      :: { name = "Russia", flagImageUrl = "" }
      :: { name = "Serbia", flagImageUrl = "" }
      :: { name = "Spain", flagImageUrl = "" }
      :: { name = "Sweden", flagImageUrl = "" }
      :: { name = "Switzerland", flagImageUrl = "" }
      :: { name = "Costa Rica", flagImageUrl = "http://img.freeflagicons.com/preview/costa_rica.png" }
      :: { name = "Mexico", flagImageUrl = "" }
      :: { name = "Panama", flagImageUrl = "" }
      :: { name = "Argentina", flagImageUrl = "" }
      :: { name = "Brazil", flagImageUrl = "" }
      :: { name = "Colombia", flagImageUrl = "" }
      :: { name = "Peru", flagImageUrl = "" }
      :: { name = "Uruguay", flagImageUrl = "" }
      :: [] }

type Msg
  = NoOp

init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )

-- Views

flagUrl: Team -> String
flagUrl team =
  if team.flagImageUrl == "" then
    interpolate "http://img.freeflagicons.com/preview/{0}.png" [String.toLower team.name]
  else
    team.flagImageUrl



viewTeam: Team -> Html Msg
viewTeam team =
  li []
    [ img [src <| flagUrl team, style [("width", "20px")] ] []
    , span [] [text team.name]
    ]

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
