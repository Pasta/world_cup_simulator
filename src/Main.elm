module Main exposing (..)

-- import Dom
import Html exposing (..)
-- import Debug exposing (log)
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

-- Model

type alias Model =
  { teams: List Team
  , groups: List Group }

type alias Team =
  { name: String
  , flagImageUrl: String
  , groupId: String
  }

type alias Group =
  { name: String
  , id: String
  }



emptyModel : Model
emptyModel =
    {
      groups =
        { name = "A", id = "A" }
        :: { name = "B", id = "B" }
        :: { name = "C", id = "C" }
        :: { name = "D", id = "D" }
        :: { name = "E", id = "E" }
        :: { name = "F", id = "F" }
        :: { name = "G", id = "G" }
        :: { name = "H", id = "H" }
        :: []
      , teams =
        { name = "France", flagImageUrl = "", groupId = "C" }
        :: { name = "Germany", flagImageUrl = "", groupId = "F" }
        :: { name = "Egypt", flagImageUrl = "", groupId = "A" }
        :: { name = "Morocco", flagImageUrl = "", groupId = "B" }
        :: { name = "Nigeria", flagImageUrl = "", groupId = "D" }
        :: { name = "Senegal", flagImageUrl = "", groupId = "H" }
        :: { name = "Tunisia", flagImageUrl = "", groupId = "G" }
        :: { name = "Australia", flagImageUrl = "", groupId = "C" }
        :: { name = "Iran", flagImageUrl = "", groupId = "B" }
        :: { name = "Japan", flagImageUrl = "", groupId = "H" }
        :: { name = "South Korea", flagImageUrl = "http://img.freeflagicons.com/preview/korea_south.png", groupId = "F" }
        :: { name = "Saudi Arabia", flagImageUrl = "http://img.freeflagicons.com/preview/saudi_arabia.png", groupId = "A" }
        :: { name = "Belgium", flagImageUrl = "", groupId = "G" }
        :: { name = "Croatia", flagImageUrl = "", groupId = "D" }
        :: { name = "Denmark", flagImageUrl = "", groupId = "C" }
        :: { name = "England", flagImageUrl = "", groupId = "G" }
        :: { name = "Iceland", flagImageUrl = "", groupId = "D" }
        :: { name = "Poland", flagImageUrl = "", groupId = "H" }
        :: { name = "Portugal", flagImageUrl = "", groupId = "B" }
        :: { name = "Russia", flagImageUrl = "", groupId = "A" }
        :: { name = "Serbia", flagImageUrl = "", groupId = "E" }
        :: { name = "Spain", flagImageUrl = "", groupId = "B" }
        :: { name = "Sweden", flagImageUrl = "", groupId = "F" }
        :: { name = "Switzerland", flagImageUrl = "", groupId = "E" }
        :: { name = "Costa Rica", flagImageUrl = "http://img.freeflagicons.com/preview/costa_rica.png", groupId = "E" }
        :: { name = "Mexico", flagImageUrl = "", groupId = "F" }
        :: { name = "Panama", flagImageUrl = "", groupId = "G" }
        :: { name = "Argentina", flagImageUrl = "", groupId = "D" }
        :: { name = "Brazil", flagImageUrl = "", groupId = "E" }
        :: { name = "Colombia", flagImageUrl = "", groupId = "H" }
        :: { name = "Peru", flagImageUrl = "", groupId = "C" }
        :: { name = "Uruguay", flagImageUrl = "", groupId = "A" }
        :: []
    }

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
    , span [] [text team.groupId]
    ]

viewGroup: Group -> List Team -> Html Msg
viewGroup group teams =
  div []
    [ span [] [text group.name]
      ,ul []
        <| List.map viewTeam teams
    ]

viewGroups: Model -> List (Html Msg)
viewGroups model =
  List.sortBy .name model.groups
    |> List.map (\group -> List.filter (\team -> team.groupId == group.id) model.teams |> viewGroup group )


view : Model -> Html Msg
view model =
  div [] <|
    viewGroups model
  -- ul [] <|
  --   List.map viewTeam <| List.sortBy .groupId model.teams


-- Updates

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
