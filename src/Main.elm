module Main exposing (..)

-- import Dom
import Html exposing (..)
import Debug exposing (log)
import Html.Attributes exposing (..)
import String.Interpolate exposing(interpolate)
import Html.Events exposing (..)
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
  , groups: List Group
  , bracket: Bracket }

type alias Team =
  { name: String
  , flagImageUrl: String
  , groupId: String
  }

type alias Group =
  { name: String
  , id: String
  , teams: List Team
  , positions: List Team
  }

type alias Match =
  { teamA: Team
  , teamB: Team
  , number: Int
  , winner: Team
  }

type alias Bracket =
  List Match

-- next matchnumber = Math.round(match# / 2) + Round-1-match-count
-- [{ Team, Team, position, winner }]

emptyModel : Model
emptyModel =
    {
      bracket = []
      , groups =
        { name = "A", id = "A", positions = [], teams = [] }
        :: { name = "B", id = "B", positions = [], teams = [] }
        :: { name = "C", id = "C", positions = [], teams = [] }
        :: { name = "D", id = "D", positions = [], teams = [] }
        :: { name = "E", id = "E", positions = [], teams = [] }
        :: { name = "F", id = "F", positions = [], teams = [] }
        :: { name = "G", id = "G", positions = [], teams = [] }
        :: { name = "H", id = "H", positions = [], teams = [] }
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


addTeamToPosition: Model -> Team -> Model
addTeamToPosition model team =
  { model | groups = List.map (\g -> if g.id == team.groupId then { g | positions = g.positions ++ [team], teams = List.filter (\t -> t.name /= team.name) g.teams } else g ) model.groups }

setTeamsInGroup: Group -> List Group -> List Team -> List Group
setTeamsInGroup group groups teams =
  List.map (\g -> if g.id == group.id then { group | teams = teams } else g ) groups

initGroupsWithTeams : Model -> Model
initGroupsWithTeams model =
  List.foldr (\group model -> { model | groups = (setTeamsInGroup group model.groups <| List.filter (\team -> team.groupId == group.id) model.teams) } ) model model.groups

init : ( Model, Cmd Msg )
init =
    ( initGroupsWithTeams emptyModel, Cmd.none )

-- Views

flagUrl: Team -> String
flagUrl team =
  if team.flagImageUrl == "" then
    interpolate "http://img.freeflagicons.com/preview/{0}.png" [String.toLower team.name]
  else
    team.flagImageUrl

viewPositionedTeamInGroup: Int -> Team -> Html Msg
viewPositionedTeamInGroup position team  =
  li []
    [ div []
      [ img [src <| flagUrl team, style [("width", "20px")] ] []
      , span [] [ text team.name ]
      , span [] [ text <| toString <| position + 1 ]
      ]
    ]

viewTeamInGroup: Team -> Html Msg
viewTeamInGroup team =
  li [ onClick (GroupTeamClicked team) ]
    [ div []
      [ img [src <| flagUrl team, style [("width", "20px")] ] []
      , span [] [text team.name]
      , button [ class "btn" ]
        [ i [class "fa fa-plus-square-o"] [] ]
      ]
    ]

viewGroup: Group -> List Team -> List Team -> Html Msg
viewGroup group positionned teams =
  div [class "inline-block col lg-col-3 md-col-3 sm-col-6"]
    [ span [] [ text (interpolate "Group {0}" [group.name]) ]
      ,ul []
        <| List.append (List.indexedMap viewPositionedTeamInGroup positionned) (List.map viewTeamInGroup teams)
    ]

viewGroups: Model -> List (Html Msg)
viewGroups model =
  List.sortBy .name model.groups
    |> List.map (\group -> viewGroup group group.positions group.teams)

view : Model -> Html Msg
view model =
  div [] <|
    viewGroups model




-- Updates
type Msg = GroupTeamClicked Team | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GroupTeamClicked team ->
          log "new Model : " ( addTeamToPosition model team, Cmd.none )
        NoOp ->
          ( model, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


  -- https://codepen.io/aronduby/pen/qliuj
  -- https://github.com/agoragames/bracket_tree
  -- https://stackoverflow.com/questions/12150313/advance-players-to-next-match-tournament-brackets-need-some-logic
