module Model exposing (..)

import Array exposing (..)
-- Model

type alias Model =
  { teams: List Team
  , groups: Array Group
  , bracket: Bracket }

type alias Team =
  { name: String
  , groupId: Int
  , flagImageUrl: String
  }

type alias Group =
  { name: String
  , id: Int
  , teams: List Team
  , positions: List Team
  }

type alias Match =
  { contenders: Array (Maybe Team)
  , winner: Maybe Team
  }

type alias Bracket =
  Array Match

emptyModel : Model
emptyModel =
    {
      bracket = Array.repeat 15 <| createMatch Nothing Nothing Nothing
      , groups = Array.fromList
        ({ name = "A", id = 0, positions = [], teams = [] }
        :: { name = "B", id = 1, positions = [], teams = [] }
        :: { name = "C", id = 2, positions = [], teams = [] }
        :: { name = "D", id = 3, positions = [], teams = [] }
        :: { name = "E", id = 4, positions = [], teams = [] }
        :: { name = "F", id = 5, positions = [], teams = [] }
        :: { name = "G", id = 6, positions = [], teams = [] }
        :: { name = "H", id = 7, positions = [], teams = [] }
        :: [])
      , teams =
        { name = "France", flagImageUrl = "", groupId = 2 }
        :: { name = "Germany", flagImageUrl = "", groupId = 5 }
        :: { name = "Egypt", flagImageUrl = "", groupId = 0 }
        :: { name = "Morocco", flagImageUrl = "", groupId = 1 }
        :: { name = "Nigeria", flagImageUrl = "", groupId = 3 }
        :: { name = "Senegal", flagImageUrl = "", groupId = 7 }
        :: { name = "Tunisia", flagImageUrl = "", groupId = 6 }
        :: { name = "Australia", flagImageUrl = "", groupId = 2 }
        :: { name = "Iran", flagImageUrl = "", groupId = 1 }
        :: { name = "Japan", flagImageUrl = "", groupId = 7 }
        :: { name = "South Korea", flagImageUrl = "http://img.freeflagicons.com/preview/korea_south.png", groupId = 5 }
        :: { name = "Saudi Arabia", flagImageUrl = "http://img.freeflagicons.com/preview/saudi_arabia.png", groupId = 0 }
        :: { name = "Belgium", flagImageUrl = "", groupId = 6 }
        :: { name = "Croatia", flagImageUrl = "", groupId = 3 }
        :: { name = "Denmark", flagImageUrl = "", groupId = 2 }
        :: { name = "England", flagImageUrl = "", groupId = 6 }
        :: { name = "Iceland", flagImageUrl = "", groupId = 3 }
        :: { name = "Poland", flagImageUrl = "", groupId = 7 }
        :: { name = "Portugal", flagImageUrl = "", groupId = 1 }
        :: { name = "Russia", flagImageUrl = "", groupId = 0 }
        :: { name = "Serbia", flagImageUrl = "", groupId = 4 }
        :: { name = "Spain", flagImageUrl = "", groupId = 1 }
        :: { name = "Sweden", flagImageUrl = "", groupId = 5 }
        :: { name = "Switzerland", flagImageUrl = "", groupId = 4 }
        :: { name = "Costa Rica", flagImageUrl = "http://img.freeflagicons.com/preview/costa_rica.png", groupId = 4 }
        :: { name = "Mexico", flagImageUrl = "", groupId = 5 }
        :: { name = "Panama", flagImageUrl = "", groupId = 6 }
        :: { name = "Argentina", flagImageUrl = "", groupId = 3 }
        :: { name = "Brazil", flagImageUrl = "", groupId = 4 }
        :: { name = "Colombia", flagImageUrl = "", groupId = 7 }
        :: { name = "Peru", flagImageUrl = "", groupId = 2 }
        :: { name = "Uruguay", flagImageUrl = "", groupId = 0 }
        :: []
    }

getTeamFromBracketMatch: Int -> Int -> Bracket -> Maybe Team
getTeamFromBracketMatch teamNumber matchNumber bracket =
  case Array.get matchNumber bracket of
    Nothing ->
      Nothing
    Just match ->
      case (Array.get teamNumber match.contenders) of
        Nothing ->
          Nothing
        Just team ->
          team


createMatch: Maybe Team -> Maybe Team -> Maybe Team -> Match
createMatch teamOne teamTwo winner =
  { contenders = Array.fromList [teamOne, teamTwo], winner = winner }

setTeamsInGroup: Group -> Array Group -> List Team -> Array Group
setTeamsInGroup group groups teams =
  Array.map (\g -> if g.id == group.id then { group | teams = teams } else g ) groups

initGroupsWithTeams : Model -> Model
initGroupsWithTeams model =
  Array.foldr (\group model -> { model | groups = (setTeamsInGroup group model.groups <| List.filter (\team -> team.groupId == group.id) model.teams) } ) model model.groups
