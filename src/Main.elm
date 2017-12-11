module Main exposing (..)

import Model exposing (Model, Team, Bracket, Group, Match, initGroupsWithTeams, emptyModel, createMatch, getTeamFromBracketMatch)
import Array exposing (..)
import Html exposing (..)
import Debug exposing (log)
import Html.Attributes exposing (..)
import String.Interpolate exposing(interpolate)
import Html.Events exposing (..)

main : Program Never Model Msg
main =
  program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

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
      [ img [src <| flagUrl team ] []
      , Html.span [] [ text team.name ]
      , Html.span [] [ text <| toString <| position + 1 ]
      ]
    ]

viewTeamInGroup: Team -> Html Msg
viewTeamInGroup team =
  li [ onClick (GroupTeamClicked team) ]
    [ div []
      [ img [src <| flagUrl team ] []
      , Html.span [] [text team.name]
      , button [ class "btn" ]
        [ i [class "fa fa-plus-square-o"] [] ]
      ]
    ]

viewGroup: Group -> List Team -> List Team -> Html Msg
viewGroup group positionned teams =
  div [class "col-3"]
    [ Html.span [] [ text (interpolate "Group {0}" [group.name]) ]
      ,ul []
        <| List.append (List.indexedMap viewPositionedTeamInGroup positionned) (List.map viewTeamInGroup teams)
    ]

viewGroups: Model -> List (Html Msg)
viewGroups model =
  Array.foldr (\group views -> (viewGroup group group.positions group.teams) :: views) [] model.groups

-- viewBracket: Bracket -> List (Html Msg)
-- viewBracket bracket =
--   [div [ id "bracket" ]
--   [ ul [ class "round round-1" ]
--     [ li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text " "
--         , span []
--             [ text " " ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "_"
--         , span []
--             [ text " " ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text ""
--         , span []
--             [ text "" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text " "
--         , span []
--             [ text "" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top " ]
--         [ text " "
--         , span []
--             [ text " " ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom winner" ]
--         [ text "Oregon "
--         , span []
--             [ text "68" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Saint Louis "
--         , span []
--             [ text "64" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "New Mexico St "
--         , span []
--             [ text "44" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Memphis "
--         , span []
--             [ text "54" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "St Mary's "
--         , span []
--             [ text "52" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Mich St "
--         , span []
--             [ text "65" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Valparaiso "
--         , span []
--             [ text "54" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Creighton "
--         , span []
--             [ text "67" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Cincinnati "
--         , span []
--             [ text "63" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Duke "
--         , span []
--             [ text "73" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Albany "
--         , span []
--             [ text "61" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     ]
--     , ul [ class "round round-2" ]
--     [ li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Lousville "
--         , span []
--             [ text "82" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Colo St "
--         , span []
--             [ text "56" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Oregon "
--         , span []
--             [ text "74" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Saint Louis "
--         , span []
--             [ text "57" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top " ]
--         [ text "Memphis "
--         , span []
--             [ text "48" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom winner" ]
--         [ text "Mich St "
--         , span []
--             [ text "70" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top " ]
--         [ text "Creighton "
--         , span []
--             [ text "50" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom winner" ]
--         [ text "Duke "
--         , span []
--             [ text "66" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     ]
--     , ul [ class "round round-3" ]
--     [ li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Lousville "
--         , span []
--             [ text "77" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Oregon "
--         , span []
--             [ text "69" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top " ]
--         [ text "Mich St "
--         , span []
--             [ text "61" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom winner" ]
--         [ text "Duke "
--         , span []
--             [ text "71" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     ]
--     , ul [ class "round round-4" ]
--     [ li [ class "spacer" ]
--         [ text " " ]
--     , li [ class "game game-top winner" ]
--         [ text "Lousville "
--         , span []
--             [ text "85" ]
--         ]
--     , li [ class "game game-spacer" ]
--         [ text " " ]
--     , li [ class "game game-bottom " ]
--         [ text "Duke "
--         , span []
--             [ text "63" ]
--         ]
--     , li [ class "spacer" ]
--         [ text " " ]
--     ]
--   ]]

viewEmptyGameOpponent: Int  -> String -> (Html Msg)
viewEmptyGameOpponent gamePosition opponentPositionClass =
  li [ classList [ ("game", True), (interpolate "game-{0}" [ opponentPositionClass ], True) ] ]
    [ text (interpolate "Game {0}" [ toString <| gamePosition + 1 ])
      , span []
          [ text "" ]
    ]

viewGameOpponent: Int -> Team -> String -> Bool -> (Html Msg)
viewGameOpponent gamePosition team opponentPositionClass winner =
  li [ classList [ ("game", True), (interpolate "game-{0}" [ opponentPositionClass ], True), ("winner", winner) ],
       onClick (BracketTeamClicked team gamePosition) ]
    [ text team.name
      , span []
          [ text "" ]
    ]
viewGameTopOpponentWithName: Int -> Team -> Bool -> (Html Msg)
viewGameTopOpponentWithName gamePosition team winner =
  viewGameOpponent gamePosition team "top" winner

viewGameBottomOpponentWithName: Int -> Team -> Bool -> (Html Msg)
viewGameBottomOpponentWithName gamePosition team winner =
  viewGameOpponent gamePosition team "bottom" winner

isTeamWinner: Team -> Maybe Team -> Bool
isTeamWinner team winner =
  case winner of
    Nothing -> False
    Just winningTeam -> team == winningTeam

viewBracketGame: Int -> Match -> List (Html Msg)
viewBracketGame index game =
  [ case (Array.get 0 game.contenders) of
      Nothing ->
        viewEmptyGameOpponent index "top"
      Just team ->
        case team of
          Nothing -> viewEmptyGameOpponent index "top"
          Just team -> viewGameTopOpponentWithName index team <| isTeamWinner team game.winner
    , li [ class "game game-spacer" ] [ text "\xA0" ]
    , case (Array.get 1 game.contenders) of
        Nothing ->
          viewEmptyGameOpponent index "bottom"
        Just team ->
          case team of
            Nothing -> viewEmptyGameOpponent index "bottom"
            Just team -> viewGameBottomOpponentWithName index team <| isTeamWinner team game.winner
    , li [ class "spacer" ] [ text "\xA0" ]
  ]

roundMatchesSlice: Int -> Array Match -> Int -> (Int, Int)
roundMatchesSlice round matches roundEntrants =
  let
    totalTeams = Array.length matches |> (+) 1
  in
    (totalTeams - roundEntrants, totalTeams - roundEntrants // 2)

gamePositionFromRoundAndRoundPosition: Int -> Int -> Int
gamePositionFromRoundAndRoundPosition round gamePositionInRound =
  case round of
    0 -> gamePositionInRound
    _ -> 16 - 16 // (round * 2) + gamePositionInRound

viewRound: Int -> Array Match -> Int -> List (Html Msg)
viewRound round matches roundEntrants =
  let
    roundMatchesIndexes = roundMatchesSlice round matches roundEntrants
    firstGameIndex = Tuple.first roundMatchesIndexes
    lastGameIndex =  Tuple.second roundMatchesIndexes
  in
    if round > 4 then
      [ span [] [ text " " ] ]
    else
      List.concat [ [ ul [ class (interpolate "round round-{0}" [ toString <| round + 1 ]) ]
        <| List.concat
          [[ li [ class "spacer" ] [ text "\xA0" ] ]
          , List.concat
             <| List.indexedMap (\index g -> viewBracketGame (gamePositionFromRoundAndRoundPosition round index) g)
             <| Array.toList
             <| Array.slice firstGameIndex lastGameIndex matches
          , [ li [ class "spacer" ] [ text "\xA0" ] ]]
      ]
      , (viewRound (round + 1) matches <| roundEntrants // 2)
      ]

viewBracket: Bracket -> List (Html Msg)
viewBracket bracket =
  [ div [ id "bracket" ]
     <| viewRound 0 bracket 16
  ]

view : Model -> Html Msg
view model =
  div [] <|
    List.append
      ([div [ id "groups", class "flex flex-wrap" ] <| viewGroups model ])
      ( viewBracket model.bracket )

-- Updates

-- #1 A 2B
-- #2 C 2D
-- #3 E 2F
-- #4 G 2H
-- #5 B 2A
-- #6 D 2C
-- #7 F 2E
-- #8 H 2G
setMatch: Int -> Int -> Int -> Team -> Bracket -> Bracket
setMatch groupRanking winnerPosition runnerUpPosition team bracket =
  let
    -- case groupRanking of
    --   0 ->
    --     gameIndex = winnerPosition
    --     game = case Array.get gameIndex bracket of
    --       Nothing -> Debug.crash "impossible"
    --       Just game -> game
    --   1 ->
    --     gameIndex = runnerUpPosition
    --     game = case Array.get gameIndex bracket of
    --       Nothing -> Debug.crash "impossible"
    --       Just game -> game
    --   _ -> Debug.crash "impossible"

    winnerGame = case Array.get winnerPosition bracket of
      Nothing -> Debug.crash "impossible"
      Just game -> game
    runnerUpGame = case Array.get runnerUpPosition bracket of
      Nothing -> Debug.crash "impossible"
      Just game -> game
  in
    -- Array.set gameIndex { game | contenders = (Array.set groupRanking (Just team) game.contenders) } bracket
    if groupRanking == 0 then
      Array.set winnerPosition { winnerGame | contenders = (Array.set 0 (Just team) winnerGame.contenders) } bracket
    else
      Array.set runnerUpPosition { runnerUpGame | contenders = (Array.set 1 (Just team) runnerUpGame.contenders) } bracket

-- SHOULD USE TUPLES
fillRound16: Bracket -> Team -> Int -> Bracket
fillRound16 bracket team groupRanking =
  case team.groupId of
    0 ->
      setMatch groupRanking 0 4 team bracket
    1 ->
      setMatch groupRanking 4 0 team bracket
    2 ->
      setMatch groupRanking 1 5 team bracket
    3 ->
      setMatch groupRanking 5 1 team bracket
    4 ->
      setMatch groupRanking 2 6 team bracket
    5 ->
      setMatch groupRanking 6 2 team bracket
    6 ->
      setMatch groupRanking 3 7 team bracket
    7 ->
      setMatch groupRanking 7 3 team bracket
    _ ->
      Debug.crash "GROUP ID SHOULD BE SET"

addTeamToGroupPosition: Team -> Model -> Model
addTeamToGroupPosition team model =
  let
    g = case Array.get team.groupId model.groups of
      Nothing -> Debug.crash "GROUPS SHOULD BE SET"
      Just group ->
        group
  in
    { model |
      groups = Array.set team.groupId ({ g | positions = g.positions ++ [team], teams = List.filter (\t -> t.name /= team.name) g.teams }) model.groups
      , bracket = fillRound16 model.bracket team <| List.length g.positions
    }

advanceTeamToNextRound: Model -> Team -> Int -> Model
advanceTeamToNextRound model winningTeam currentGamePosition =
  let
    bracket = model.bracket
    nextGamePosition = log "new nextGamePosition : " <| (log "new currentGamePosition : " currentGamePosition) // 2 + 8 -- nextGamePosition = Math.round(match# / 2) + Round-1-match-count
    currentGame = case Array.get currentGamePosition bracket of
      Nothing -> Debug.crash "impossible"
      Just game -> { game | winner = (Just winningTeam) }
    contenderPosition = if currentGamePosition % 2 == 0 then 0 else 1
    nextGame = case Array.get nextGamePosition bracket of
      Nothing -> Debug.crash "impossible"
      Just game -> { game | contenders = (Array.set contenderPosition (Just winningTeam) game.contenders) }
  in
  { model |
    bracket = Array.set currentGamePosition currentGame bracket
                |> Array.set nextGamePosition nextGame
  }
type Msg = GroupTeamClicked Team | BracketTeamClicked Team Int | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GroupTeamClicked team ->
          ( addTeamToGroupPosition team model, Cmd.none )
        BracketTeamClicked team gameNumber ->
          ( advanceTeamToNextRound model team gameNumber, Cmd.none )
        NoOp ->
          ( model, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- next matchnumber = match# // 2 + Round-1-match-count
-- https://codepen.io/aronduby/pen/qliuj
-- https://github.com/agoragames/bracket_tree
-- https://stackoverflow.com/questions/12150313/advance-players-to-next-match-tournament-brackets-need-some-logic
