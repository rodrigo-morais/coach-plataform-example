module View (..) where


import Html exposing (..)
import Html.Attributes exposing (class)


import Actions exposing (..)
import Models exposing (..)


import Coaches.List exposing (..)
import Coaches.Edit exposing (..)
import Coaches.Models exposing (CoachId, Coach, newCoach)


import Routing


import String


view : Signal.Address Action -> AppModel -> Html.Html
view address model =
  div [ ]
      [
        flash address model,
        page address model
      ]


flash : Signal.Address Action -> AppModel -> Html
flash address model =
  if String.isEmpty model.errorMessage then
    span [ ] [ ]
  else
    div [ class "bold center p2 mb2 white bg-red rounded" ]
        [ text model.errorMessage ]


page : Signal.Address Action -> AppModel -> Html.Html
page address model =
  case model.routing.route of
    Routing.CoachesRoute ->
      coachesPage address model

    Routing.CoachNewRoute ->
      coachPage address model 0

    Routing.CoachEditRoute coachId ->
      coachPage address model coachId

    Routing.NotFoundRoute ->
      notFoundView


coachesPage : Signal.Address Action -> AppModel -> Html.Html
coachesPage address model =
  let
    viewModel =
      {
        coaches = model.coaches
      }
  in
    Coaches.List.view (Signal.forwardTo address CoachesAction) viewModel


coachPage : Signal.Address Action -> AppModel -> CoachId -> Html.Html
coachPage address model coachId =
  let
    maybeCoach =
      model.coaches
        |> List.filter (\coach -> coach.id == coachId)
        |> List.head

  in
    case maybeCoach of
      Just coach ->
        let
          viewModel =
            {
              coach = coach
            }
        in
          Coaches.Edit.view (Signal.forwardTo address CoachesAction) viewModel

      Nothing ->
        notFoundView


notFoundView : Html.Html
notFoundView =
  div []
      [
        text "Not found"
      ]