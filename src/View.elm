module View (..) where


import Html exposing (..)

import Actions exposing (..)
import Models exposing (..)


import Coaches.List exposing (..)
import Coaches.Edit exposing (..)
import Coaches.Models exposing (CoachId, Coach, newCoach)


import Routing


view : Signal.Address Action -> AppModel -> Html.Html
view address model =
  div [ ]
      [
        page address model
      ]


page : Signal.Address Action -> AppModel -> Html.Html
page address model =
  case model.routing.route of
    Routing.CoachesRoute ->
      coachesPage address model

    Routing.CoachNewRoute ->
      coachNewPage address

    Routing.CoachEditRoute coachId ->
      coachEditPage address model coachId

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


coachNewPage : Signal.Address Action -> Html.Html
coachNewPage address =
  let
    viewModel =
      {
        coach = newCoach
      }
  in
    Coaches.Edit.view (Signal.forwardTo address CoachesAction) viewModel


coachEditPage : Signal.Address Action -> AppModel -> CoachId -> Html.Html
coachEditPage address model coachId =
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