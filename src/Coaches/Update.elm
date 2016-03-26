module Coaches.Update (..) where


import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import Task


import Coaches.Actions exposing (..)
import Coaches.Models exposing (..)
import Coaches.Effects exposing (..)


type alias UpdateModel =
  {
    coaches : List Coach
  }


update : Action -> UpdateModel -> (List Coach, Effects Action)
update action model =
  case action of
    NoOp ->
      (model.coaches, Effects.none)

    EditCoach id ->
      let
        path =
          "/coaches/" ++ (toString id) ++ "/edit"

      in
        (model.coaches, Effects.map HopAction (navigateTo path))

    ListCoaches ->
      let
        path =
          "/coaches"

      in
        (model.coaches, Effects.map HopAction (navigateTo path))

    HopAction _ ->
      (model.coaches, Effects.none)

    FetchAllDone result ->
      case result of
        Ok coaches ->
          (coaches, Effects.none)

        Err error ->
          (model.coaches, Effects.none)

    CreateCoach ->
      (model.coaches, create newCoach)

    CreateNewCoach ->
      let
        path =
          "/coaches/new"

      in
        (model.coaches, Effects.map HopAction (navigateTo path))

    CreateCoachDone result ->
      case result of
        Ok coach ->
          let
            updatedCollection =
              coach :: model.coaches

            fx =
              Task.succeed (EditCoach coach.id)
                |> Effects.task
          in
            ( updatedCollection, fx )

        Err error ->
          let
            message =
              toString error
          in
            ( model.coaches, Effects.none )