module Coaches.Update (..) where


import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import Task


import Coaches.Actions exposing (..)
import Coaches.Models exposing (..)
import Coaches.Effects exposing (..)


type alias UpdateModel =
  {
    coaches : List Coach,
    showErrorAddress : Signal.Address String
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
          let
            errorMessage =
              toString error

            fx =
              Signal.send model.showErrorAddress errorMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.coaches, fx )


    TaskDone () ->
      ( model.coaches, Effects.none )


    SaveCoach newCoach ->
      let
        fxForCoach coach =
          if coach.id /= newCoach.id then
            Effects.none
          else
            let
              updatedCoach =
                newCoach
            in
              save updatedCoach

        fx =
          List.map fxForCoach model.coaches
          |> Effects.batch
      in
        ( model.coaches, fx )


    CreateCoach newCoach ->
      (model.coaches, save newCoach)


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
            fx =
              Task.succeed ListCoaches
              |> Effects.task
          in
            ( model.coaches, fx )

        Err error ->
          let
            message =
              toString error
          in
            ( model.coaches, Effects.none )
