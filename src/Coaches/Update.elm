module Coaches.Update (..) where


import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import Task
import String


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


        fx =
          Effects.map HopAction (navigateTo path)
          

      in
        (model.coaches, fx)


    ReturnToListCoaches ->
      let
        fx =
          Task.succeed ListCoaches
          |> Effects.task


        updateCoaches =
          List.filter (\coach -> coach.id /= 0) model.coaches


        updatedCoaches =
          { model | coaches = updateCoaches }


      in
        (updatedCoaches.coaches, fx)


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
          if String.isEmpty newCoach.name then
            Signal.send model.showErrorAddress "The field 'Name' couldn't be blank."
            |> Effects.task
            |> Effects.map TaskDone
          else
            List.map fxForCoach model.coaches
            |> Effects.batch
      in
        ( model.coaches, fx )


    CreateCoach newCoach ->
      let
        fx =
          if String.isEmpty newCoach.name then
            Signal.send model.showErrorAddress "The field 'Name' couldn't be blank."
            |> Effects.task
            |> Effects.map TaskDone
          else
            save newCoach


      in
        (model.coaches, fx)


    CreateNewCoach ->
      let
        path =
          "/coaches/new"


        updatedCoach =
          newCoach :: model.coaches

      in
        (updatedCoach, Effects.map HopAction (navigateTo path))


    CoachDone result ->
      case result of
        Ok savedCoach ->
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


    IncreaseSpot coach ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | spots = existing.spots + 1 }

        updatedCoach =
          List.map updateCoach model.coaches
            
      in
        ( updatedCoach, Effects.none )


    DecreaseSpot coach ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | spots = existing.spots - 1 }

        updatedCoach =
          List.map updateCoach model.coaches
      in
        ( updatedCoach, Effects.none )


    ChangeName coach name ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | name = name }

        updatedCoach =
          List.map updateCoach model.coaches
      in
        ( updatedCoach, Effects.none )


    ChangeCapabilities coach capabilities ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | capabilities = capabilities }

        updatedCoach =
          List.map updateCoach model.coaches
      in
        ( updatedCoach, Effects.none )


    ChangeDescription coach description ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | description = description }

        updatedCoach =
          List.map updateCoach model.coaches
      in
        ( updatedCoach, Effects.none )


    ChangeMentorType coach isMentor ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | mentor = isMentor }

        updatedCoach =
          List.map updateCoach model.coaches
      in
        ( updatedCoach, Effects.none )


    ChangeCoachType coach isCoach ->
      let
        updateCoach existing =
          if existing.id /= coach.id then
            existing
          else
            { existing | coach = isCoach }

        updatedCoach =
          List.map updateCoach model.coaches
      in
        ( updatedCoach, Effects.none )