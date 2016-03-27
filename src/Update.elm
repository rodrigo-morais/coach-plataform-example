module Update (..) where


import Effects exposing (Effects)


import Actions exposing (..)
import Models exposing (..)
import Mailboxes exposing (..)


import Coaches.Update exposing (..)


import Routing


update : Action -> AppModel -> (AppModel, Effects Action)
update action model =
  case action of
    CoachesAction subAction ->
      let
        updateModel =
          {
            coaches = model.coaches,
            showErrorAddress = Signal.forwardTo actionsMailbox.address ShowError
          }

        (updatedCoaches, fx) =
          Coaches.Update.update subAction updateModel
      
      in
        ({ model | coaches = updatedCoaches }, Effects.map CoachesAction fx)

    NoOp ->
      (model, Effects.none)

    RoutingAction subAction ->
      let
        (updatedRouting, fx) =
          Routing.update subAction model.routing

      in
        ({ model | routing = updatedRouting }, Effects.map  RoutingAction fx)

    ShowError message ->
      ( { model | errorMessage = message }, Effects.none )