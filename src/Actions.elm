module Actions (..) where


import Coaches.Actions exposing (..)

import Routing

type Action =
  NoOp
  | CoachesAction Coaches.Actions.Action
  | RoutingAction Routing.Action
  | ShowError String