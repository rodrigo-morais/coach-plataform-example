module Coaches.Actions (..) where


import Coaches.Models exposing (CoachId, Coach)


import Hop
import Http


type Action =
  NoOp
  | HopAction ()
  | EditCoach CoachId
  | ListCoaches
  | FetchAllDone (Result Http.Error (List Coach))
  | CreateNewCoach
  | CreateCoach
  | CreateCoachDone (Result Http.Error Coach)
  | TaskDone ()