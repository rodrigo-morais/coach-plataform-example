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
  | SaveCoach Coach
  | CreateNewCoach
  | CreateCoach Coach
  | CoachDone (Result Http.Error Coach)
  | TaskDone ()
  | IncreaseSpot Coach
  | DecreaseSpot Coach
  | ChangeName Coach String
  | ChangeCapabilities Coach String
  | ChangeDescription Coach String