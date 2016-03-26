module Coaches.Models (..) where


type alias CoachId =
  Int

type alias Coach =
  {
    id : CoachId,
    name : String,
    spots : Int,
    coach : Bool,
    mentor : Bool,
    capabilities : String,
    description : String
  }


new : Coach
new =
  {
    id = 0,
    name = "",
    spots = 0,
    coach = False,
    mentor = False,
    capabilities = "",
    description = ""
  }