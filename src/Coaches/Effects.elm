module Coaches.Effects (..) where

import Effects exposing (Effects)
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Task


import Coaches.Models exposing (CoachId, Coach)
import Coaches.Actions exposing (..)


fetchAll : Effects Action
fetchAll =
  Http.get collectionDecoder fetchAllUrl
    |> Task.toResult
    |> Task.map FetchAllDone
    |> Effects.task


fetchAllUrl : String
fetchAllUrl =
  "http://localhost:4000/coaches"


create : Coach -> Effects Action
create coach =
  let
    body =
      memberEncoded coach
        |> Encode.encode 0
        |> Http.string

    config =
      {
        verb = "POST",
        headers = [ ( "Content-Type", "application/json" ) ],
        url = createUrl,
        body = body
      }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson memberDecoder
      |> Task.toResult
      |> Task.map CreateCoachDone
      |> Effects.task


createUrl : String
createUrl =
  "http://localhost:4000/coaches"


-- DECODERS
collectionDecoder : Decode.Decoder (List Coach)
collectionDecoder =
  Decode.list memberDecoder


memberDecoder : Decode.Decoder Coach
memberDecoder =
  Decode.object7
    Coach
    ("id" := Decode.int)
    ("name" := Decode.string)
    ("spots" := Decode.int)
    ("mentor" := Decode.bool)
    ("coach" := Decode.bool)
    ("capabilities" := Decode.string)
    ("description" := Decode.string)


-- ENCODE
memberEncoded : Coach -> Encode.Value
memberEncoded coach =
  let
    list =
      [
        ( "id", Encode.int coach.id ),
        ( "name", Encode.string coach.name ),
        ( "spots", Encode.int coach.spots ),
        ( "mentor", Encode.bool coach.mentor ),
        ( "coach", Encode.bool coach.coach ),
        ( "capabilities", Encode.string coach.capabilities ),
        ( "description", Encode.string coach.description )
      ]
  in
    list
      |> Encode.object