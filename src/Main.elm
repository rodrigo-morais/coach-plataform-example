module Main (..) where


import Html exposing (..)
import Effects exposing (Effects, Never)
import Task
import StartApp


import Actions exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)


-- START APP
init : (Model, Effects Action)
init =
  (initialModel, Effects.none)


app : StartApp.App Model
app =
  StartApp.start
  {
    init = init,
    inputs = [ ],
    update = update,
    view = view
  }


-- MAIN
main : Signal.Signal Html
main =
  app.html


-- PORT
port runner : Signal (Task.Task Never ())
port runner =
  app.tasks