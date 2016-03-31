module Coaches.Edit (..) where


import Html exposing (..)
import Html.Attributes exposing (class, value, href, placeholder, cols, rows, type', checked)
import Html.Events exposing (onClick, targetValue, on, targetChecked)


import String


import Coaches.Models exposing (..)
import Coaches.Actions exposing (..)


type alias ViewModel =
  {
    coach : Coach
  }


view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div []
      [
        nav address model,
        form address model
      ]


nav : Signal.Address Action -> ViewModel -> Html.Html
nav address model =
  div [ class "clearfix mb2 white bg-black" ]
      [
        div [ class "left p1" ]
            [ listBtn address model ],
        div [ class "right p1" ]
            [ saveBtn address model ]
      ]


form : Signal.Address Action -> ViewModel -> Html.Html
form address model =
  let
    name =
      if (String.isEmpty model.coach.name) then
        "New coach"
      else
        model.coach.name

  in
    div [ class "m3" ]
        [
          h1  []
              [ text name ],
          formSpot address model,
          formName address model,
          formTypes address model,
          formCapabilities address model,
          formDescription address model
        ]


formSpot : Signal.Address Action -> ViewModel -> Html.Html
formSpot address model =
  div [ class "clearfix py1" ]
      [
        div [ class "col col-3" ]
            [ text "Spot" ],
        div [ class "col col-9" ]
            [
              btnSpotDecrease address model,
              span  [ class "h2 bold" ]
                    [ text (toString model.coach.spots) ],
              btnSpotIncrease address model
            ]
      ]


btnSpotDecrease : Signal.Address Action -> ViewModel -> Html.Html
btnSpotDecrease address model =
  a [
      class "btn ml0 h1",
      onClick address (DecreaseSpot model.coach)
    ]
    [
      i [ class "fa fa-minus-circle" ]
        [ ]
    ]


btnSpotIncrease : Signal.Address Action -> ViewModel -> Html.Html
btnSpotIncrease address model =
  a [
      class "btn ml0 h1",
      onClick address (IncreaseSpot model.coach)
    ]
    [
      i [ class "fa fa-plus-circle" ]
        [ ]
    ]


formName : Signal.Address Action -> ViewModel -> Html.Html
formName address model =
  let
    class' =
      if String.isEmpty model.coach.name then
        "pr1 red bold h2"
      else
        "pr1 black bold h2"


    nameLabel =
        label [ class class']
              [ text "*" ]


  in
    div [ class "clearfix py1" ]
        [
          div [ class "col col-3" ]
              [
                text "Name ",
                nameLabel
              ],
          div [ class "col col-9" ]
              [
                inputName address model
              ]
        ]


formTypes : Signal.Address Action -> ViewModel -> Html.Html
formTypes address model =
  div [ class "clearfix py1" ]
      [
        div [ class "col col-3" ]
            [ text "Types" ],
        div [ class "col col-9" ]
            [
              div [ class "p1" ]
                  [
                    input [
                            type' "checkbox",
                            checked model.coach.mentor,
                            on "change" targetChecked (\isMentor -> Signal.message address (ChangeMentorType model.coach isMentor))
                          ]
                          [ ],
                    text "Mentor"
                  ],
              div [ class "p1" ]
                  [
                    input [
                            type' "checkbox",
                            checked model.coach.coach,
                            on "change" targetChecked (\isCoach -> Signal.message address (ChangeCoachType model.coach isCoach))
                          ]
                          [ ],
                    text "Coach"
                  ]
            ]
      ]


inputName : Signal.Address Action -> ViewModel -> Html.Html
inputName address model =
  input [
          class "field-light",
          value model.coach.name,
          placeholder "New coach",
          on "change" targetValue (\name -> Signal.message address (ChangeName model.coach name))
        ]
        [ ]


formCapabilities : Signal.Address Action -> ViewModel -> Html.Html
formCapabilities address model =
  div [ class "clearfix py1" ]
      [
        div [ class "col col-3" ]
            [ text "Capabilities" ],
        div [ class "col col-9" ]
            [
              inputCapabilities address model
            ]
      ]


inputCapabilities : Signal.Address Action -> ViewModel -> Html.Html
inputCapabilities address model =
  input [
          class "field-light col col-12",
          value model.coach.capabilities,
          placeholder "Fill the capabilities of coach",
          on "change" targetValue (\capabilities -> Signal.message address (ChangeCapabilities model.coach capabilities))
        ]
        [ ]


formDescription : Signal.Address Action -> ViewModel -> Html.Html
formDescription address model =
  div [ class "clearfix py1" ]
      [
        div [ class "col col-3" ]
            [ text "Description" ],
        div [ class "col col-9" ]
            [
              inputDescription address model
            ]
      ]


inputDescription : Signal.Address Action -> ViewModel -> Html.Html
inputDescription address model =
  textarea  [
              class "field-light col col-12",
              value model.coach.description,
              placeholder "Fill the description of coach",
              cols 12,
              rows 5,
              on "change" targetValue (\description -> Signal.message address (ChangeDescription model.coach description))
            ]
            [ ]


listBtn : Signal.Address Action -> ViewModel -> Html.Html
listBtn address model =
  button  [
            class "btn regular",
            onClick address ReturnToListCoaches
          ]
          [
            i [ class "fa fa-chevron-left mr1" ]
              [],
            text "List"
          ]


saveBtn : Signal.Address Action -> ViewModel -> Html.Html
saveBtn address model =
  let
    saveEvent =
      if model.coach.id == 0 then
        CreateCoach
      else
        SaveCoach


  in
    button  [
              class "btn regular",
              onClick address (saveEvent model.coach)
            ]
            [
              i [ class "fa fa-floppy-o mr1" ] 
                [ ],
              text "Save coach"
            ]