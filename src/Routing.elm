module Routing (..) where


import Task exposing (Task)
import Effects exposing (Effects, Never)
import Hop
import Hop.Types exposing (Location, PathMatcher, Router, newLocation)
import Hop.Navigate exposing (navigateTo)
import Hop.Matchers exposing (match1, match2, match3, int)


import Coaches.Models exposing (CoachId, Coach)


type Route =
  CoachesRoute
  | CoachNewRoute
  | CoachEditRoute CoachId
  | NotFoundRoute


type Action =
  HopAction ()
  | ApplyRoute ( Route, Location )
  | NavigateTo String


type alias Model =
  {
    location : Location,
    route : Route
  }


initialModel : Model
initialModel =
  {
    location = newLocation,
    route = CoachesRoute
  }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NavigateTo path ->
      ( model, Effects.map HopAction (navigateTo path) )

    ApplyRoute ( route, location ) ->
      ( { model | route = route, location = location }, Effects.none )

    HopAction () ->
      ( model, Effects.none )


indexMatcher : PathMatcher Route
indexMatcher =
  match1 CoachesRoute "/"


coachesMatcher : PathMatcher Route
coachesMatcher =
  match1 CoachesRoute "/coaches"


coachNewMatcher : PathMatcher Route
coachNewMatcher =
  match1 CoachNewRoute "/coaches/new"


coachEditMatch : PathMatcher Route
coachEditMatch =
  match3 CoachEditRoute "/coaches/" int "/edit"


matchers : List (PathMatcher Route)
matchers =
  [
    indexMatcher,
    coachesMatcher,
    coachEditMatch,
    coachNewMatcher
  ]


router : Router Route
router =
  Hop.new
    {
      matchers = matchers,
      notFound = NotFoundRoute
    }


run : Task () ()
run =
  router.run


signal : Signal Action
signal =
  Signal.map ApplyRoute router.signal