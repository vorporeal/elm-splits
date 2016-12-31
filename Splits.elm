-- A simple timer with splits for tracking speedruns.

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time exposing (..)
import Task exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type ActiveSplit = Unstarted | SplitIndex Int | Done

type alias Split =
  { name : String
  , endTime : Time
  , duration : Time
  }


type alias Model =
  { isRunning : Bool
  , elapsedTime : Time
  , lastTickTime : Maybe Time
  , activeSplit : ActiveSplit
  , splits : List Split
  }


init : (Model, Cmd Msg)
init =
  (emptyModel, Cmd.none)
  
  
emptyModel : Model
emptyModel =
  Model False 0 Nothing Unstarted []



-- UPDATE


type Msg
    = StartTimer
    | StopTimer
    | SetLastTickTime Time
    | Tick Time
    | ResetTimer


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartTimer ->
      let
        -- Ensure a started timer is marked as running.
        newModel = { model | isRunning = True }
      in
        -- If we don't know when the last tick occurred, set it to
        -- the current time, so that the next tick actually starts
        -- updating the timer.
        case model.lastTickTime of
          Nothing ->
            (newModel, Task.perform SetLastTickTime Time.now)
          Just _ ->
            (newModel, Cmd.none)
    
    StopTimer ->
      -- Stop the timer and unset the last tick time, as when we restart
      -- the timer, we don't want to include all of the time that elapsed
      -- while the timer was paused.
      ({ model | isRunning = False
               , lastTickTime = Nothing }
       , Cmd.none)
      
    SetLastTickTime time ->
      ({ model | lastTickTime = Just time }, Cmd.none)

    Tick time ->
      if model.isRunning then
        -- If we're running but don't know when the last tick was,
        -- the current tick was the last tick.  Otherwise, update
        -- the timer based on the interval between this tick and the
        -- last one.
        case model.lastTickTime of
          Nothing ->
            ({ model | lastTickTime = Just time }, Cmd.none)
          Just lastTickTime ->
            ({ model | elapsedTime = model.elapsedTime + (time - lastTickTime)
                     , lastTickTime = Just time }
            , Cmd.none)
      else
        (model, Cmd.none)
    
    ResetTimer ->
      (emptyModel, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick


-- VIEW


view : Model -> Html Msg
view model =
  div [style [("color", "#333"), ("font", "24px monospace")]]
    [ text <| formatTimer model
    , br [] []
    , button [onClick StartTimer] [ text "Start timer" ]
    , button [onClick StopTimer] [ text "Pause timer" ]
    , button [onClick ResetTimer] [ text "Reset timer" ]
    ]


formatTimer : Model -> String
formatTimer model =
  formatDuration model.elapsedTime
      
formatDuration : Time -> String
formatDuration duration =
  let
    milliseconds = (Time.inMilliseconds (duration / 10) |> truncate) % 100
    seconds = (Time.inSeconds duration |> truncate) % 60
    minutes = (Time.inMinutes duration |> truncate) % 60
    hours = Time.inHours duration |> truncate
  in
    formatDurationComponent hours ":" 0 True ++
    formatDurationComponent minutes ":" (if hours == 0 then 0 else 2) True ++
    formatDurationComponent seconds "." (if minutes == 0 then 0 else 2) False ++
    formatDurationComponent milliseconds "" 2 False
    
formatDurationComponent : Int -> String -> Int -> Bool -> String
formatDurationComponent value separator pad truncate =
  if value > 0 || not truncate then
    (toString value |> String.padLeft pad '0') ++ separator
  else
    ""
