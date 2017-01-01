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
  { index : Int
  , name : String
  , endTime : Time
  , duration : Time
  , currentAttemptEndTime : Maybe Time
  , currentAttemptDuration : Maybe Time
  }


type alias Model =
  { isRunning : Bool
  , elapsedTime : Time
  , lastTickTime : Time
  , activeSplit : ActiveSplit
  , splits : List Split
  }


init : (Model, Cmd Msg)
init =
  initModel
  
  
initModel : (Model, Cmd Msg)
initModel =
  emptyModel ! [Task.perform SetLastTickTime Time.now]
  
  
emptyModel : Model
emptyModel =
  Model False 0 0 Unstarted
    [ Split 0 "split 1" 67800 67800 Nothing Nothing
    , Split 1 "split 2" 89000 21200 Nothing Nothing
    ]



-- UPDATE


type Msg
    = StartTimerOrSplit
    | StopTimer
    | SetLastTickTime Time
    | Tick Time
    | ResetTimer


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartTimerOrSplit ->
      let
        activeSplit =
          case model.activeSplit of
            Unstarted ->
              SplitIndex 0
            SplitIndex index ->
              if index == List.length model.splits - 1 then
                Done
              else
                SplitIndex (index + 1)
            Done ->
              Done
              
        isRunning =
          case activeSplit of
            SplitIndex _ ->
              True
            _ ->
              False
              
        splits = advanceSplits model
      in
        { model | isRunning = isRunning
                , activeSplit = activeSplit
                , splits = splits}
            ! []
    
    StopTimer ->
      -- Stop the timer and unset the last tick time, as when we restart
      -- the timer, we don't want to include all of the time that elapsed
      -- while the timer was paused.
      { model | isRunning = False } ! []
      
    SetLastTickTime time ->
      { model | lastTickTime = time } ! []

    Tick time ->
      let
        dt = time - model.lastTickTime

        elapsedTime =
            if model.isRunning then
              model.elapsedTime + dt
            else
              model.elapsedTime
      in
        { model | elapsedTime = elapsedTime
                , lastTickTime = time }
            ! []
    
    ResetTimer ->
      initModel
      
      
advanceSplits : Model -> List Split
advanceSplits model =
  case model.activeSplit of
    SplitIndex index ->
      let
        updateSplit : Split -> Split -> Split
        updateSplit split prevSplit =
          if split.index == index then
            let
              prevEndTime = Maybe.withDefault 0 prevSplit.currentAttemptEndTime
            in
              { split | currentAttemptEndTime = Just model.elapsedTime
                      , currentAttemptDuration = Just (model.elapsedTime - prevEndTime) }
          else
            split
      in
        List.scanl updateSplit (Split 0 "" 0 0 Nothing Nothing) model.splits
            |> List.tail
            |> Maybe.withDefault []
    _ ->
      model.splits



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick


-- VIEW


view : Model -> Html Msg
view model =
  div [ style [("width", "400px")] ]
  [ div [style [("background", "#eee")]]
      [ viewSplitList model.splits model.activeSplit
      , div [style [("border-top", "1px solid #ddd")]] [ viewTimer model ]
      ]
  , br [] []
  , button [onClick StartTimerOrSplit] [ text "Start timer" ]
  , button [onClick StopTimer] [ text "Pause timer" ]
  , button [onClick ResetTimer] [ text "Reset timer" ]
  ]


viewTimer : Model -> Html msg
viewTimer model =
  div
    [ flexRow "center" "flex-end"
    , style [("color", "#333"), ("font", "24px monospace"), ("padding", "8px")]
    ]
    [ text <| formatDuration model.elapsedTime ]
    

viewSplitList : List Split -> ActiveSplit -> Html msg
viewSplitList splits activeSplit =
  identity splits
      |> List.indexedMap (viewSplit activeSplit)
      |> div []
    
    
viewSplit : ActiveSplit -> Int -> Split -> Html msg
viewSplit activeSplit index split =
  let
    currentSplitDelta =
      case split.currentAttemptDuration of
        Just duration ->
          Just (duration - split.duration)
        Nothing ->
          Nothing

    splitDeltaStyles : Time -> List (Attribute msg)
    splitDeltaStyles delta =
      if delta < 0 then
        [style [("color", "green")]]
      else
        [style [("color", "red")]]
    
    deltaEl =
      case currentSplitDelta of
        Just delta ->
          span (splitDeltaStyles delta) [text <| formatDelta delta]
        Nothing ->
          span [] []
  in
    div (splitStyles activeSplit index)
      [ span [] [text split.name]
      , div []
        [ deltaEl
        , span [style [("margin-left", "8px")]]
          [text <| formatDuration
                <| Maybe.withDefault split.endTime split.currentAttemptEndTime]
        ]
      ]
    
splitStyles : ActiveSplit -> Int -> List (Attribute msg)
splitStyles activeSplit index =
  let
    baseStyles = [ flexRow "center" "space-between", style [("padding", "8px")] ]
  in
    case activeSplit of
      SplitIndex activeIndex ->
        if activeIndex == index then
          style [("background", "#bbb")] :: baseStyles
        else
          baseStyles
      _ ->
        baseStyles


flexRow : String -> String -> Attribute msg
flexRow align justify =
  style [("display", "flex"), ("flex-direction", "row"),
         ("align-items", align), ("justify-content", justify)]
         
         
formatDelta : Time -> String
formatDelta duration =
  let
    sign = (if duration > 0 then "+" else "-")
  in
    sign ++ (formatDuration <| abs duration)

      
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
