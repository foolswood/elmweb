module TransportClockView exposing (..)

import Html as H exposing (Html)
import Html.Events as HE

import TransportTracker exposing (Transport, TransportLoadError, TransportState(..))
import ClTypes exposing (Time, unbounded)
import Form exposing (FormState(..), AtomState(..))
import EditTypes exposing (asPartialTime, asFullTime, EditEvent(..), PartialTime)
import TupleViews

transportClockView : Result TransportLoadError Transport -> FormState PartialTime -> Html (EditEvent PartialTime Time)
transportClockView transp fsTime = case transp of
    Err msg -> H.text <| toString msg
    Ok {pos, state} -> case state of
        TransportRolling -> TupleViews.timeViewer unbounded pos
        TransportStopped ->
          let
            {editing, pt} = case fsTime of
                FsViewing -> {editing = False, pt = asPartialTime unbounded <| Just pos}
                FsEditing pt -> {editing = True, pt = pt}
            timeEdit = H.map EeUpdate <| TupleViews.timeEditor unbounded <| if editing
                then AsEditing pt
                else AsViewing pos pt
            contents = case asFullTime unbounded pt of
                Nothing -> [timeEdit]
                Just t -> if t == pos
                    then [timeEdit]
                    else [timeEdit, H.button [HE.onClick <| EeSubmit t] [H.text "Set"]]
          in H.div [] contents
