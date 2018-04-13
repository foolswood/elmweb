module TimeSeriesDiff exposing (changedRegions, ChangedTimes)

import Dict exposing (Dict)
import Set exposing (Set)

import ClTypes exposing (Time, TpId)
import TimeSeries exposing (TimeSeries)

-- The idea of this is to make a thing that contains "active" and "passive"
-- regions of time.
type alias ChangedTimes = Dict Time (Maybe Time)

addTimeSlice : Time -> Maybe Time -> ChangedTimes -> ChangedTimes
addTimeSlice sliceStart sliceEnd cts =
  let
    before ma mb = case ma of
        Nothing -> False
        Just a -> case mb of
            Nothing -> False
            Just b -> a < b
    mMax ma mb = if before ma mb
        then mb
        else ma
    foldylocks rStart rEnd (start, end, result) =
        if before rEnd (Just start) || before end (Just rStart)
            then (start, end, (rStart, rEnd) :: result)
            else (min rStart start, mMax rEnd end, result)
    (newStart, newEnd, kept) = Dict.foldl foldylocks (sliceStart, sliceEnd, []) cts
  in Dict.fromList <| (newStart, newEnd) :: kept

changedRegions : Set TpId -> TimeSeries a -> TimeSeries b -> ChangedTimes
changedRegions touched a b =
  let
    foldylocks t tpid _ (touchStart, prevTouched, cts) = if Set.member tpid touched
        then (touchStart, True, cts)
        else if prevTouched
            then (t, False, addTimeSlice touchStart (Just t) cts)
            else (t, False, cts)
    addTimesFor x cts =
      let
        (finalTouchStarted, finalTouched, partialCts) = TimeSeries.fold foldylocks ((0, 0), False, cts) x
      in if finalTouched
        then addTimeSlice finalTouchStarted Nothing partialCts
        else partialCts
  in addTimesFor a <| addTimesFor b Dict.empty
