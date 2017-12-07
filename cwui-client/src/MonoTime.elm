module MonoTime exposing (now)

import Native.MonoTime
import Task exposing (Task)
import Time exposing (Time)

now : Task x Time
now = Native.MonoTime.now
