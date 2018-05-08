module MonoTime exposing (now, rightNow)

import Native.MonoTime
import Task exposing (Task)

now : Task x Float
now = Native.MonoTime.now

-- This is most unelmlike, but having to go all the way around the loop to get
-- the current time is a PITA
rightNow : () -> Float
rightNow = Native.MonoTime.rightNow
