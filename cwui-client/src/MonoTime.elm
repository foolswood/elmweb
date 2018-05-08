module MonoTime exposing (now)

import Native.MonoTime
import Task exposing (Task)

now : Task x Float
now = Native.MonoTime.now
