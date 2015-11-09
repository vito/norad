module EventSource where

import Time
import Task
import Native.EventSource

type EventSource = EventSource

type alias Event =
  { lastEventId : Maybe String
  , name : Maybe String
  , data : String
  }

connect : String -> Task.Task x EventSource
connect = Native.EventSource.connect

on : String -> Signal.Address Event -> EventSource -> Task.Task x EventSource
on = Native.EventSource.on

close : EventSource -> Task.Task x ()
close = Native.EventSource.close
