module Forecast exposing (Forecast, empty, extend, dumpWeek)

import Time exposing (Weekday(..))
import Maybe exposing (withDefault)
import List exposing (head)

import Week

type Forecast =
  Forecast (List (Weekday, Float))

empty : Forecast
empty = Forecast []

extend : Weekday -> Float -> Forecast -> Forecast
extend d p (Forecast fs) =
  Forecast ( [(d, p)] ++ fs )

prob : Forecast -> Weekday -> Float
prob (Forecast fs) day =
  fs
    |> List.filter (\(d,_) -> d == day)
    |> List.map (\(_,p) -> p)
    |> head
    |> withDefault 0
  
dumpWeek : Weekday -> Forecast -> List (Weekday, Float)
dumpWeek today f =
  let
    days = Week.daysFrom today
  in
    List.map (\d -> (d, prob f d) ) days