module Forecast exposing (Forecast, empty, extend)

import Time exposing (Weekday(..))
import Maybe exposing (withDefault)
import List exposing (head)

import Week

type Forecast =
  Forecast (List (Weekday, Int))

empty : Forecast
empty = Forecast []

extend : Weekday -> Int -> Forecast -> Forecast
extend d p (Forecast fs) =
  Forecast ( [(d, p)] ++ fs )

prob : Forecast -> Weekday -> Int
prob (Forecast fs) day =
  fs
    |> List.filter (\(d,_) -> d == day)
    |> List.map (\(_,p) -> p)
    |> head
    |> withDefault 0