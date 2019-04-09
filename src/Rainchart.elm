module Rainchart exposing (build)

import Time exposing (Weekday(..))

import Axis
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))

import Forecast exposing (Forecast)


weekdayToString : Weekday -> String
weekdayToString d =
  case d of
    Mon -> "Mon"
    Tue -> "Tue"
    Wed -> "Wed"
    Thu -> "Thu"
    Fri -> "Fri"
    Sat -> "Sat"
    Sun -> "Sun"


w : Float
w = 900


h : Float
h = 450


padding : Float
padding = 30


xScale : List (Weekday, Float) -> BandScale Weekday
xScale model =
  List.map Tuple.first model
    |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : ContinuousScale Float
yScale =
  Scale.linear ( h - 2 * padding, 0 ) ( 0, 100 )


xAxis : List (Weekday, Float) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable weekdayToString (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


column : BandScale Weekday -> (Weekday, Float) -> Svg msg
column scale (day, value) =
  g [ class [ "column" ] ]
    [ rect
      [ x <| Scale.convert scale day
      , y <| Scale.convert yScale value
      , width <| Scale.bandwidth scale
      , height <| h - Scale.convert yScale value - 2 * padding
      ]
      []
    , text_
      [ x <| Scale.convert (Scale.toRenderable weekdayToString scale) day
      , y <| Scale.convert yScale value - 5
      , textAnchor AnchorMiddle
      ]
      [ text <| String.fromFloat value ]
    ]


build : Weekday -> Forecast -> Svg msg
build today fc =
  let
    model = Forecast.dumpWeek today fc
  in
    svg [ viewBox 0 0 w h ]
      [ style [] [ text """
        .column rect { fill: rgba(71, 120, 197, 0.8); }
        .column text { display: none; }
        .column:hover rect { fill: rgb(71, 120, 197); }
        .column:hover text { display: inline; }
      """ ]
      , g [ transform [ Translate (padding - 1) (h - padding) ] ]
        [ xAxis model ]
      , g [ transform [ Translate (padding - 1) padding ] ]
        [ yAxis ]
      , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
        List.map (column (xScale model)) model
      ]