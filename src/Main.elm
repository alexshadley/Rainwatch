import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Svg.Styled
import Task
import Http
import List exposing (map, filter)
import Json.Decode exposing (Decoder, field, string, float, list, andThen)
import Time exposing (Posix, Weekday(..), Zone, utc, toWeekday)
import Iso8601

import Week
import Forecast exposing (Forecast)
import Rainchart exposing (build)
import RainCss


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view >> Html.Styled.toUnstyled
    }



-- MODEL


type alias Model =
  { pos: LatLng
  , dataURL: String
  , today: Weekday
  , timeZone: Zone
  , forecast: Forecast
  , error: String
  }




init : () -> (Model, Cmd Msg)
init _ =
  ( { pos = lawrence
    , dataURL = ""
    , today = Fri
    , timeZone = utc
    , forecast = Forecast.empty
    , error = ""
    }
  , getNWSPoint lawrence)


maxOfDay : Zone -> Weekday -> List RainProb -> Float
maxOfDay zone day probs =
  let
    max =
      probs
        |> filter ( \rp -> toWeekday zone rp.time == day )
        |> map ( \rp -> rp.probability )
        |> List.maximum
  in
    case max of
      Just v  -> v
      Nothing -> 0


build : Zone -> List RainProb -> Forecast
build zone probs =
  let
    maxOf = maxOfDay zone
    foldFn day fc =
      Forecast.extend day (maxOf day probs) fc

  in
    List.foldl foldFn Forecast.empty Week.days


-- UPDATE


type Msg
  = GotTimeAndZone Posix Zone
  | GetPoint
  | GotPoint (Result Http.Error String)
  | GotWeather (Result Http.Error (List RainProb))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotTimeAndZone time zone ->
      ({model | today = Time.toWeekday zone time, timeZone = zone }, getWeatherData model.dataURL)

    GetPoint ->
      (model, getNWSPoint lawrence)

    GotPoint result ->
      case result of
        Ok url ->
          ({ model | dataURL = url}, getTime)

        Err _ ->
          ({ model | dataURL = "ERROR!"}, Cmd.none)
    
    GotWeather result ->
      case result of
        Ok probs ->
          let
            newForecast = build model.timeZone probs
          in
          ({ model | forecast = newForecast}, Cmd.none)
        
        Err e ->
          ({ model | error = (Debug.toString e) }, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ node "link" [ href "https://fonts.googleapis.com/css?family=Montserrat", rel "stylesheet" ] []
    , h1 [css [RainCss.title]] [ text "Rainwatch" ]
    , button [ onClick GetPoint ] [ text "get data!" ]
    , p [] [ text model.dataURL ]
    , p [] [ text ( model.error ) ]
    , Svg.Styled.fromUnstyled ( Rainchart.build model.today model.forecast )
    ]


-- some funky manuvering here to get 2 task results into one message
getTime : Cmd Msg
getTime =
  Task.map2 (\t z -> (t, z)) Time.now Time.here
    |> Task.perform (\(t, z) -> GotTimeAndZone t z)

-- HTTP

type LatLng = Pos Float Float
lawrence = Pos 38.9717 -95.2353

type alias RainProb =
  { time: Posix
  , probability: Float
  }


getNWSPoint : LatLng -> Cmd Msg
getNWSPoint (Pos lat lng) =
  Http.get
    { url = "https://api.weather.gov/points/" ++ String.fromFloat lat ++ "," ++ String.fromFloat lng
    , expect = Http.expectJson GotPoint pointsDecoder
    }


getWeatherData : String -> Cmd Msg
getWeatherData url =
  Http.get
    { url = url
    , expect = Http.expectJson GotWeather weatherDecoder
    }

-- JSON Decoders


pointsDecoder : Decoder String
pointsDecoder =
  field "properties" (field "forecastGridData" string)


-- from the slack <3
dateDecoder : Decoder Posix
dateDecoder =
  let
    clean str =
      str |> String.split "/" |> List.head

    fn str =
      case clean str of
        Just s ->
          case Iso8601.toTime s of
            Ok value ->
              Json.Decode.succeed value

            Err err ->
              Json.Decode.fail "Not a date I can understand"
        
        Nothing ->
          Json.Decode.fail "Not a date I can understand"
  in
    Json.Decode.andThen fn Json.Decode.string


rainProbDecoder : Decoder RainProb
rainProbDecoder =
  Json.Decode.map2 (\t p -> {time=t,probability=p})
    (field "validTime" dateDecoder)
    (field "value" float)


weatherDecoder : Decoder (List RainProb)
weatherDecoder =
  field "properties" 
    ( field "probabilityOfPrecipitation" 
      ( field "values" 
        ( list rainProbDecoder )
      ) 
    )
