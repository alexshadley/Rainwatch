import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (map, filter)
import Json.Decode exposing (Decoder, field, string, int, list, andThen)
import Time exposing (Posix, Weekday(..), Zone, utc, toWeekday)
import Iso8601


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { pos: LatLng
  , dataURL: String
  , forecast: Maybe Forecast
  , error: String
  }


type alias Forecast =
  { first: Weekday 
  , mon: Int
  , tue: Int
  , wed: Int
  , thu: Int
  , fri: Int
  , sat: Int
  , sun: Int
  }


maxOfDay : Zone -> Weekday -> List RainProb -> Int
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


build : Zone -> Weekday -> List RainProb -> Forecast
build zone first probs =
  let
    maxOf = maxOfDay zone
  in
    { first = first
    , mon = maxOf Mon probs
    , tue = maxOf Tue probs
    , wed = maxOf Wed probs
    , thu = maxOf Thu probs
    , fri = maxOf Fri probs
    , sat = maxOf Sat probs
    , sun = maxOf Sun probs
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( { pos = lawrence
    , dataURL = ""
    , forecast = Nothing
    , error = ""
    }
  , getNWSPoint lawrence)



-- UPDATE


type Msg
  = GetPoint
  | GotPoint (Result Http.Error String)
  | GotWeather (Result Http.Error (List RainProb))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetPoint ->
      (model, getNWSPoint lawrence)

    GotPoint result ->
      case result of
        Ok url ->
          ({ model | dataURL = url}, getWeatherData url)

        Err _ ->
          ({ model | dataURL = "ERROR!"}, Cmd.none)
    
    GotWeather result ->
      case result of
        Ok probs ->
          let
            newForecast = build utc Mon probs
          in
          ({ model | forecast = Just newForecast}, Cmd.none)
        
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
    [ h2 [] [ text "Rainwatch" ]
    , button [ onClick GetPoint ] [ text "get data!" ]
    , p [] [ text model.dataURL ]
    , p [] [ text ( model.error ) ]
    , viewForecast model.forecast
    ]


viewForecast : Maybe Forecast -> Html Msg
viewForecast fc =
  case fc of
    Just f ->
      div []
        [ p [] [ text ( "Mon: " ++ String.fromInt f.mon ) ]
        , p [] [ text ( "Tue: " ++ String.fromInt f.tue ) ]
        , p [] [ text ( "Wed: " ++ String.fromInt f.wed ) ]
        , p [] [ text ( "Thu: " ++ String.fromInt f.thu ) ]
        , p [] [ text ( "Fri: " ++ String.fromInt f.fri ) ]
        , p [] [ text ( "Sat: " ++ String.fromInt f.sat ) ]
        , p [] [ text ( "Sun: " ++ String.fromInt f.sun ) ]
        ]
    
    Nothing -> div [] []


-- HTTP

type LatLng = Pos Float Float
lawrence = Pos 38.9717 -95.2353

type alias RainProb =
  { time: Posix
  , probability: Int
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
{-
sufixDateDecoder : Decoder Posix
sufixDateDecoder =
    let
        fn str =
            case Iso8601.toTime (String.slice 0 -5 str) of
                Ok value ->
                    Json.succeed value

                Err err ->
                    Json.fail "not a date I can understand"
    in
    Json.andThen fn Json.string

dateDecoder : Decoder Posix
dateDecoder =
    Json.oneOf
        [ sufixDateDecoder
        , Json.succeed (Time.millisToPosix 0)
        ]-}


dateDecoder : Decoder Posix
dateDecoder =
  let
    toTime = \s ->
      case Iso8601.toTime s of
        Ok p -> p
        Err e -> Time.millisToPosix 0
    
    fn = \s -> String.slice 0 -5 s |> toTime

  in
    Json.Decode.map fn string


rainProbDecoder : Decoder RainProb
rainProbDecoder =
  Json.Decode.map2 (\t p -> {time=t,probability=p})
    (field "validTime" dateDecoder)
    (field "value" int)


weatherDecoder : Decoder (List RainProb)
weatherDecoder =
  field "properties" 
    ( field "probabilityOfPrecipitation" 
      ( field "values" 
        ( list rainProbDecoder )
      ) 
    )
