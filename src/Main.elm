import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, int, list)
import Time
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
  , rainProbs: List Int
  , error: String
  }



init : () -> (Model, Cmd Msg)
init _ =
  ( { pos = lawrence
    , dataURL = ""
    , rainProbs = []
    , error = ""
    }
  , getNWSPoint lawrence)



-- UPDATE


type Msg
  = GetPoint
  | GotPoint (Result Http.Error String)
  | GotWeather (Result Http.Error (List Int))


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
            prob =
              case List.maximum probs of
                Just max -> max
                Nothing  -> 0
          in
          ({ model | rainChance = prob}, Cmd.none)
        
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
    , p [] [ text ( String.fromInt model.rainChance ) ]
    , p [] [ text ( model.error ) ]
    ]


-- HTTP

type LatLng = Pos Float Float
lawrence = Pos 38.9717 -95.2353

type alias RainProb =
  { time: Date
  , probability: Int
  }


getNWSPoint : LatLng -> Cmd Msg
getNWSPoint (Pos lat lng) =
  Http.get
    { url = "https://api.weather.gov/points/" ++ String.fromFloat lat ++ "," ++ String.fromFloat lng
    , expect = Http.expectJson GotPoint pointsDecoder
    }


-- JSON Decoders


date : Decoder Date
date =
    let
        convert : String -> Decoder Date
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    succeed date

                Err error ->
                    fail error
    in
        string |> andThen convert


pointsDecoder : Decoder String
pointsDecoder =
  field "properties" (field "forecastGridData" string)


getWeatherData : String -> Cmd Msg
getWeatherData url =
  Http.get
    { url = url
    , expect = Http.expectJson GotWeather weatherDecoder
    }


rainProbDecoder : Decoder RainProb
  


weatherDecoder : Decoder (List Int)
weatherDecoder =
  field "properties" 
    ( field "probabilityOfPrecipitation" 
      ( field "values" 
        ( list 
          (field "value" int)
        )
      ) 
    )
