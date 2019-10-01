import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getWttr)

type Msg
  = GotWttr (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotWttr result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div [ class "mainText" ]
    [ viewWttr model ]

viewWttr : Model -> Html Msg
viewWttr model =
  case model of
    Failure ->
      div [ class "mainText" ]
        [ text "Could not load weather." ]
    Loading ->
      text "Loading..."

    Success url ->
      div []
        [ text (String.fromFloat(jsonDecoder url)) ]

getWttr : Cmd Msg
getWttr =
  Http.get {
    url = ("http://api.openweathermap.org/data/2.5/forecast?id=3161733&APPID=" ++ getAPIKey)
    , expect = Http.expectString GotWttr
    }


--getAPIKey : String
getAPIKey =
    "bd4001045f14f9f74d66293492e32130"

jsonDecoder : String -> Float
jsonDecoder js =
    case decodeString (at ["list", "0", "rain", "3h"] float) js of
        Ok val ->
            val
        Err _ ->
            10



