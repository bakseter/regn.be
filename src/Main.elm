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
  = MorePlease
  | GotWttr (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getWttr)

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
  div []
    [ h1 [ style "text-align" "center" ] [ text "Rain" ]
    , viewWttr model
    ]


viewWttr : Model -> Html Msg
viewWttr model =
  case model of
    Failure ->
      div []
        [ text "Could not load weather."
        , button [ onClick MorePlease, style "display" "block" ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success url ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , p [] [ text (String.fromFloat(jsonDecoder url)) ]
        ]

getWttr : Cmd Msg
getWttr =
  Http.get
    { --url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
    url = "http://api.openweathermap.org/data/2.5/forecast?id=3161733&APPID=bd4001045f14f9f74d66293492e32130"
    , expect = Http.expectString GotWttr
    }


jsonDecoder : String -> Float
jsonDecoder js =
    case decodeString (at ["list", "0", "rain", "3h"] float) js of
        Ok val ->
            val
        Err _ ->
            10



