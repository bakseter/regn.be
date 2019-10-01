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
        [ {- div [ class "typeText" ] [ text (getType url) ],
          div [ class "descText" ] [ text (getDesc url) ],
          div [ class "tempText" ] [ text ((getTemp url) ++ " " ++ String.fromChar (Char.fromCode 176) ++ "C") ],
          div [ class "speedText" ] [ text ((getSpeed url) ++ " m/s") ], -}
          div [ class "rainUnicode" ] [ text (Tuple.first (getRainStr (getRain url))) ],
          div [ class "rainText" ] [ text (Tuple.second (getRainStr (getRain url))) ]
        ]


getRainStr : Float -> (String, String)
getRainStr mm =
            if mm == 0 then
                (String.fromChar (Char.fromCode 0x2600), "Nei.")
            else if mm < 1.0 then
                (String.fromChar (Char.fromCode 0x1F327), "Ja, men bare litt.")
            else if mm < 1.5 && mm > 1.0 then
                (String.fromChar (Char.fromCode 0x1F327), "Ja, en del.")
            else if mm < 2.0 && mm > 1.5 then
                (String.fromChar (Char.fromCode 0x1F327), "Ja, ganske mye.")
            else if mm < 3.0 && mm > 2.0 then
                (String.fromChar (Char.fromCode 0x1F327), "Ja, VELDIG mye.")
            else if mm < 5.0 && mm > 3.0 then
                (String.fromChar (Char.fromCode 0x26C8), "Jeg ville holdt meg inne om jeg var deg.")
            else
                (String.fromChar (Char.fromCode 0x1F32A), "Hva skal man egentlig ute?")

getWttr : Cmd Msg
getWttr =
  Http.get {
    url = ("http://api.openweathermap.org/data/2.5/forecast?id=3161733&APPID=" ++ getAPIKey)
    , expect = Http.expectString GotWttr
    }


--getAPIKey : String
getAPIKey =
    "bd4001045f14f9f74d66293492e32130"

--- main, description, temp, speed, 3h
getType : String -> String
getType js =
    case decodeString (at ["list", "0", "weather", "0", "main"] string) js of
        Ok val ->
            val
        Err _ ->
            "error"

getDesc : String -> String
getDesc js =
    case decodeString (at ["list", "0", "weather", "0", "description"] string) js of
        Ok val ->
            val
        Err _ ->
            "error"

getTemp : String -> String 
getTemp js =
    case decodeString (at ["list", "0", "main", "temp"] float) js of
        Ok val ->
            String.slice 0 3 (String.fromFloat (val - 273.15))
        Err _ ->
            "error"

getSpeed : String -> String
getSpeed js =
    case decodeString (at ["list", "0", "wind", "speed"] float) js of
        Ok val ->
            String.slice 0 3 (String.fromFloat val)
        Err _ ->
            "error"

getRain : String -> Float
getRain js =
    case decodeString (at ["list", "0", "rain", "3h"] float) js of
        Ok val ->
            val
        Err _ ->
            -1

