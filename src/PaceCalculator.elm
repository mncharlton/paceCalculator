module PaceCalculator exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = initialModel, update = update, view = view }



-- HELPERS


parseFloat : String -> Float
parseFloat string =
    Maybe.withDefault 0 (String.toFloat string)


precedingZeroCheck : Int -> String
precedingZeroCheck value =
    if value < 10 then
        "0" ++ String.fromInt value

    else
        String.fromInt value


calculateTotalSeconds : Float -> Float -> Float -> Float
calculateTotalSeconds hours minutes seconds =
    (hours * 60 * 60) + (minutes * 60) + seconds


calculatePaceSeconds : Float -> Float -> Float
calculatePaceSeconds distance totalTime =
    totalTime / distance


calculateHours : Float -> Int
calculateHours seconds =
    floor (seconds / 60 / 60)


calculateMinutes : Float -> Int
calculateMinutes seconds =
    floor ((seconds - toFloat (calculateHours seconds * 60 * 60)) / 60)


calculateSeconds : Float -> Int
calculateSeconds seconds =
    floor
        (seconds - toFloat (calculateHours seconds * 60 * 60) - toFloat (calculateMinutes seconds * 60))



-- MODEL


type alias Model =
    { totalSeconds : Float
    , timeHours : Float
    , timeMinutes : Float
    , timeSeconds : Float
    , distance : Float
    }


initialModel : Model
initialModel =
    { totalSeconds = 0
    , timeHours = 0
    , timeMinutes = 0
    , timeSeconds = 0
    , distance = 0
    }



-- UPDATE


type Msg
    = ChangeDistance String
    | ChangeHours String
    | ChangeMinutes String
    | ChangeSeconds String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDistance string ->
            { model | distance = parseFloat string, totalSeconds = calculatePaceSeconds (parseFloat string) (calculateTotalSeconds model.timeHours model.timeMinutes model.timeSeconds) }

        ChangeHours string ->
            { model | timeHours = parseFloat string, totalSeconds = calculatePaceSeconds model.distance (calculateTotalSeconds (parseFloat string) model.timeMinutes model.timeSeconds) }

        ChangeMinutes string ->
            { model | timeMinutes = parseFloat string, totalSeconds = calculatePaceSeconds model.distance (calculateTotalSeconds model.timeHours (parseFloat string) model.timeSeconds) }

        ChangeSeconds string ->
            { model | timeSeconds = parseFloat string, totalSeconds = calculatePaceSeconds model.distance (calculateTotalSeconds model.timeHours model.timeMinutes (parseFloat string)) }



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "calculator" ]
        [ h1 [] [ text "Pace Calculator" ]
        , p [] [ text "Enter the details below to calculate your pace" ]
        , h2 [] [ text "Time" ]
        , label [ Attr.for "hours" ] [ text "Hours" ]
        , input [ Attr.type_ "number", Attr.name "hours", Attr.placeholder "00", Attr.min "0", Attr.max "99", onInput ChangeHours ] []
        , label [ Attr.for "minutes" ] [ text "Minutes" ]
        , input [ Attr.type_ "number", Attr.name "minutes", Attr.placeholder "00", Attr.min "0", Attr.max "59", onInput ChangeMinutes ] []
        , label [ Attr.for "seconds" ] [ text "Seconds" ]
        , input [ Attr.type_ "number", Attr.name "seconds", Attr.placeholder "00", Attr.min "0", Attr.max "59", onInput ChangeSeconds ] []
        , h2 [] [ text "Distance" ]
        , input [ Attr.type_ "number", Attr.placeholder "Distance", onInput ChangeDistance ] []
        , p [ Attr.id "output" ] [ text ("Pace : " ++ precedingZeroCheck (calculateHours model.totalSeconds) ++ ":" ++ precedingZeroCheck (calculateMinutes model.totalSeconds) ++ ":" ++ precedingZeroCheck (calculateSeconds model.totalSeconds)) ]
        ]
