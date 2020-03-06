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
    { milePace : Float
    , kmPace : Float
    , timeHours : Float
    , timeMinutes : Float
    , timeSeconds : Float
    , miles : Float
    , kilometres : Float
    , distanceUnit : String
    }


initialModel : Model
initialModel =
    { milePace = 0
    , kmPace = 0
    , timeHours = 0
    , timeMinutes = 0
    , timeSeconds = 0
    , miles = 0
    , kilometres = 0
    , distanceUnit = "km"
    }



-- UPDATE


type Msg
    = ChangeDistance String
    | ChangeHours String
    | ChangeMinutes String
    | ChangeSeconds String
    | ChangeDistanceUnit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDistance string ->
            let
                newKilometres =
                    if model.distanceUnit == "km" then
                        parseFloat string

                    else
                        parseFloat string * 1.609341

                newMiles =
                    if model.distanceUnit == "km" then
                        parseFloat string * 0.621371

                    else
                        parseFloat string

                newMilePace =
                    calculatePaceSeconds newMiles (calculateTotalSeconds model.timeHours model.timeMinutes model.timeSeconds)

                newKmPace =
                    calculatePaceSeconds newKilometres (calculateTotalSeconds model.timeHours model.timeMinutes model.timeSeconds)
            in
            { model
                | kilometres = newKilometres
                , miles = newMiles
                , milePace = newMilePace
                , kmPace = newKmPace
            }

        ChangeHours string ->
            let
                newMilePace =
                    calculatePaceSeconds model.miles (calculateTotalSeconds (parseFloat string) model.timeMinutes model.timeSeconds)

                newKmPace =
                    calculatePaceSeconds model.kilometres (calculateTotalSeconds (parseFloat string) model.timeMinutes model.timeSeconds)
            in
            { model
                | milePace = newMilePace
                , kmPace = newKmPace
                , timeHours = parseFloat string
            }

        ChangeMinutes string ->
            let
                newMilePace =
                    calculatePaceSeconds model.miles (calculateTotalSeconds model.timeHours (parseFloat string) model.timeSeconds)

                newKmPace =
                    calculatePaceSeconds model.kilometres (calculateTotalSeconds model.timeHours (parseFloat string) model.timeSeconds)
            in
            { model
                | milePace = newMilePace
                , kmPace = newKmPace
                , timeMinutes = parseFloat string
            }

        ChangeSeconds string ->
            let
                newMilePace =
                    calculatePaceSeconds model.miles (calculateTotalSeconds model.timeHours model.timeMinutes (parseFloat string))

                newKmPace =
                    calculatePaceSeconds model.kilometres (calculateTotalSeconds model.timeHours model.timeMinutes (parseFloat string))
            in
            { model
                | milePace = newMilePace
                , kmPace = newKmPace
                , timeSeconds = parseFloat string
            }

        ChangeDistanceUnit string ->
            let
                newMiles =
                    if model.distanceUnit == "km" then
                        model.kilometres

                    else
                        model.miles * 0.621371

                newKilometres =
                    if model.distanceUnit == "km" then
                        model.kilometres * 1.609341

                    else
                        model.miles

                newMilePace =
                    calculatePaceSeconds newMiles (calculateTotalSeconds model.timeHours model.timeMinutes (parseFloat string))

                newKmPace =
                    calculatePaceSeconds newKilometres (calculateTotalSeconds model.timeHours model.timeMinutes (parseFloat string))
            in
            { model
                | distanceUnit = string
                , miles = newMiles
                , kilometres = newKilometres
                , milePace = newMilePace
                , kmPace = newKmPace
            }



-- VIEW


viewResults : Float -> Float -> Float -> Html msg
viewResults distance kmPace milePace =
    if distance > 0 && kmPace > 0 then
        span []
            [ h2 [] [ text "Pace" ]
            , p [ Attr.id "output" ] [ text (precedingZeroCheck (calculateHours kmPace) ++ ":" ++ precedingZeroCheck (calculateMinutes kmPace) ++ ":" ++ precedingZeroCheck (calculateSeconds kmPace) ++ " per km") ]
            , p [ Attr.id "output" ] [ text (precedingZeroCheck (calculateHours milePace) ++ ":" ++ precedingZeroCheck (calculateMinutes milePace) ++ ":" ++ precedingZeroCheck (calculateSeconds milePace) ++ " per mile") ]
            ]

    else
        p [] [ text "Enter details above to work out your pace!" ]


view : Model -> Html Msg
view model =
    div [ Attr.class "background" ]
        [ div [ Attr.class "calculator" ]
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
            , select [ Attr.id "distanceUnit", onInput ChangeDistanceUnit ]
                [ option [ Attr.value "km" ] [ text "km" ]
                , option [ Attr.value "mile" ] [ text "miles" ]
                ]
            , viewResults model.kilometres model.kmPace model.milePace
            ]
        , footer [ Attr.class "footer" ]
            [ p [ Attr.id "footerText" ] [ text "Pace Calculator by Matt Charlton" ]
            , p [ Attr.id "photoCredit" ] [ text "Photo by Andrea Leopardi on Unsplash" ]
            ]
        ]
