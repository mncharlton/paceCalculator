module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import PaceCalculator exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Test functions"
        [ describe "Calculate Total Seconds"
            [ test "0 in is 0 out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 0 0 0
                        |> Expect.equal 0
            , test "10 seconds in is 10 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 0 0 10
                        |> Expect.equal 10
            , test "1 minute in is 60 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 0 1 0
                        |> Expect.equal 60
            , test "1 hour in is 3600 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 1 0 0
                        |> Expect.equal 3600
            , test "1 minute, 1 second in is 61 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 0 1 1
                        |> Expect.equal 61
            , test "1 hour, 1 second in is 3601 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 1 0 1
                        |> Expect.equal 3601
            , test "1 of each in is 3661 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 1 1 1
                        |> Expect.equal 3661
            , test "2 of each in is 7322 seconds out" <|
                \_ ->
                    PaceCalculator.calculateTotalSeconds 2 2 2
                        |> Expect.equal 7322
            ]
        , describe "Preceding Zero Check"
            [ test "0 in is 00 out" <|
                \_ ->
                    PaceCalculator.precedingZeroCheck 0
                        |> Expect.equal "00"
            , test "5 in is 05 out" <|
                \_ ->
                    PaceCalculator.precedingZeroCheck 5
                        |> Expect.equal "05"
            , test "10 in is 10 out" <|
                \_ ->
                    PaceCalculator.precedingZeroCheck 10
                        |> Expect.equal "10"
            ]
        , describe "Calculate Pace Seconds"
            [ test "5km in 20mins" <|
                \_ ->
                    PaceCalculator.calculatePaceSeconds 5 20
                        |> Expect.equal 4
            , test "10km in 45mins" <|
                \_ ->
                    PaceCalculator.calculatePaceSeconds 10 45
                        |> Expect.within (Expect.Absolute 0.000000001) 4.5
            , test "6km in 36mins" <|
                \_ ->
                    PaceCalculator.calculatePaceSeconds 6 36
                        |> Expect.equal 6
            ]
        , describe "Calculate Hours"
            [ test "3600s is 1 hour" <|
                \_ ->
                    PaceCalculator.calculateHours 3600
                        |> Expect.equal 1
            , test "7000s is 1 hour" <|
                \_ ->
                    PaceCalculator.calculateHours 7000
                        |> Expect.equal 1
            , test "7200s is 2 hours" <|
                \_ ->
                    PaceCalculator.calculateHours 7200
                        |> Expect.equal 2
            , test "3599.9s is 0 hours" <|
                \_ ->
                    PaceCalculator.calculateHours 3599.9
                        |> Expect.equal 0
            , test "3600.1s is 1 hours" <|
                \_ ->
                    PaceCalculator.calculateHours 3600.1
                        |> Expect.equal 1
            ]
        , describe "Calculate Minutes"
            [ test "3600s is 0 minutes" <|
                \_ ->
                    PaceCalculator.calculateMinutes 3600
                        |> Expect.equal 0
            , test "59s is 0 minutes" <|
                \_ ->
                    PaceCalculator.calculateMinutes 59
                        |> Expect.equal 0
            , test "60s is 1 minute" <|
                \_ ->
                    PaceCalculator.calculateMinutes 60
                        |> Expect.equal 1
            , test "4800s is 20minutes" <|
                \_ ->
                    PaceCalculator.calculateMinutes 4800
                        |> Expect.equal 20
            , test "119s is 1 minute" <|
                \_ ->
                    PaceCalculator.calculateMinutes 119
                        |> Expect.equal 1
            ]
        , describe "Calculate Seconds"
            [ test "3600s is 0s" <|
                \_ ->
                    PaceCalculator.calculateSeconds 3600
                        |> Expect.equal 0
            , test "60s is 0s" <|
                \_ ->
                    PaceCalculator.calculateSeconds 60
                        |> Expect.equal 0
            , test "1s is 1s" <|
                \_ ->
                    PaceCalculator.calculateSeconds 1
                        |> Expect.equal 1
            , test "119s is 59s" <|
                \_ ->
                    PaceCalculator.calculateSeconds 119
                        |> Expect.equal 59
            , test "3700s is 40s" <|
                \_ ->
                    PaceCalculator.calculateSeconds 3700
                        |> Expect.equal 40
            , test "45s is 45s" <|
                \_ ->
                    PaceCalculator.calculateSeconds 45
                        |> Expect.equal 45
            ]
        ]
