module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Svg exposing (..)
import Svg.Attributes exposing (width, viewBox, cx, cy, r, fill, textAnchor, dominantBaseline, fontSize, x, y, x1, y1, x2, y2, stroke, strokeWidth)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            Time.toHour model.zone model.time

        minute =
            Time.toMinute model.zone model.time

        second =
            Time.toSecond model.zone model.time

        angleSecond =
            toFloat <| 6 * second

        angleMinute =
            toFloat <| 6 * minute

        angleHour =
            -- 下記は(toFloat <| 30 * hourNow) + (angleMinute / 12) と同じ
            (+) (angleMinute / 12) <| (toFloat <| 30 * hour)

        handSecondX =
            String.fromFloat (50 + 38 * sin (angleSecond * pi / 180))

        handSecondY =
            String.fromFloat (50 - 38 * cos (angleSecond * pi / 180))

        handMinuteX =
            String.fromFloat (50 + 32 * sin (angleMinute * pi / 180))

        handMinuteY =
            String.fromFloat (50 - 32 * cos (angleMinute * pi / 180))

        handHourX =
            String.fromFloat (50 + 25 * sin (angleHour * pi / 180))

        handHourY =
            String.fromFloat (50 - 25 * cos (angleHour * pi / 180))
    in
    div []
        [ nav [ class "navbar has-background-info is-fixed-top" ]
            [ div [ class "navbar-brand" ]
                [ Html.a [ class "navbar-item has-text-white" ]
                    [ Html.text "Elm Bulma Clock" ]
                ]
            ]
        , div [ class "section" ]
            [ div [ class "container" ]
                [ svg [ width "300px", viewBox "0 0 100 100" ]
                    [ circle [ cx "50", cy "50", r "45", fill "#2a2a2a" ] []
                    , circle [ cx "50", cy "50", r "40", fill "#f6f6f6" ] []
                    , g [ textAnchor "middle", dominantBaseline "central", fontSize "9" ]
                        [ Svg.text_ [ x "50", y "15" ] [ Svg.text "12" ]
                        , Svg.text_ [ x "67.5", y "19.69" ] [ Svg.text "1" ]
                        , Svg.text_ [ x "80.31", y "32.5" ] [ Svg.text "2" ]
                        , Svg.text_ [ x "85", y "50" ] [ Svg.text "3" ]
                        , Svg.text_ [ x "80.31", y "69.69" ] [ Svg.text "4" ]
                        , Svg.text_ [ x "67.5", y "80.31" ] [ Svg.text "5" ]
                        , Svg.text_ [ x "50", y "85" ] [ Svg.text "6" ]
                        , Svg.text_ [ x "32.5", y "80.31" ] [ Svg.text "7" ]
                        , Svg.text_ [ x "19.69", y "69.69" ] [ Svg.text "8" ]
                        , Svg.text_ [ x "15", y "50" ] [ Svg.text "9" ]
                        , Svg.text_ [ x "19.69", y "32.5" ] [ Svg.text "10" ]
                        , Svg.text_ [ x "32.5", y "19.69" ] [ Svg.text "11" ]
                        ]
                    , line [ x1 "50", y1 "50", x2 handSecondX, y2 handSecondY, stroke "#2a2a2a", strokeWidth "0.5" ] []
                    , line [ x1 "50", y1 "50", x2 handMinuteX, y2 handMinuteY, stroke "#2a2a2a", strokeWidth "1" ] []
                    , line [ x1 "50", y1 "50", x2 handHourX, y2 handHourY, stroke "#2a2a2a", strokeWidth "2" ] []
                    ]

                -- 以下は時間のデバッグ用に表示
                , h1 [] [ Html.text (String.fromInt hour ++ ":" ++ String.fromInt minute ++ ":" ++ String.fromInt second) ]
                ]
            ]
        , footer [ class "footer" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ Html.a [ Html.Attributes.href "http://i-doctor.sakura.ne.jp/font/?p=39013" ] [ Html.text "WordPressでフリーオリジナルフォント2" ]
                    ]
                ]
            ]
        ]
