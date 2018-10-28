module Main exposing (Ball, Control, Direction(..), Game, Model, Moving(..), Msg(..), Paddle, Side(..), aspectRatio, ballAcceleration, ballRadius, boxHeight, boxWidth, defaultViewbox, init, initBallSpeed, main, pad, paddleHeight, paddleWidth, randomSideAndMoving, subscriptions, update, updateBall, updateDone, updatePaddle, updatePaddlePosition, view, viewBall, viewPaddle, viewTime)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes as HtmlAttr
import Keyboard exposing (Key(..), KeyChange(..), KeyParser, RawKey, characterKey)
import Svg exposing (..)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , fillOpacity
        , height
        , preserveAspectRatio
        , r
        , textAnchor
        , viewBox
        , width
        , x
        , y
        )
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Control =
    { up : Key, down : Key }


type Side
    = Left
    | Right


type Moving
    = Up
    | Down


type Direction
    = LeftUp
    | LeftDown
    | RightUp
    | RightDown


type alias Paddle =
    { position : Float
    , side : Side
    , moving : Maybe Moving
    , control : Control
    }


type alias Ball =
    { center : ( Float, Float )
    , side : Maybe Side
    , moving : Maybe Moving
    }


type alias Game =
    { leftPaddle : Paddle
    , rightPaddle : Paddle
    , ball : Ball
    , ballSpeed : Float
    , done : Bool
    , loops : Int
    , seconds : Int
    , seed : Int
    }


type alias Model =
    Game


type Msg
    = KeyDown RawKey
    | KeyUp RawKey
    | Frame Posix
    | Tick Posix
    | Restart Game


aspectRatio : Float
aspectRatio =
    2


pad : Float
pad =
    0


boxHeight : Float
boxHeight =
    100


boxWidth : Float
boxWidth =
    boxHeight * aspectRatio


paddleHeight : Float
paddleHeight =
    boxHeight / 4


paddleWidth : Float
paddleWidth =
    boxWidth / 50


ballRadius : Float
ballRadius =
    1


initBallSpeed : Float
initBallSpeed =
    0.15


ballAcceleration : Float
ballAcceleration =
    0.05


init : () -> ( Model, Cmd Msg )
init =
    let
        paddleY =
            (boxHeight - paddleHeight) / 2

        ballX =
            (boxWidth - ballRadius) / 2

        ballY =
            (boxHeight - ballRadius) / 2

        leftPaddle =
            Paddle paddleY Left Nothing (Control (Keyboard.Character "a") (Keyboard.Character "z"))

        rightPaddle =
            Paddle paddleY Right Nothing (Control (Keyboard.Character "'") (Keyboard.Character "/"))

        ball =
            Ball ( ballX, ballY ) Nothing Nothing
    in
    \_ -> ( Game leftPaddle rightPaddle ball initBallSpeed False 0 0 0, Cmd.none )


randomSideAndMoving : Int -> ( Side, Moving )
randomSideAndMoving seed =
    case modBy 8 seed of
        0 ->
            ( Left, Up )

        1 ->
            ( Left, Up )

        2 ->
            ( Left, Down )

        3 ->
            ( Left, Down )

        4 ->
            ( Right, Up )

        5 ->
            ( Right, Up )

        _ ->
            ( Right, Down )


subscriptions : Model -> Sub Msg
subscriptions game =
    case game.done of
        True ->
            Sub.none

        False ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                , Time.every 2 Frame
                , Time.every 1000 Tick
                ]


updateBall : Msg -> Model -> Ball -> Ball
updateBall msg game ball =
    let
        ( x, y ) =
            ball.center

        minX =
            0

        maxX =
            boxWidth - ballRadius

        minY =
            0

        maxY =
            boxHeight - ballRadius

        nextPosX =
            case msg of
                Frame _ ->
                    case ball.side of
                        Just Left ->
                            max (x - game.ballSpeed) minX

                        Just Right ->
                            min (x + game.ballSpeed) maxX

                        Nothing ->
                            x

                _ ->
                    x

        nextPosY =
            case msg of
                Frame time ->
                    case ball.moving of
                        Just Up ->
                            max (y - game.ballSpeed) minY

                        Just Down ->
                            min (y + game.ballSpeed) maxY

                        Nothing ->
                            y

                _ ->
                    y

        ( side, moving ) =
            randomSideAndMoving game.seed

        nextMoving =
            if y <= 0 + ballRadius then
                Just Down

            else if y >= (boxHeight - ballRadius) then
                Just Up

            else if x <= paddleWidth || x >= boxWidth - paddleWidth then
                Just moving

            else
                case ball.moving of
                    Just _ ->
                        ball.moving

                    Nothing ->
                        if game.seed == 0 then
                            Nothing

                        else
                            Just moving

        betweenLeftPaddle =
            let
                paddleStart =
                    game.leftPaddle.position

                paddleEnd =
                    paddleStart + paddleHeight
            in
            y <= paddleEnd && y >= paddleStart

        betweenRightPaddle =
            let
                paddleStart =
                    game.rightPaddle.position

                paddleEnd =
                    paddleStart + paddleHeight
            in
            y <= paddleEnd && y >= paddleStart

        nextSide =
            if x <= paddleWidth && betweenLeftPaddle then
                Just Right

            else if x >= boxWidth - paddleWidth && betweenRightPaddle then
                Just Left

            else
                case ball.side of
                    Just _ ->
                        ball.side

                    Nothing ->
                        if game.seed == 0 then
                            Nothing

                        else
                            Just side
    in
    { ball
        | center = ( nextPosX, nextPosY )
        , moving = nextMoving
        , side = nextSide
    }


updatePaddlePosition : Paddle -> Paddle
updatePaddlePosition paddle =
    let
        pos =
            paddle.position

        minPos =
            0

        maxPos =
            boxHeight - paddleHeight

        nextPosition =
            case paddle.moving of
                Just Up ->
                    max (pos - 0.5) minPos

                Just Down ->
                    min (pos + 0.5) maxPos

                Nothing ->
                    pos
    in
    { paddle | position = nextPosition }


updatePaddle : Msg -> Paddle -> Paddle
updatePaddle msg paddle =
    case msg of
        KeyDown rawKey ->
            let
                maybeKey =
                    characterKey rawKey
            in
            case maybeKey of
                Just key ->
                    if key == paddle.control.up then
                        { paddle | moving = Just Up }

                    else if key == paddle.control.down then
                        { paddle | moving = Just Down }

                    else
                        paddle

                Nothing ->
                    paddle

        KeyUp rawKey ->
            let
                maybeKey =
                    characterKey rawKey
            in
            case maybeKey of
                Just key ->
                    if key == paddle.control.up || key == paddle.control.down then
                        { paddle | moving = Nothing }

                    else
                        paddle

                Nothing ->
                    paddle

        Frame _ ->
            updatePaddlePosition paddle

        Tick _ ->
            paddle

        Restart _ ->
            paddle


updateDone : Msg -> Ball -> Bool
updateDone msg ball =
    case msg of
        Frame _ ->
            let
                ( x, y ) =
                    ball.center

                leftHitbox =
                    x - 2

                rightHitbox =
                    x + 2

                _ =
                    rightHitbox >= boxWidth
            in
            if leftHitbox <= 0 || rightHitbox >= boxWidth then
                True

            else
                False

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg game =
    let
        nextGame =
            { game
                | leftPaddle = updatePaddle msg game.leftPaddle
                , rightPaddle = updatePaddle msg game.rightPaddle
                , ball = updateBall msg game game.ball
                , ballSpeed =
                    case msg of
                        Tick time ->
                            if modBy 15 game.seconds == 0 then
                                game.ballSpeed + ballAcceleration

                            else
                                game.ballSpeed

                        _ ->
                            game.ballSpeed
                , done = updateDone msg game.ball
                , loops =
                    case msg of
                        Frame _ ->
                            game.loops + 1

                        _ ->
                            game.loops
                , seconds =
                    case msg of
                        Tick time ->
                            game.seconds + 1

                        _ ->
                            game.seconds
                , seed =
                    case msg of
                        Frame posix ->
                            posix |> Time.posixToMillis

                        _ ->
                            game.seed
            }
    in
    ( nextGame, Cmd.none )


view : Model -> Document Msg
view game =
    let
        styles =
            if game.done then
                [ HtmlAttr.style "opacity" "0.5" ]

            else
                []

        gameOverHeading =
            if game.done then
                let
                    gameOverText =
                        case game.ball.side of
                            Just Left ->
                                [ text_
                                    [ x "65"
                                    , y "50"
                                    ]
                                    [ Svg.text "Right wins" ]
                                ]

                            Just Right ->
                                [ text_
                                    [ x "70"
                                    , y "50"
                                    ]
                                    [ Svg.text "Left wins" ]
                                ]

                            Nothing ->
                                []
                in
                gameOverText

            else
                []
    in
    Document "Pong"
        [ Grid.containerFluid []
            [ CDN.stylesheet
            , Grid.row []
                [ Grid.col []
                    [ svg [ defaultViewbox ]
                        (gameOverHeading
                            ++ [ viewPaddle game.done game.leftPaddle
                               , viewBall game.done game.ball
                               , viewPaddle game.done game.rightPaddle
                               ]
                        )
                    ]
                ]
            , Grid.row [ Row.attrs styles ]
                [ Grid.col []
                    [ h1 [ HtmlAttr.align "center" ] [ Html.text "0 | 0" ]
                    , h4 [ HtmlAttr.align "center" ] [ viewTime game.seconds ]
                    ]
                ]
            ]
        ]


viewTime : Int -> Html Msg
viewTime seconds =
    let
        mins =
            seconds // 60 |> String.fromInt

        secs =
            modBy 60 seconds |> String.fromInt |> String.padLeft 2 '0'
    in
    Html.text (mins ++ ":" ++ secs)


viewPaddle : Bool -> Paddle -> Svg Msg
viewPaddle done paddle =
    let
        ( paddleX, paddleColor ) =
            case paddle.side of
                Left ->
                    ( 0, "blue" )

                Right ->
                    ( boxWidth - paddleWidth, "green" )
    in
    rect
        [ paddleX |> String.fromFloat |> x
        , paddle.position |> String.fromFloat |> y
        , paddleWidth |> String.fromFloat |> width
        , paddleHeight |> String.fromFloat |> height
        , fill paddleColor
        , if done then
            fillOpacity "0.5"

          else
            fillOpacity "1"
        ]
        []


viewBall : Bool -> Ball -> Svg Msg
viewBall done ball =
    let
        ( centerX, centerY ) =
            ball.center
    in
    circle
        [ centerX |> String.fromFloat |> cx
        , centerY |> String.fromFloat |> cy
        , r (String.fromFloat ballRadius)
        , fill "orange"
        , if done then
            fillOpacity "0.5"

          else
            fillOpacity "1"
        ]
        []


defaultViewbox : Html.Attribute Msg
defaultViewbox =
    [ pad, pad, boxWidth, boxHeight ]
        |> List.map String.fromFloat
        |> String.join " "
        |> viewBox
