module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Dom.Scroll exposing (..)
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Message =
    { text : String
    , sender : Sender
    }


type alias Model =
    { messages : List Message
    , input : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [ initialMessage ] "", Cmd.none )


initialMessage : Message
initialMessage =
    Message "Try typing a message in the box and hitting the Send button!" Them



-- Update


type Msg
    = NewMessage String
    | Send
    | Input String
    | NoOp


type Sender
    = Us
    | Them


scrollToBottom : Cmd Msg
scrollToBottom =
    Task.attempt (always NoOp) <| toBottom "messages"


sendMessage : Msg -> Model -> ( Model, Cmd Msg )
sendMessage msg model =
    case model.input of
        "" ->
            ( model, Cmd.none )

        input ->
            ( Model
                (model.messages ++ [ Message input Us ])
                ""
            , WebSocket.send echoServer input
            )


addMessage : String -> Model -> ( Model, Cmd Msg )
addMessage msg model =
    ( Model
        (model.messages ++ [ Message msg Them ])
        model.input
    , scrollToBottom
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( Model model.messages input, Cmd.none )

        Send ->
            sendMessage msg model

        NewMessage str ->
            addMessage str model

        NoOp ->
            ( model, Cmd.none )



-- Subscriptions


echoServer : String
echoServer =
    "ws://echo.websocket.org"


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen echoServer NewMessage



-- View


heading : Html msg
heading =
    h1 [] [ text "Elm Chat." ]


messages : Model -> Html msg
messages model =
    div [ id "messages" ] <| List.map message model.messages


message : Message -> Html msg
message msg =
    let
        usOrThem =
            case msg.sender of
                Us ->
                    "sent-by-us"

                Them ->
                    "sent-by-them"
    in
        div [ class "message-container" ]
            [ blockquote [ class usOrThem ] [ text msg.text ] ]


sendForm : Model -> Html Msg
sendForm model =
    footer []
        [ Html.form [ onSubmit Send ]
            [ node "textarea" [ onInput Input, value model.input, autofocus True, spellcheck False, placeholder "Type your message here..." ] []
            , sendButton
            ]
        ]


sendButton : Html msg
sendButton =
    button [ id "send-button", type_ "submit" ]
        [ i [ class "fa fa-paper-plane" ] []
        ]


paragraphs : Html msg
paragraphs =
    article [] []


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ meta
        , styles
        , heading
        , messages model
        , sendForm model
        ]


link : List (Attribute msg) -> List (Html msg) -> Html msg
link =
    node "link"


stylesheets : List String
stylesheets =
    [ "https://cdnjs.cloudflare.com/ajax/libs/normalize/7.0.0/normalize.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
    , "static/css/fa/css/font-awesome.min.css"
    , "static/css/style.css"
    ]


scriptSources : List String
scriptSources =
    [ "https://use.fontawesome.com/b65b90e8dd.js"
    ]


stylesheet : String -> Html msg
stylesheet url =
    link [ rel "stylesheet", href url ] []


script : String -> Html msg
script url =
    node "script" [ src url ] []


styles : Html msg
styles =
    div [] (List.map stylesheet stylesheets)


scripts : Html msg
scripts =
    div [] (List.map script scriptSources)


meta : Html msg
meta =
     Html.node "meta" [ name "viewport", content "width=device-width, initial-scale=1" ] []
