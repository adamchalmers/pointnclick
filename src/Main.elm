port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
import Engine exposing (SceneID, Shape(..), World, renderScene)
import GameData
import Graph
import Html exposing (Html, button, div, h1, p, span, text)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { world : Engine.World GameData.State
    , currScene : SceneID
    , serverMessage : String
    , state : GameData.State
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    let
        ( firstScene, newGameState ) =
            GameData.newGame
    in
    ( { currScene = firstScene
      , world = GameData.world
      , serverMessage = "Hello"
      , state = newGameState
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = TestServer
    | OnServerResponse (Result Http.Error String)
    | ChangeScene SceneID
    | ChangeState (GameData.State -> GameData.State)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangeScene id ->
            ( { model | currScene = id }, Cmd.none )

        ChangeState f ->
            ( { model | state = f model.state }, Cmd.none )

        TestServer ->
            let
                expect =
                    Http.expectJson OnServerResponse (Decode.field "result" Decode.string)
            in
            ( model
            , Http.get { url = "/test", expect = expect }
            )

        OnServerResponse res ->
            case res of
                Ok r ->
                    ( { model | serverMessage = r }, Cmd.none )

                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl _ ->
            "BadUrl"

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.header []
            [ h1 [] [ text "ElMyst" ]
            ]
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-3" ] []
            , div [ class "pure-u-1-3" ]
                [ button
                    [ class "pure-button pure-button-primary"
                    , onClick TestServer
                    ]
                    [ text "ping dev server" ]
                , text model.serverMessage
                ]
            ]
        , render model.currScene model.world
        ]


imgNamed filename =
    "images/" ++ filename


render : SceneID -> World GameData.State -> Html Msg
render sceneID world =
    case Graph.getData sceneID world of
        Just sceneData ->
            renderScene ChangeScene ChangeState imgNamed sceneData

        Nothing ->
            p [] [ text <| "No scene #" ++ String.fromInt sceneID ++ " exists" ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
