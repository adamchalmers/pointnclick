port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
import Engine exposing (Scene, SceneData, SceneID, Shape(..), Transition, World, attrsOf, getImg, renderScene)
import GameData
import Graph
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode
import Maybe exposing (withDefault)



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { world : Engine.World
    , currScene : SceneID
    , serverMessage : String
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { currScene = flags, world = GameData.world, serverMessage = "Hello" }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = TestServer
    | OnServerResponse (Result Http.Error String)
    | ChangeScene SceneID


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangeScene id ->
            ( { model | currScene = id }, Cmd.none )

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
        (defaultGUI model ++ render model.currScene model.world)


defaultGUI model =
    [ header []
        [ -- img [ src "/images/logo.png" ] []
          span [ class "logo" ] []
        , h1 [] [ text "Elm 0.19 Webpack Starter, with hot-reloading" ]
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
    ]


render : SceneID -> World -> List (Html Msg)
render sceneID world =
    case Graph.getData sceneID world of
        Just sceneData ->
            renderScene ChangeScene sceneData

        Nothing ->
            [ p [] [ text <| "No scene #" ++ String.fromInt sceneID ++ " exists" ] ]



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
