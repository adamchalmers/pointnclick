module Engine exposing (Scene, SceneData, SceneID, Shape(..), Transition, World, makeWorld, renderHeight, renderScene, renderWidth)

import Graph exposing (Graph, empty, insertData, insertEdgeData)
import Html exposing (div)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import List exposing (foldr, intersperse)
import String
import Tuple exposing (first, second)



-- ---------------------------
-- Data definitions
-- ---------------------------


type alias SceneID =
    Int


type alias Scene a =
    { id : SceneID
    , data : SceneData a
    }


type alias SceneData a =
    { img : String
    , targets : List (Target a)
    , transitions : List Transition
    , description : String
    }


type alias Target a =
    { shape : Shape
    , action : a -> a
    , img : String
    , topLeft : ( Int, Int )
    , dimensions : Maybe ( Int, Int )
    , description : String
    }


type Shape
    = Circle { x : Int, y : Int, radius : Int }
    | Rect { topLeft : ( Int, Int ), bottomRight : ( Int, Int ) }


type alias Transition =
    { data : TransitionData
    , to : SceneID
    }


type alias TransitionData =
    { shape : Shape }


type alias World a =
    -- Remember Graph is type (Graph nodeID nodeData edgeData)
    Graph Int (SceneData a) TransitionData



-- ---------------------------
-- Rendering
-- ---------------------------


renderWidth =
    800


renderHeight =
    600


renderScene : (SceneID -> msg) -> ((state -> state) -> msg) -> (String -> String) -> SceneData state -> Html.Html msg
renderScene transitionMsg stateMsg locateImage sceneData =
    -- Returns an <img> displaying the scene and a <map> that activates transitions when clicked.
    let
        transitionToArea : Transition -> Html.Html msg
        transitionToArea t =
            area <| onClick (transitionMsg t.to) :: attrsOf t.data.shape

        targetToArea : Target state -> Html.Html msg
        targetToArea t =
            area <| onClick (stateMsg t.action) :: attrsOf t.shape

        targetToImg : Target state -> Html.Html msg
        targetToImg t =
            let
                dimensionAttrs =
                    case t.dimensions of
                        Nothing ->
                            []

                        Just ( w, h ) ->
                            [ Attrs.width w
                            , Attrs.height h
                            ]

                mainAttrs =
                    [ Attrs.src (locateImage t.img)
                    , Attrs.alt t.description
                    ]
            in
            Html.img (mainAttrs ++ dimensionAttrs) []

        areas =
            List.concat
                [ List.map transitionToArea sceneData.transitions
                , List.map targetToArea sceneData.targets
                ]

        area attrs =
            Html.node "area" attrs []

        sceneImg =
            Html.img
                [ Attrs.class "scene"
                , Attrs.usemap "#scene"
                , Attrs.src (locateImage sceneData.img)
                , Attrs.alt sceneData.description
                , Attrs.width renderWidth
                , Attrs.height renderHeight
                ]
                []

        targetImgs =
            List.map targetToImg sceneData.targets
    in
    div
        [ Attrs.width renderWidth
        , Attrs.height renderHeight
        ]
    <|
        List.concat
            [ targetImgs
            , [ Html.node "map" [ Attrs.name "scene" ] areas ]
            , [ sceneImg ]
            ]


attrsOf : Shape -> List (Html.Attribute msg)
attrsOf s =
    case s of
        Circle _ ->
            [ Attrs.shape "circle"
            ]

        Rect { topLeft, bottomRight } ->
            let
                ( x1, y1 ) =
                    topLeft

                ( x2, y2 ) =
                    bottomRight

                fourCoords =
                    [ String.fromInt x1, String.fromInt y1, String.fromInt x2, String.fromInt y2 ]

                coordStr =
                    fourCoords |> intersperse "," |> String.concat
            in
            [ Attrs.shape "rect"
            , Attrs.coords coordStr
            ]



-- ---------------------------
-- Constructors
-- ---------------------------


makeWorld : List (Scene a) -> World a
makeWorld scenes =
    let
        addSceneData scene =
            insertData scene.id scene.data

        addTransitions : Scene a -> World a -> World a
        addTransitions scene world =
            foldr (addTransition scene.id) world scene.data.transitions

        addTransition : SceneID -> Transition -> World a -> World a
        addTransition origin transition =
            insertEdgeData origin transition.to transition.data

        addScene s =
            addTransitions s << addSceneData s
    in
    foldr addScene empty scenes
