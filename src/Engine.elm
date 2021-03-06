module Engine exposing (Scene, SceneData, SceneID, Shape(..), Transition, World, makeWorld, renderHeight, renderScene, renderWidth)

import Graph exposing (Graph, empty, insertData, insertEdgeData)
import Html exposing (div)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import List exposing (foldr, intersperse)
import String



-- ---------------------------
-- Data definitions
-- ---------------------------


type alias SceneID =
    Int


type alias Scene s =
    { id : SceneID
    , data : SceneData s
    }


type alias SceneData s =
    { img : String
    , targets : List (Target s)
    , transitions : List Transition
    , description : String
    }


type alias Target s =
    { shape : Shape
    , action : s -> s
    , img : s -> String
    , topLeft : ( Int, Int )
    , dimensions : Maybe ( Int, Int ) -- If Nothing, use default image source dimensions
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


renderScene : (SceneID -> msg) -> ((state -> state) -> msg) -> (String -> String) -> SceneData state -> state -> Html.Html msg
renderScene transitionMsg stateMsg locateImage sceneData state =
    -- Returns an <img> displaying the scene and a <map> that activates transitions when clicked.
    let
        transitionToArea : Transition -> Html.Html msg
        transitionToArea t =
            area <| onClick (transitionMsg t.to) :: attrsOf t.data.shape

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

                ( top, left ) =
                    t.topLeft

                mainAttrs =
                    [ Attrs.src (locateImage <| t.img state)
                    , Attrs.alt t.description
                    , Attrs.style "position" "absolute"
                    , Attrs.style "display" "block"
                    , Attrs.style "top" (intToPx top)
                    , Attrs.style "left" (intToPx left)
                    , onClick <| stateMsg t.action
                    ]
            in
            Html.img (mainAttrs ++ dimensionAttrs) []

        areas =
            List.map transitionToArea sceneData.transitions

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
        , Attrs.style "position" "relative"
        ]
    <|
        List.concat
            [ targetImgs
            , [ Html.node "map" [ Attrs.name "scene" ] areas ]
            , [ sceneImg ]
            ]


intToPx : Int -> String
intToPx i =
    String.fromInt i ++ "px"


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
