module Engine exposing (Scene, SceneData, SceneID, Shape(..), Transition, World, attrsOf, getImg, makeWorld, renderScene)

import Graph exposing (Graph, empty, insertData, insertEdge, insertEdgeData)
import Html
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import List exposing (foldr, intersperse)
import String exposing (concat)



-- ---------------------------
-- Data definitions
-- ---------------------------


type alias SceneID =
    Int


type alias Scene =
    { id : SceneID
    , data : SceneData
    }


type alias SceneData =
    { img : String
    , targets : List Shape
    , transitions : List Transition
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


type alias World =
    -- Remember Graph is type (Graph nodeID nodeData edgeData)
    Graph Int SceneData TransitionData


getImg : SceneData -> String
getImg sceneData =
    "images/" ++ sceneData.img ++ ".png"



-- ---------------------------
-- Rendering
-- ---------------------------


renderScene : (SceneID -> msg) -> SceneData -> List (Html.Html msg)
renderScene msgConstructor sceneData =
    let
        attrs =
            [ Attrs.name "scene" ]

        toArea : Transition -> Html.Html msg
        toArea transition =
            Html.node "area" (onClick (msgConstructor transition.to) :: attrsOf transition.data.shape) []

        areas =
            List.map toArea sceneData.transitions
    in
    [ Html.node "map" attrs areas
    , Html.img
        [ Attrs.usemap "#scene"
        , Attrs.src (getImg sceneData)
        , Attrs.alt "Game scene"
        , Attrs.width 800
        , Attrs.height 600
        ]
        []
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
                    fourCoords |> intersperse "," |> concat
            in
            [ Attrs.shape "rect"
            , Attrs.coords coordStr
            ]



-- ---------------------------
-- Constructors
-- ---------------------------


makeWorld : List Scene -> World
makeWorld scenes =
    foldr addScene empty scenes


addScene : Scene -> World -> World
addScene s =
    let
        addSceneData scene =
            insertData scene.id scene.data

        addTransitions : Scene -> World -> World
        addTransitions scene world =
            foldr (f scene.id) world scene.data.transitions

        f : SceneID -> Transition -> World -> World
        f origin transition =
            insertEdgeData origin transition.to transition.data
    in
    addTransitions s << addSceneData s
