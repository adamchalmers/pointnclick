module Engine exposing (Scene, Shape(..), Transition, World, makeWorld)

import Graph exposing (Graph, empty, insertData, insertEdge, insertEdgeData)
import List exposing (foldr)


constants =
    { sceneHeight = 600
    , sceneWidth = 800
    }


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
