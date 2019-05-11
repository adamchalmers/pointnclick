module EngineTest exposing (testMakeSimpleWorld)

import Engine exposing (Shape(..), makeWorld)
import Expect exposing (Expectation)
import Graph
import Set exposing (fromList)
import Test exposing (..)


testMakeSimpleWorld : Test
testMakeSimpleWorld =
    let
        edge1to2 =
            { shape = Circle { x = 0, y = 0, radius = 10 } }

        scene1Data =
            { img = "img1"
            , targets = []
            , transitions = [ { to = 2, data = edge1to2 } ]
            }

        scene1 =
            { id = 1
            , data =
                scene1Data
            }

        scene2Data =
            { img = "img2"
            , targets = []
            , transitions = []
            }

        scene2 =
            { id = 2
            , data =
                scene2Data
            }

        world =
            makeWorld [ scene1, scene2 ]
    in
    describe "Simple world"
        [ test "size should be 2" <|
            \() ->
                world
                    |> Graph.size
                    |> Expect.equal 2
        , test "scene 1 data correct" <|
            \() ->
                world
                    |> Graph.getData 1
                    |> Expect.equal (Just scene1Data)
        , test "scene 2 data correct" <|
            \() ->
                world
                    |> Graph.getData 2
                    |> Expect.equal (Just scene2Data)
        , test "edge 1->2 data correct" <|
            \() ->
                world
                    |> Graph.getEdgeData 1 2
                    |> Expect.equal (Just edge1to2)
        , test "only outgoing node from 1 is 2" <|
            \() ->
                world
                    |> Graph.outgoing 1
                    |> Expect.equal (fromList [ 2 ])
        , test "only incoming node from 2 is 1" <|
            \() ->
                world
                    |> Graph.incoming 2
                    |> Expect.equal (fromList [ 1 ])
        ]
