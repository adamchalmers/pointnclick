module EngineTest exposing (testMakeSimpleWorld, testMakeWorld)

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


testMakeWorld : Test
testMakeWorld =
    let
        edge1to2 =
            { shape = Circle { x = 0, y = 0, radius = 10 } }

        edge1to3 =
            { shape = Circle { x = 10, y = 10, radius = 10 } }

        edge3to1 =
            { shape = Circle { x = 10, y = 10, radius = 10 } }

        scene1Data =
            { img = "img1"
            , targets = []
            , transitions =
                [ { to = 2, data = edge1to2 }
                , { to = 3, data = edge1to3 }
                ]
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

        scene3Data =
            { img = "img3"
            , targets = []
            , transitions = [ { to = 1, data = edge3to1 } ]
            }

        scene3 =
            { id = 3
            , data =
                scene3Data
            }

        world =
            makeWorld [ scene1, scene2, scene3 ]
    in
    describe "3-node world"
        [ test "size should be 3" <|
            \() ->
                world
                    |> Graph.size
                    |> Expect.equal 3
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
        , test "scene 3 data correct" <|
            \() ->
                world
                    |> Graph.getData 3
                    |> Expect.equal (Just scene3Data)
        , test "edge 1->2 data correct" <|
            \() ->
                world
                    |> Graph.getEdgeData 1 2
                    |> Expect.equal (Just edge1to2)
        , test "edge 1->3 data correct" <|
            \() ->
                world
                    |> Graph.getEdgeData 1 3
                    |> Expect.equal (Just edge1to3)
        , test "edge 3->1 data correct" <|
            \() ->
                world
                    |> Graph.getEdgeData 3 1
                    |> Expect.equal (Just edge3to1)
        , test "node 1 goes to 2 and 3" <|
            \() ->
                world
                    |> Graph.outgoing 1
                    |> Expect.equal (fromList [ 2, 3 ])
        , test "node 2 doesn't go anywhere" <|
            \() ->
                world
                    |> Graph.outgoing 2
                    |> Expect.equal (fromList [])
        , test "node 3 goes to 1" <|
            \() ->
                world
                    |> Graph.outgoing 3
                    |> Expect.equal (fromList [ 1 ])
        , test "there's a cycle from 1-3-1" <|
            \() ->
                world
                    |> Graph.isAcyclic
                    |> Expect.false "there should be a cycle from 1 to 3 to 1"
        ]
