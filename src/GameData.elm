module GameData exposing (firstScene, world)

import Engine exposing (Scene, SceneID, Shape(..), World, makeWorld)



-- ---------------------------
-- GAME DATA
-- ---------------------------


entryWay : Scene
entryWay =
    { id = 0
    , data =
        { img = "myst1"
        , targets = []
        , transitions =
            [ { to = 1
              , data =
                    { shape = Rect { topLeft = ( 100, 100 ), bottomRight = ( 700, 500 ) }
                    }
              }
            ]
        }
    }


dock : Scene
dock =
    { id = 1
    , data =
        { img = "myst2"
        , targets = [ Circle { x = 400, y = 300, radius = 100 } ]
        , transitions =
            [ { to = 0
              , data = { shape = Rect { topLeft = ( 0, 500 ), bottomRight = ( 800, 600 ) } }
              }
            ]
        }
    }


firstScene : SceneID
firstScene =
    0


world : World
world =
    makeWorld [ entryWay, dock ]
