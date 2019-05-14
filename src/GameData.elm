module GameData exposing (State, newGame, world)

import Engine exposing (Scene, SceneID, Shape(..), World, makeWorld)



-- ---------------------------
-- STATE
-- ---------------------------


type alias State =
    { leverOn : Bool }



-- ---------------------------
-- SCENE DATA
-- ---------------------------


entryWay : Scene State
entryWay =
    { id = 0
    , data =
        { img = "myst1.png"
        , targets = []
        , description = "You're standing on a dock. Calm water gently laps nearby. There's a lever in the distance."
        , transitions =
            [ { to = 1
              , data =
                    { shape = Rect { topLeft = ( 100, 100 ), bottomRight = ( 700, 500 ) }
                    }
              }
            ]
        }
    }


dock : Scene State
dock =
    { id = 1
    , data =
        { img = "myst2.png"
        , targets =
            [ { shape = Circle { x = 400, y = 300, radius = 100 }
              , action = \s -> { s | leverOn = not s.leverOn }
              , img = "dots.jpg"
              , topLeft = ( 400, 400 )
              , dimensions = Nothing
              , description = "A lever"
              }
            ]
        , description = "A walkway leads up the hillside to a pair of giant gears. There's a lever right next to you."
        , transitions =
            [ { to = 0
              , data = { shape = Rect { topLeft = ( 0, 500 ), bottomRight = ( 800, 600 ) } }
              }
            ]
        }
    }


newGame : ( SceneID, State )
newGame =
    ( 0, { leverOn = False } )


world : World State
world =
    makeWorld [ entryWay, dock ]
