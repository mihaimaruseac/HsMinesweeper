module Agent (AgentID, Move(Move, MoveAndDefuse, Stay),
    AgentState(AS, asPos, asId), WorldState(WS), AgentAction,
    AgentFunction, dummyAgent, combineWS)
where

import Plane(Coords)

type AgentID = Int

data Move = Move Coords
          | MoveAndDefuse Coords
          | Stay
          deriving (Show, Eq)

data AgentState a = AS {
    asPos :: Coords,
    asMax :: Int,
    asId :: AgentID,
    asTicks :: Int,
    asSpecificData :: Maybe a
    }
    deriving (Show, Eq)

data WorldState = WS {
    safeSpots :: [Coords]
--    unsafeSpots :: [Coords],
--    uncertainSpots :: [Coords],
    }
    deriving (Show, Eq)

type AgentAction a = ((Move, Maybe a), WorldState)
type AgentFunction a = WorldState -> AgentState a -> AgentAction a

dummyAgent :: AgentFunction a
dummyAgent ws as
  | x == max && y == max = ((Stay, asSpecificData as), ws)
  | y == max && even x = ((MoveAndDefuse (x + 1, y), asSpecificData as), ws)
  | y == 0 && odd x = ((MoveAndDefuse (x + 1, y), asSpecificData as), ws)
  | even x = ((MoveAndDefuse (x, y + 1), asSpecificData as), ws)
  | otherwise = ((MoveAndDefuse (x, y - 1), asSpecificData as), ws)
    where
      (x, y) = asPos as
      max = asMax as

combineWS :: WorldState -> WorldState -> WorldState
combineWS ws1 ws2 = ws1
