module Agent (AgentID, Move(Move, MoveAndDeffuse, Stay),
    AgentState(AS, asPos, asId), WorldState(WS), AgentAction,
    AgentFunction, dummyAgent)
where

import Plane(Coords)

type AgentID = Int

data Move = Move Coords
          | MoveAndDeffuse Coords
          | Stay
          deriving (Show, Eq)

data AgentState a = AS {
    asPos :: Coords,
    asMax :: Int,
    asId :: AgentID,
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
  | otherwise = ((Stay, asSpecificData as), ws)
    where
      (x, y) = asPos as
      max = asMax as

