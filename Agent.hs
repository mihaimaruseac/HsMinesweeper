module Agent (AgentID, Move, AgentState(AS, asPos, asId),
    WorldState(WS), AgentAction, AgentFunction, dummyAgent)
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

type AgentAction a = (Move, WorldState, Maybe a)
type AgentFunction a = WorldState -> AgentState a -> AgentAction a

dummyAgent :: AgentFunction a
dummyAgent ws as
  | x == max && y == max = (Stay, ws, asSpecificData as)
  | otherwise = (Stay, ws, asSpecificData as)
    where
      (x, y) = asPos as
      max = asMax as

