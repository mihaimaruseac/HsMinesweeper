module Agent (AgentID, Move(Move, MoveAndDefuse, Stay),
    AgentState(AS, asPos, asId), WorldState(WS), AgentAction,
    AgentPos, AgentFunction, dummyAgent, combineWS, emptyWS, intelligentAgent)
where

import Control.Applicative (pure, (<*>))
import Control.Arrow (first, (&&&))
import Data.List
import Data.Maybe

import Plane(Coords, neigh)

type AgentID = Int
type AgentPos = (AgentID, Coords)

data Move = Move Coords
          | MoveAndDefuse Coords
          | Stay
          deriving (Show, Eq)

data AgentState a = AS {
    asPos :: Coords,
    asMax :: Int,
    asId :: AgentID,
    asTick :: Bool,
    asSpecificData :: Maybe a
    }
    deriving (Show, Eq)

data WorldState = WS {
    safeSpots :: [Coords],
    uncertainSpots :: [Coords]
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
combineWS (WS s uc) (WS s' uc') = WS ns nu
  where
    ns = nub $ s ++ s'
    nu = (nub $ uc ++ uc') \\ ns

emptyWS :: WorldState
emptyWS = WS [] []

data InteligentState = IS {} deriving (Show, Eq)

-- sort the neighbors according to some heuristics depending on safe and
-- unsafe spots
intelligentAgent :: AgentFunction InteligentState
intelligentAgent ws@(WS ss ucs) as@(AS p@(x, y) max id tick asd)
  = ((m np, asd), ws')
  where
    -- action
    m = if tick then MoveAndDefuse else Move
    -- get all neighbors of current position
    n = neigh max p
    -- parse tick information
    (ss', ucs') = getNewLists n tick (ss, ucs)
    -- order the neighbors according to some function (neighCompare)
    nn = sortBy neighCompare n
    neighCompare n1 n2 = neighEval n1 `compare` neighEval n2
    neighEval = uncurry (+) . (selfEval &&& nEval)
    selfEval = sum . ([safePen, bHelp] <*>) . pure
    safePen n = if n `elem` ss then -10 else 0
    bHelp (x, y) = if tick && (x == 0 || y == 0 || x == max || y == max)
                   then -20 else 0
    nEval = sum.map neighEval1.filter (/= p).neigh max
    neighEval1 n = if n `elem` ss' then 0 else if n `elem` ucs' then -10 else 1
    -- next position and the new world state
    np = last nn
    nnx = zip nn (map neighEval nn)
    ws' = WS (ss' ++ [p, np]) (ucs' \\ [p, np])

-- new safe and unsafe spots
getNewLists :: [Coords]->Bool -> ([Coords], [Coords]) -> ([Coords], [Coords])
getNewLists neighs True (ss, ucs) = (ss, (ucs ++ neighs) \\ ss)
getNewLists neighs False (ss, ucs) = let s' = ss ++ neighs in (s', ucs \\ s')

