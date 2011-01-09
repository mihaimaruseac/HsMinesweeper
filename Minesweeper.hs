module Main
where

import Control.Arrow ((&&&))

import Agent
import Master
import Plane

data Trace a = Trace {
            tTime :: Time,
            tAgentPos :: [(AgentID, Coords)],
            tCmd :: [AgentCmd],
            tBDefused :: [AgentBombs],
            tBombsRemaining :: [Coords],
            tWS :: WorldState
            }
            deriving (Eq, Show)

getTrace :: World a -> Trace a
getTrace (t, p, ws, ac, bs, as) = Trace t apos ac bs tb ws
  where
    agents = map snd as
    apos = map (asId &&& asPos) agents
    tb = getBombs p

getTraces :: (Eq a)
  => Time -> Int -> Int -> Int -> Int -> AgentFunction a -> [Trace a]
getTraces tmax agents size mines seed f = map getTrace ws
  where
    ws = evolveWorld tmax agents size mines seed f

main = print (head $ getTraces 16 4 5 8 42 intelligentAgent)

