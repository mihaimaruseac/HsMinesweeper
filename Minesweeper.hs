module Main
where

import Master
import Agent
import Plane

type AgentTrace = [(Time, AgentID, Coords)]
type PlaneTrace = [(Time, Plane)]

data Trace = Trace {
            tTime :: Time,
            tAgentPos :: [(AgentID, Coords)]
            }
            deriving (Eq, Show)

getTrace :: World a -> Trace
getTrace (t, p, ws, as) = Trace t apos
  where
    agents = map snd as
    apos = map (\x -> (asId x, asPos x)) agents

getTraces :: Int -> Int -> Int -> Int -> Int -> AgentFunction a -> [Trace]
getTraces tmax agents size mines seed f = map getTrace ws
  where
    ws = evolveWorld tmax agents size mines seed f

main = print $ getTraces 10 2 5 3 42 dummyAgent

