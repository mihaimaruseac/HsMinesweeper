module Main
where

import Data.DList hiding (map)

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

getTraces :: Int -> Int -> Int -> Int -> Int -> [Trace]
getTraces tmax agents size mines seed = map getTrace ws
  where
    ws = evolveWorld tmax agents size mines seed

main = print $ getTraces 10 2 5 3 42

