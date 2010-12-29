module Main
where

import Control.Arrow ((&&&))

import Agent
import Master
import Plane

{-
type AgentTrace = [(Time, AgentID, Coords)]
type PlaneTrace = [(Time, Plane)]
--}

data Trace a = Trace {
            tTime :: Time,
            tAgentPos :: [(AgentID, Coords)]
            }
            deriving (Eq, Show)

getTrace :: World a -> Trace a
getTrace (t, p, ws, bs, as) = Trace t apos
  where
    agents = map snd as
    apos = map (asId &&& asPos) agents

getTraces :: (Eq a)
  => Time -> Int -> Int -> Int -> Int -> AgentFunction a -> [Trace a]
getTraces tmax agents size mines seed f = map getTrace ws
  where
    ws = evolveWorld tmax agents size mines seed f

main = print $ (getTraces 10 2 5 3 42 dummyAgent :: [Trace Int])

