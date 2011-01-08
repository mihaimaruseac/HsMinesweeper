module Master (Time, AgentActivations, World, evolveWorld, AgentBombs,
  Cmd (CStay, CMoveDefuse, CMove), AgentCmd)
where

import Control.Arrow ((&&&))
import Data.Array
import Data.List

import Debug.Trace

import Agent
import Plane

data Cmd = CStay | CMove | CMoveDefuse deriving (Eq, Enum, Show)

type Time = Int
type AgentActivations a = (Time, AgentState a)
type AgentBombs = (AgentID, Int)
type AgentCmd = (AgentID, Cmd)
type World a = (Time, Plane, WorldState,
                [AgentCmd], [AgentBombs], [AgentActivations a])
type AgentActivity a = (AgentPos, (Move, Maybe a))

buildWorld :: Int -> Int -> Int -> Int -> World a
buildWorld agents size mines seed
  | size < 5 = error "Field too small"
  | agents == 0 = error "Need at least one agent"
  | agents == 1 = (0, p, iws, [], [], [(0, AS (0, 0) size 0 False Nothing)])
  | agents == 2 = (0, p, iws, [], [], [(0, AS (0, 0) size 0 False Nothing),
                                   (0, AS (size, size) size 1 False Nothing)])
  | agents == 3 = (0, p, iws, [], [], [(0, AS (0, 0) size 0 False Nothing),
                                   (0, AS (0, size) size 1 False Nothing),
                                   (0, AS (size, size) size 2 False Nothing)])
  | agents == 4 = (0, p, iws, [], [], [(0, AS (0, 0) size 0 False Nothing),
                                   (0, AS (0, size) size 1 False Nothing),
                                   (0, AS (size, 0) size 2 False Nothing),
                                   (0, AS (size, size) size 3 False Nothing)])
  | otherwise = error "Too many agents"
  where
    p = buildPlane size mines seed
    iws = WS [] [] [] []

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateUntil p f = (takeWhile (not . p) . iterate f $)

evolveWorld :: (Eq a) =>
  Time -> Int -> Int -> Int -> Int -> AgentFunction a -> [World a]
evolveWorld tmax agents size mines seed f
  = iterateUntil endOfWorld (advanceWorld size f) p0
    where
      p0 = buildWorld agents size mines seed
      endOfWorld (_, _, _, _, _, []) = True
      endOfWorld (t, p, _, _, _, _) = t > tmax || safePlane p

__w0, __w1, __w1' :: World a
__w0 = (0, buildPlane 5 3 42, WS [] [] [] [], [], [], [(0, AS (0, 0) 5 0 False Nothing)])
__w1 = (0, buildPlane 5 3 42, WS [(1, 0)] [] [(0, 1)] [], [], [], [(0, AS (0, 0) 5 0 True Nothing)])
__w1' = (0, buildPlane 5 3 42, WS [] [] [] [], [], [], [(0, AS (0, 0) 5 0 False Nothing), (0, AS (5, 5) 5 1 False Nothing)])
__w4, __w5, __w5' :: World InteligentState
__w4 = (0, buildPlane 5 3 42, WS [(0, 0), (0, 1), (1, 0), (1, 1)] [] [] [],
        [], [], [(0, AS (0, 2) 5 0 False
        (Just (IS [(0, 3), (0, 4), (0, 5), (1, 5), (1, 4), (1, 3), (1, 2), (1, 1)])))])
__w5 = (0, buildPlane 5 3 42,
        WS [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2), (1, 3)]
        [] [] [],
        [], [], [(0, AS (0, 4) 5 0 False
        (Just (IS [(0, 5), (1, 5), (1, 4), (1, 3)])))])
__w5' = (0, buildPlane 5 3 42,
        WS [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (1, 0), (1, 1), (1, 2), (1, 3)]
        [] [] [],
        [], [], [(0, AS (1, 4) 5 0 False
        (Just (IS [(1, 3), (1, 2), (1, 1), (1, 0), (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5)])))])

advanceWorld :: (Eq a) => Int -> AgentFunction a -> World a -> World a
advanceWorld size f (t, p, w, ac, bs, as) = (t+1, p', w', ac', bs', as')
  where
    now = filter (\x -> fst x == t) as
    activeAgents = map snd now
    actions = map (f w) activeAgents
    actionsState = zip (map (asId &&& asPos) activeAgents)
                           (map fst actions)
    later = removeDuplicatesBy notSameDefuse $ filter (\x -> fst x /= t) as
    notSameDefuse (_, as1) (_, as2) = asPos as1 == asPos as2
    as' = concatMap (newSt t size p) actionsState ++ later
    defusedPoss = filter (\c -> getTicks p c > 7) $ map (asPos.snd) later
    p' = defuse p defusedPoss
    agentsDefusing = map (asId.snd &&& asPos.snd) later
    success = map fst $ filter (\(_,y)->y `elem` defusedPoss) agentsDefusing
    bs' = addSuccesses success bs
    w' = foldl combineWS emptyWS $ map snd actions
    ac' = zip (map (asId . snd) now) (map (getCmd.fst.fst) actions)

getCmd :: Move -> Cmd
getCmd Stay = CStay
getCmd (Move _) = CMove
getCmd (MoveAndDefuse _) = CMoveDefuse

newSt :: Time -> Int -> Plane -> AgentActivity a -> [AgentActivations a]
newSt t size pl ((id, p), (Stay, o))
  = [(t + 1, AS p size id (trim (getTicks pl p) > 0) o)]
newSt t size pl ((id, p), ((Move np), o))
  = [(t + 1, AS np size id (trim (getTicks pl np) > 0) o) | safe np]
  where
    safe pos = getTicks pl pos < 5
newSt t size pl ((id, p), ((MoveAndDefuse np), o))
  = [(t + 2, AS np size id (trim (getTicks pl np) > 0) o)]

removeDuplicatesBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
removeDuplicatesBy f l = filter (not.elemBy f (l \\ nubBy f l)) l
  where
    elemBy f ys x = foldr ((||) . f x) False ys

trim :: Value -> Value
trim v = if v > 7 then v - 8 else v

addSuccesses :: [AgentID] -> [AgentBombs] -> [AgentBombs]
addSuccesses a b = nubBy (\x y -> fst x == fst y) $ map (addSuccess b) a ++ b

addSuccess :: [AgentBombs] -> AgentID -> AgentBombs
addSuccess bs id = case lookup id bs of
  Nothing -> (id, 1)
  Just v -> (id, v + 1)

