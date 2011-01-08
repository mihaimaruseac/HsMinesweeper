module Agent (AgentID, Move(Move, MoveAndDefuse, Stay),
    AgentState(AS, asPos, asId), WorldState(WS), AgentAction,
    AgentPos, AgentFunction, dummyAgent, combineWS, emptyWS, intelligentAgent,
    InteligentState(IS) --debug
    )
where

import Debug.Trace

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
    unsafeSpots :: [Coords],
    uncertainSpots :: [Coords],
    agentPos :: [AgentPos]
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
combineWS (WS s us uc p) (WS s' us' uc' p')
  = WS (nub $ s ++ s') (nub $ us ++ us') (nub $ uc ++ uc') np
  where
    np = (nubBy (\x y -> fst x == fst y) $ p' ++ p)

emptyWS :: WorldState
emptyWS = WS [] [] [] []

data T = I | D deriving (Eq, Show)

getTrajectory :: Int -> Coords -> [Coords]
getTrajectory max p
  | p == (0, 0) = unfoldr (cTPos max) (I, I, p)
  | p == (0, max) = unfoldr (cTPos max) (I, D, p)
  | p == (max, 0) = unfoldr (cTPos max) (D, I, p)
  | p == (max, max) = unfoldr (cTPos max) (D, D, p)

cTPos :: Int -> (T, T, Coords) -> Maybe (Coords, (T, T, Coords))
cTPos max (dx, dy, (x, y))
  | dx == I && dy == I && x == max && y == max = Nothing
  | dx == I && dy == I && y == max = let p = (x+1, max) in Just (p, (I, D, p))
  | dx == I && dy == I = let p = (x, y+1) in Just (p, (I, I, p))
  | dx == I && x == max && y == 0 = Nothing
  | dx == I && y == 0 = let p = (x+1, 0) in Just (p, (I, I, p))
  | dx == I = let p = (x, y-1) in Just (p, (I, D, p))
  | dy == I && x == 0 && y == max = Nothing
  | dy == I && y == max = let p = (x-1, max) in Just (p, (D, D, p))
  | dy == I = let p = (x, y+1) in Just (p, (D, I, p))
  | x == 0 && y == 0 = Nothing
  | y == 0 = let p = (x-1, 0) in Just (p, (D, I, p))
  | otherwise = let p = (x, y-1) in Just (p, (D, D, p))

data InteligentState = IS {
    isTr :: [Coords]
    }
    deriving (Show, Eq)

intelligentAgent :: AgentFunction InteligentState
intelligentAgent ws@(WS ss uss ucs ap) as@(AS p@(x, y) max id False asd)
--  | tick = trace ("TICK") ((Stay, asd), ws)
{-  | otherwise -}
  = trace (""{-\n>>" ++ show nn ++ "<<\n"-}) ((Move np, asd), ws')
--  | t == [] = ((Stay, asd'), ws')
--  | pNC `elem` ucs' = ((MoveAndDefuse pNC, asd'), ws')
--  | otherwise = ((Move pNC, asd'), ws')
--  | otherwise = trace ("\n>" ++ {-show t ++ " " ++ show pNC ++ -}"<\n") ((Stay, asd'), ws')
  where
    -- get all neighbors of current position
    n = neigh max p
    -- parse tick information
    (ss', ucs') = getNewLists n False (ss, ucs)
    uss' = [] :: [Coords] -- TODO
    -- order the neighbors according to some function (neighCompare)
    nn = sortBy neighCompare n
    neighCompare n1 n2 = (neighEval n1) `compare` (neighEval n2)
    -- the real value of a node is the safe/unsafe value of all its neighbors,
    -- and node's safety penalty
    neighEval = uncurry (+) . (safePen &&& (sum.(map neighEval1).(neigh max)))
    -- if position is safe, apply a penalty of 10 points
    safePen n = if n `elem` ss' then -10 else 0
    -- the value of each node is safe*0 + uncertain * 10 + unknown * 1
    neighEval1 n = if n `elem` ss' then 0 else if n `elem` ucs' then 10 else 1
    -- next position and the new world state
    np = last nn
    ws' = WS ss' uss' ucs' [(id, np)] -- TODO: np or p?
intelligentAgent ws@(WS ss uss ucs ap) as@(AS p@(x, y) max id True asd)
  = trace (""{-\n>>" ++ show nn ++ "<<\n"-}) ((MoveAndDefuse np, asd), ws')
  where
    -- get all neighbors of current position
    n = neigh max p
    -- parse tick information
    (ss', ucs') = getNewLists n True (ss, ucs)
    uss' = [] :: [Coords] -- TODO
    -- order the neighbors according to some function (neighCompare)
    nn = sortBy neighCompare n
    neighCompare n1 n2 = (neighEval n1) `compare` (neighEval n2)
    -- the real value of a node is the safe/unsafe value of all its neighbors,
    -- and node's safety penalty
    neighEval = uncurry (+) . (safePen &&& (sum.(map neighEval1).(neigh max)))
    -- if position is safe, apply a penalty of 10 points
    safePen n = if n `elem` ss' then -10 else 0
    -- the value of each node is safe*0 + uncertain * 10 + unknown * 1
    neighEval1 n = if n `elem` ss' then 0 else if n `elem` ucs' then 10 else 1
    -- next position and the new world state
    np = last nn
    ws' = WS ss' uss' ucs' [(id, np)] -- TODO: np or p?

--    neighEval = uncurry (+) . (safePen &&& (sum.(map neighEval1).(neigh max)))
--    -- if position is safe, apply a penalty of 10 points
--    safePen n = if n `elem` ss' then 10 else 0

--    -- world state if staying (at least update according to ticks)
--    ws' = WS ss' uss' ucs' [(id, p)]
--    -- safe and uncertain spots
--
--    -- compute a possible agent state (mainly to obtain the trajectory)
--    asd' = nextASD max p asd
--    -- get the remaining trajectory
--    t = isTr $ fromJust asd'
--    -- get next viable neighbour: in trajectory (TODO: and not visited by others)
--    n = neigh max p
--    vT = filter ((/= Nothing).fst) . map (\x -> (elemIndex x t, x)) $ n
--    pNC = let vTT = map (first fromJust) vT; p1 = lookup 2 vTT in
--          if p1 == Nothing then fromJust (lookup 0 vTT)
--          else fromJust p1
----  | uss' /= [] = goToBombCell ws' as
--  | ucs' == [] = trace ("\nX" ++ show wsNC ++ "X\n") ((Move pNC, asdNC), wsNC)
--  | otherwise = ((Stay, asd'), ws')
--  where
--    asd' = nextASD max p asd
--    t = isTr $ fromJust asd'
--    ws' = WS ss' uss' ucs' [(id, p)]
--    n = neigh max p
--    (ss',ucs') = getNewLists n tick (ss, ucs)
--    uss' = [] :: [Coords] --TODO
--    vT = filter ((/= Nothing).fst) . map (\x -> (elemIndex x t, x)) $ n
--    vTT = map (first fromJust) vT
--    pNC = let p1 = lookup 2 vTT in if p1 == Nothing
--          then fromJust (lookup 0 vTT) else fromJust p1
--    asdNC = let a = fromJust asd' in
--          Just (a { isTr = tail $ dropWhile (/= pNC) $ isTr a})
--    wsNC = WS ss' uss' ucs' [(id, pNC)]
--    ns = firstTry ss t

goToBombCell ws@(WS ss uss ucs ap) as@(AS p@(x, y) max id tick asd)
  = undefined

getNewLists :: [Coords]->Bool -> ([Coords], [Coords]) -> ([Coords], [Coords])
getNewLists neighs True (ss, ucs) = (ss, (ucs ++ neighs) \\ ss)
getNewLists neighs False (ss, ucs) = let s' = ss ++ neighs in (s', ucs \\ s')

nextASD :: Int -> Coords -> Maybe InteligentState -> Maybe InteligentState
nextASD max p Nothing = Just $ IS (getTrajectory max p)
nextASD _ _ is = is

{--
firstTry :: [Coords] -> [Coords] -> [Coords]
firstTry safe [] = []
firstTry safe [x] = [x | x `elem` safe]
firstTry safe (x:y:l) = if x `elem` safe then fTaux (y:l) else []
  where
    fTaux [y] = [y | y `elem` safe]
    fTaux (y:l) = if y `elem` safe then fTaux l else (y:l)
--}

