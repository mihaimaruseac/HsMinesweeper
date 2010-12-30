module Agent (AgentID, Move(Move, MoveAndDefuse, Stay),
    AgentState(AS, asPos, asId), WorldState(WS), AgentAction,
    AgentPos, AgentFunction, dummyAgent, combineWS, intelligentAgent,
    InteligentState(IS) --debug
    )
where

import Debug.Trace

import Control.Arrow (first)
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
  = WS (nub $ s ++ s') (nub $ us ++ us') (nub $ uc ++ uc') (nub $ p ++ p')

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
intelligentAgent ws@(WS ss uss ucs ap) as@(AS p@(x, y) max id tick asd)
--  | uss' /= [] = goToBombCell ws' as
  | ucs' == [] = trace ("\nX" ++ show wsNC ++ "X\n") ((Move pNC, asdNC), wsNC)
  | otherwise = ((Stay, asd'), ws')
  where
    asd' = nextASD max p asd
    t = isTr $ fromJust asd'
    ws' = WS ss' uss' ucs' [(id, p)]
    n = neigh max p
    (ss',ucs') = getNewLists n tick (ss, ucs)
    uss' = [] :: [Coords] --TODO
    vT = filter ((/= Nothing).fst) . map (\x -> (elemIndex x t, x)) $ n
    vTT = map (first fromJust) vT
    pNC = let p1 = lookup 2 vTT in if p1 == Nothing
          then fromJust (lookup 0 vTT) else fromJust p1
    asdNC = let a = fromJust asd' in
          Just (a { isTr = tail $ dropWhile (/= pNC) $ isTr a})
    wsNC = WS ss' uss' ucs' [(id, pNC)]
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

