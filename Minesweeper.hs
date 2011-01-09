module Main
where

import Control.Arrow ((&&&))
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk

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

main = do
  let size = 5
      traces = getTraces 16 4 size 8 42 intelligentAgent
      worldSpace = [(x, y) | x <- [0..size], y <- [0..size]]
  print $ head traces
  initGUI
  mainWindow (size + 1) worldSpace

mainWindow n worldSpace = do
  window <- windowNew
--  widgetSetSizeRequest window 800 600
  windowMaximize window
  -- nonhomogenous hbox with no spacing
  hBox <- hBoxNew False 10
  -- add it to the window
  window `containerAdd` hBox
  -- table for showing the world
  table <- tableNew n n True
  -- fill the table with initial images
  fields <- mapM (initWorldCell table) worldSpace
  -- pack the table in the hbox
  boxPackStart hBox table PackGrow 10
  -- vertical box for the second part of window
  vBox <- vBoxNew False 10
  boxPackStart hBox vBox PackNatural 10
  -- first, a single label containing the actual time
  timeLabel <- labelNew $ Just "Time: 0"
  boxPackStart vBox timeLabel PackNatural 10
  -- then, a liststore (aka normal table) showing agent details
  agentDetails <- listStoreNew [(0, 0, CStay)]
  agentView <- treeViewNewWithModel agentDetails
  agentView `treeViewSetHeadersVisible` True
  c1 <- buildAgentColumn "ID" agentDetails (\(x, y, z) -> x)
  treeViewInsertColumn agentView c1 0
  c2 <- buildAgentColumn "Defused" agentDetails (\(x, y, z) -> y)
  treeViewInsertColumn agentView c2 1
  c3 <- buildAgentColumn "Cmd" agentDetails (\(x, y, z) -> z)
  treeViewInsertColumn agentView c3 2
  widgetSetSizeRequest agentView 400 400
  boxPackStart vBox agentView PackGrow 10
  -- lastly, a button box for the 3 control buttons
  hbtn <- hButtonBoxNew
  boxPackStart vBox hbtn PackNatural 10
  btnPrev <- buttonNewWithLabel "Previous"
  boxPackStart hbtn btnPrev PackNatural 5
  btnNext <- buttonNewWithLabel "Next"
  boxPackStart hbtn btnNext PackNatural 5
  btnNew <- buttonNewWithLabel "New"
  boxPackStart hbtn btnNew PackNatural 5
  -- cleanup event
  window `on` deleteEvent $ liftIO mainQuit >> return False
  -- show everything
  widgetShowAll window
  -- run GUI loop
  mainGUI

buildAgentColumn title model f = do
  c <- treeViewColumnNew
  cr <- cellRendererTextNew
  treeViewColumnPackStart c cr False
  cellLayoutSetAttributes c cr model $ \x -> [cellText := show $ f x]
  c `treeViewColumnSetTitle` title
  return c

initWorldCell table (x, y) = do
  img <- imageNewFromFile "empty.png"
  tableAttachDefaults table img x (x+1) y (y+1)
  return img

