{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Ambiant.Simulation.Engine
       ( runSimulation
       ) where

import           Ambiant.Input.Brain
import           Ambiant.Input.World hiding (CellStart(..))
import qualified Ambiant.Input.World as IW
import           Ambiant.Simulation.Types
import           Ambiant.Simulation.NumberTheory

import           Control.Lens
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Writer
import           Control.Monad.RWS
import           Control.Monad.Trans.Iter

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Prelude hiding (round)


-- using a concrete type instance, lens/zoom won't work with polymorphic states
--    see ttps://github.com/ekmett/lens/issues/316

-- type AntSimulation a = ( MonadState GameState m
--                       , MonadReader GameConfig m
--                       , MonadWriter String m
--                       , MonadError String m) => m a

type AntSimulation = RWST GameConfig [GameTrace] GameState (Either String)

runSimulation :: World -> AntBrain -> AntBrain -> IterT (Writer [GameTrace]) (Either String [GameTrace])
runSimulation w rb bb = 
    let gs = mkInitialGameState w
        gc = GameConfig $ \c -> case c of Red -> rb; Black -> bb
    in  gameStep gc gs

mkInitialGameState :: World -> GameState
mkInitialGameState (World _ initialCells) =
    let (cells, _) = M.foldlWithKey' mkCell ([], AntId 0) initialCells
    in  mkGameState cells
  where
    mkCell :: ([(Pos, CellState)], AntId) -> Pos -> IW.CellStart -> ([(Pos, CellState)], AntId)
    mkCell (gs, nid)           pos IW.Rocky           = ((pos, CellState Rocky Nothing S.empty 0):gs, nid)
    mkCell (gs, nid)           pos (IW.Clear f)       = ((pos, CellState Clear Nothing S.empty f):gs, nid)
    mkCell (gs, nid@(AntId i)) pos (IW.Anthill color) = ((pos, CellState
                                                              (Anthill color)
                                                              (Just $ mkAnt nid color) S.empty 0):gs
                                                       , AntId $ i+1)

mkGameState :: [(Pos, CellState)] -> GameState
mkGameState cells =
    let antPos = M.fromList $ concatMap (\(p,cs) -> case cs^?cellAnt._Just.antId of
                                               Nothing -> []
                                               Just aid -> [(aid, p)])
                                        cells
    in  GameState (M.fromList cells) 0 (mkNTRandom 12345) antPos

gameStep :: GameConfig -> GameState -> IterT (Writer [GameTrace]) (Either String [GameTrace])
gameStep c s =
    case runRWST gameRound c s of
        Left err    -> return $ Left err
        Right (False,  _, w') -> return $ Right w'
        Right (True , s', w') -> delay $ do
            tell w'
            gameStep c s'

gameRound :: AntSimulation Bool
gameRound = do
    maxAntId <- liftM (maximumOf (gsCells.traverse.cellAnt.traverse.antId)) get
    case maxAntId of
        Just mx -> do forM_ [AntId 0..mx] antStep
                      get >>= (tell . return . TraceState)
                      return True
        Nothing -> return False

antStep :: AntId -> AntSimulation ()
antStep aid = do
    st <- get
    let mpa = do p <- st^?gsAntPos.ix aid
                 a <- st^?gsCells.ix p.cellAnt._Just
                 return (p,a)
    case mpa of
        Nothing         -> tell [TraceMessage $ "ant " ++ show aid ++ "doesn't exist anymore"]
        Just (pos, ant) -> 
            if 0 /= ant^.antResting
            then do
                antAt pos.antResting -= 1
                tell [TraceMessage $ show aid ++ " resting\n"]
            else do
                brain <- view gcBrain >>= (\f -> return $ f (ant^.antColor))
                case instructionOf brain (ant^.antState) of
                    Just instr -> instructionStep (ant,pos) instr
                    Nothing -> throwError "invalid instruction"
                  
instructionStep :: (Ant,Pos) -> Instruction -> AntSimulation ()
instructionStep (a,p) (Sense sd st1 st2 cond) = do
    let ad = a^.antFacing
    c <- useCellAt (relativePosition sd ad p)
    let st = if matchCond cond (a^.antColor) c then st1 else st2
    zoom (antAt p)$ setAntState st

instructionStep (a,p) (Mark mid st) = do
    zoom (cellAt p) $
      cellMarkers %= S.insert (a^.antColor,mid)
    zoom (antAt p) $ setAntState st
                                                
instructionStep (a,p) (Unmark mid st) = do
    zoom (cellAt p) $
      cellMarkers %= S.delete (a^.antColor,mid)
    zoom (antAt p) $ setAntState st

instructionStep (a,p) (PickUp st1 st2) =
    zoom (cellAt p) $ do
        c <- get
        if a^.antHasFood || c^.cellFood == 0
          then cellAnt.traversed.antState .= st2
          else do cellFood -= 1
                  zoom (cellAnt.traversed) $ do
                      antHasFood .= True
                      antState .= st1
              
instructionStep (a,p) (Drop st) =
    zoom (cellAt p) $
        if a^.antHasFood
        then do cellFood += 1
                cellAnt.traversed.antHasFood .= False
        else cellAnt.traversed.antState .= st

instructionStep (_,p) (Turn lr st) =
    zoom (antAt p) $ do
        antFacing %= turnDir lr
        antState .= st
    
instructionStep (a,p) (Move st1 st2) = do
    let newp = adjacentCell p (a^.antFacing)
    newc <- useCellAt newp
    if (newc^.cellType) == Rocky || isJust (newc^.cellAnt)
      then antAt p.antState .= st1
      else do cellAt p.cellAnt .= Nothing
              cellAt newp.cellAnt .= Just a
              gsAntPos.ix (a^.antId) .= newp
              zoom (antAt newp) $ do
                  antState .= st2
                  antResting .= 14
              checkForSurroundedAnts newp
    
instructionStep (_,p) (Flip n st1 st2) = do
    r <- nextRandomInt n
    antAt p.antState .= if r == 0 then st1 else st2
    
nextRandomInt :: Int -> AntSimulation Int
nextRandomInt n = do
    g <- use gsRandom
    let (r, g') = ntRandom g n
    gsRandom .= g'
    return r

otherColor :: Color -> Color
otherColor Red   = Black
otherColor Black = Red

checkForSurroundedAntAt :: Pos -> AntSimulation ()
checkForSurroundedAntAt p = do
    ma <- preuse (gsCells.ix p.cellAnt._Just)
    case ma of
        Nothing -> return ()
        Just ant -> do
            let enemyColor = otherColor $ ant^.antColor
            cells <- use gsCells
            let enemyCount = length $
                               filter (\a -> (a^.antColor) == enemyColor) $
                               map fromJust $
                               filter isJust $
                               map ((>>= (^.cellAnt)) . (`M.lookup` cells))
                               (neighbourCells p)

            when (enemyCount >= 5) $ do
                gsAntPos %= M.delete (ant^.antId)
                zoom (cellAt p) $ do
                    cellAnt .= Nothing
                    cellFood += 3

neighbourCells :: Pos -> [Pos]
neighbourCells p = map (adjacentCell p) [minBound..maxBound]

checkForSurroundedAnts :: Pos -> AntSimulation ()
checkForSurroundedAnts p = do
    checkForSurroundedAntAt p
    mapM_ checkForSurroundedAntAt $ neighbourCells p

setAntState :: (MonadState Ant m) => BrainState -> m ()
setAntState st = antState .= st

cellAt :: Pos -> Traversal' GameState CellState
cellAt p = gsCells.ix p

useCellAt :: Pos -> AntSimulation CellState
useCellAt p = use (gsCells.at p) >>= maybe (throwError "cell not found") return

antAt :: Pos -> Traversal' GameState Ant
antAt p = gsCells.ix p.cellAnt.traverse

adjacentCell :: Pos -> Direction -> Pos
adjacentCell (x,y) d =
    case d of
        E  -> (x+1, y)
        SE -> if even y then (x  , y+1) else (x+1, y+1)
        SW -> if even y then (x-1, y+1) else (x  , y+1)
        W  -> (x-1, y)
        NW -> if even y then (x-1, y-1) else (x  , y-1)
        NE -> if even y then (x  , y-1) else (x+1, y-1)

relativePosition :: SenseDirection -> Direction -> Pos -> Pos
relativePosition Here       _  p = p
relativePosition Ahead      ad p = adjacentCell p ad
relativePosition LeftAhead  ad p = adjacentCell p (turnDir LRLeft  ad)
relativePosition RightAhead ad p = adjacentCell p (turnDir LRRight ad)

matchCond :: SenseCondition -> Color -> CellState -> Bool
matchCond Friend         color cell = cell^?cellAnt._Just.antColor == Just color
matchCond Foe            color cell = cell^?cellAnt._Just.antColor == Just (otherColor color)

matchCond FriendWithFood color cell = fromMaybe False $ do
                                          ant <- cell^?cellAnt._Just
                                          return $ ant^.antColor == color && ant^.antHasFood

matchCond FoeWithFood    color cell = fromMaybe False $ do
                                          ant <- cell^?cellAnt._Just
                                          return $ ant^.antColor == otherColor color && ant^.antHasFood

matchCond Food           _     cell = cell^.cellFood > 0
matchCond Rock           _     cell = cell^.cellType == Rocky
matchCond (Marker mid)   color cell = (color, mid) `S.member` (cell^.cellMarkers)
matchCond FoeMarker      color cell = any (\(c,_) -> c == otherColor color) $ S.elems $ cell^.cellMarkers
matchCond Home           color cell = cell^.cellType == Anthill color
matchCond FoeHome        color cell = cell^.cellType == Anthill (otherColor color)

turnDir :: LeftOrRight -> Direction -> Direction
turnDir LRLeft  E  = NE
turnDir LRRight NE = E
turnDir LRLeft  d  = pred d
turnDir LRRight d  = succ d
