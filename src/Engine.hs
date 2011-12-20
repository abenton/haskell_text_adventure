{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Engine where
import Types2
import Clients
import CmdParser
import Utils
import Builder
import Control.Concurrent
import Control.Monad.State
import Data.Map as Map
import Data.Set as Set
import Data.List (nub)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, Handle, BufferMode(..))
import Control.Concurrent (forkIO)

-- | The basic idea behind the game engine is that there is a single game
-- | state which is being mutated by clients interacting through the engine.
-- | The engine keeps a mutable list of mappings from Players to command
-- | strings.  Clients concurrently add commands for the engine to execute
-- | along with the Player they correspond to.  These commands are parsed,
-- | executed if valid, and the proper object will be sent back for the
-- | client to display as it pleases.
-- |
-- | Clients may either be normal players sending messages to the engine via
-- | command line, or over the network.  They can also be AIs attached to NPCs.
-- | As far as the engine is concerned, there is no difference between PCs and
-- | NPCs.  In this demo the engine only listen on localhost for clients.
-- | 
-- | The engine has the current copy of the game state and a mutable buffer
-- | of the list of actions to be executed.

-- | The state of the engine.  Keeps a copy of the current game state and
-- | a list of all the registered remote clients.
type ES = (GS, [(Handle, String)])

startEngine :: State ES () -> IO ()
startEngine gsST = withSocketsDo $ do
  socket <- listenOn $ PortNumber 8080
  --forkIO $ remoteHandler gsST socket
  forkIO $ localHandler (mkPlayer "main character" "" True) gsST
  return ()

type ErrGS = Either String GS

-- | The "critical loop" of the Engine.  Returns an error string if the action
-- | is not valid, else returns the new game state.
execAction :: GS -> Player -> Action -> ErrGS
execAction gs p (Go dir) = (case validMoves of
                           []    ->  Left $ " cannot go " ++ dirStr dir ++ "."
                           (_:_) ->  Right $ (id &&&! validMoves) gs) where
  pRms = findRmsWith p gs
  oppRms = fmap (\r -> getOppRoom r (Door "door" dir allowAll)) pRms
  validRmPairs = Prelude.filter isValidPair (zip pRms oppRms)
  validMoves   = fmap (\(r1, Just r2) -> moveObjRms p r1 r2) validRmPairs
  isValidPair (_, Just _) = True
  isValidPair _           = False
execAction gs p (Get o) = (case validPickups of
                           [] -> Left $ " has no " ++ name o ++ " to pick up."
                           (_:_) -> if not $ canTake o p
                               then Left $ " cannot pick up the " ++ 
                                    name o ++ "."
                               else Right $ (id &&&! validPickups) gs) where
  pRms = findRmsWith p gs
  validRms = Prelude.filter (\r -> hasObjChild o r) pRms
  validPickups = fmap (\r -> getObj o r p) validRms
execAction gs p (Drop o) = if not $ hasObjChild o p
                           then Left $ " has no " ++ name o ++ " to drop."
                           else Right $ dropObj o p gs
execAction gs p (Use o)  = (if not $ hasObjChild o p
                            then Left $ " has no " ++ name o ++ " to use."
                            else if not $ isUsable o p
                                 then Left $ " tried to use " ++ name o ++
                                      " but failed."
                                 else Right $ (use o p) gs)
-- These do not change the game state, just broadcast messages to others.
execAction gs _ (Say   _)  = Right gs
execAction gs _ (Yell  _)  = Right gs
execAction gs _ Look       = Right gs
execAction gs _ (LookAt _) = Right gs
execAction gs _ SeeAll     = Right gs
execAction gs p (MkObj o) = if isSuper p
                            then Right $ (addObjs o (findRmsWith p gs)) gs
                            else Left $ notSudoMsg o "make"
execAction gs p (MkRm d r) = (
  if isSuper p
  then case pRms of
    r1':_ -> case rs' of
      r2':_ -> Right $ (id&&&![addRoom (addExit' r1 d r2)
                             | r1 <- pRms, r2 <- rs']) gs
      []    -> Right $ ((addRoom r)&&&![addRoom (addExit' r1 d r)
                             | r1<-pRms]) gs
    []    -> Left $ " is off the map. . ."
  else Left $ notSudoMsg r "make") where
  rs' = findRms r gs
  pRms = findRmsWith p gs
execAction gs p (MkBag b) = if isSuper p
                            then Right $ (addObjs b (findRmsWith p gs)) gs
                            else Left $ notSudoMsg b "make"
execAction gs p (MkPlayer p') = if isSuper p
                                then Right $ (addObjs p' (findRmsWith p gs)) gs
                                else Left $ notSudoMsg p' "make"
execAction gs p (RmObj s) = (if isSuper p
                            then Right $ (removeObjs dummyObj pRms) gs
                            else Left $ notSudoMsg dummyObj "remove") where
  pRms = findRmsWith p gs
  dummyObj = mkObj s ""
execAction gs p (RmDoor dir) = (if isSuper p
                                then Right $ (rmDoors dummyDoor pRms) gs
                                else Left $ notSudoMsg dummyDoor "remove") where
  pRms = findRmsWith p gs
  dummyDoor = Door "door" dir allowAll
execAction gs p (SetStat n k v) = execSuMod n gs p setStat where
  setStat o = setNumField k v o
execAction gs p (MustHaveStatLE n k v) = execSuMod n gs p setUse where
  setUse o = setUsable o (isStatLE v k)&&&(isUsable o)
execAction gs p (MustHaveStatGE n k v) = execSuMod n gs p setUse where
  setUse o = setUsable o $ (isStatGE v k)&&&(isUsable o)
execAction gs p (MustHaveNObjs n k v) = execSuMod n gs p setUse where
  setUse o = setUsable o $ (hasNObj v k)&&&(isUsable o)
execAction gs p (Teleports r n) = execSuMod n gs p addTeleport where 
  addTeleport o = addEffect (teleports r) o
execAction gs p (SetsStat n k v) = execSuMod n gs p setsStat where
  setsStat o = addEffect (setStat k v) o
execAction gs p Inv   = Right gs
execAction gs p Stats = Right gs
execAction gs p Help  = Right gs
execAction gs p Save  = Right gs
execAction gs p Quit  = Right gs

-- | Abstracts executing an object modification initiated by a super-user.
execSuMod :: (Objectable a) => String -> GS -> Player -> (a -> a) -> ErrGS
execSuMod n gs p f = (if isSuper p
                     then Right $ (modObjs tmpObj f) gs
                     else Left $ notSudoMsg tmpObj "modify") where
  tmpObj = mkObj n ""

-- | Returned when a user does not have permission to issue an instruction.
notSudoMsg :: (Objectable a) => a -> String -> String
notSudoMsg o verb = " does not have permission to " ++ verb ++ " a " ++
                    name o ++ "."

getPersonalResp :: GS -> Action -> ErrGS -> String
getPersonalResp _ _ (Left s)  = "You " ++ s
getPersonalResp _ a (Right _) = "You " ++ stdDisp a

getOthersResp :: GS -> Player -> Action -> ErrGS -> Maybe String
getOthersResp _ _ a _ | isPrivate a = Nothing
getOthersResp _ p _ (Left s)        = Just (name p ++ s)
getOthersResp _ p a (Right _)       = Just (name p ++ stdDisp a)

broadcast :: GS -> [(Handle, String)] -> Player -> Action -> ErrGS -> IO ()
broadcast gs handles p a ret | isLoud a = do
                       (forM_ otherHandles (\h -> maybeSendNetMsg h othersResp))
                       (case thisHandles of
                         (h:_) -> sendNetMsg h personalResp
                         []    -> return ())
                       return () where
  others = Prelude.filter (\(h, n) -> n /= name p) handles
  this = Prelude.filter (\(h, n) -> n == name p) handles
  thisHandles = fmap fst this
  otherHandles = fmap fst others
  otherNames = fmap snd others
  othersResp = getOthersResp gs p a ret
  personalResp = getPersonalResp gs a ret
broadcast gs handles p a ret = do
                       (forM_ sendHandles (\h -> maybeSendNetMsg h othersResp))
                       (case thisHandles of
                           (h:_) -> sendNetMsg h personalResp
                           []    -> return ())
                       return () where
  pRms = findRmsWith p gs
  others = Prelude.filter (\(h, n) -> n /= name p) handles
  this = Prelude.filter (\(h, n) -> n == name p) handles
  thisHandles = fmap fst this
  otherHandles = fmap fst others
  otherNames = fmap snd others
  othersResp = getOthersResp gs p a ret
  personalResp = getPersonalResp gs a ret 
  revHandles = fmap (\(a, b) -> (b, a)) handles
  sendNames = fmap name $
              nub $ Prelude.filter (\(TB o) -> elem (name o) otherNames) $ 
              foldr (++) [] $ Prelude.map contains pRms
  sendHandles = foldr (\h -> ((case h of 
                          Just h' -> [h']
                          Nothing -> [])++)) [] $ 
                Prelude.map (\n -> Prelude.lookup n revHandles) sendNames

isPrivate :: Action -> Bool
isPrivate Inv   = True
isPrivate Stats = True
isPrivate Help  = True
isPrivate _     = False

isLoud    :: Action -> Bool
isLoud (Yell _) = True
isLoud _        = False

localHandler :: Player -> State ES () -> IO ()
localHandler p gsST = do cmd <- getLine
                         (case parse cmd of
                             Nothing -> putStrLn parseErrMsg
                             Just action -> do
                               (gsST', clients) <- get
                               (case execAction gsST' p action of 
                                   Left errMsg -> putStrLn errMsg
                                   Right gs'   -> do put (gs', clients)
                                                     broadcast gs' clients p action $ execAction gsST' p action))
                         gsST' <- get
                         localHandler p gsST'

-- | Listens for messages sent on the network (localhost in this case).
-- | Code for listening on the network borrowed from
-- | http://www.catonmat.net/blog/simple-haskell-tcp-server/
--remoteHandler :: State ES () -> Socket -> IO ()
--remoteHandler gsST socket = do 
--                              (handle, _, _) <- accept socket
--                              hSetBuffering handle NoBuffering
--                              forkIO $ cmdHandler handle gsST
--                              gsST' <- get
--                              remoteHandler gsST' socket

--cmdHandler :: Handle -> State ES () -> IO ()
--cmdHandler handle gsST = do cmd <- hGetLine handle
--                            (case parse cmd of
--                              Nothing -> sendNetMsg handle parseErrMsg
--                              Just action -> do
--                                (gsST', clients) <- get
--                                (case execAction gsST' (dummyPlayer clients) action of
--                                  Left s    -> sendNetMsg handle s
--                                  Right gs' -> (case action of
--                                    (AddMe n) -> do put (gs', (handle,n):clients)
--                                                    return ()
--                                    _         -> do put (gs',clients)
--                                                    broadcast gs' clients (dummyPlayer clients) action $ execAction gsST' (dummyPlayer clients) action))
--                             gsST' <- get
--                             cmdHandler handle gsST' where
--  dummyPlayer clients = case Prelude.lookup handle clients of
--    Nothing -> mkPlayer "" "" False
--    Just n  -> mkPlayer n "" False

maybeSendNetMsg :: Handle -> Maybe String -> IO ()
maybeSendNetMsg handle (Just msg) = sendNetMsg handle msg
maybeSendNetMsg handle Nothing = return ()

sendNetMsg :: Handle -> String -> IO ()
sendNetMsg handle msg = do hPutStrLn handle msg
