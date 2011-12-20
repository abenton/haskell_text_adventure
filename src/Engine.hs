{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ExistentialQuantification #-}

module Engine where
import Types2
import Utils
import Builder
import CmdParser
import Data.IORef
import Control.Monad.State
import Data.List (nub)
import Network (accept, listenOn, withSocketsDo, PortID(..), Socket)
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
type ES = (IORef GS, IORef [(Handle, String)])

startEngine :: GS -> Player -> IO ()
startEngine gs p = withSocketsDo $ do
  socket <- listenOn $ PortNumber 8080
  gsRef <- newIORef gs
  handlesRef <- newIORef []
  _ <- forkIO $ remoteHandler gsRef handlesRef socket
  _ <- forkIO $ localHandler p gsRef handlesRef 
--  forever $ do localHandler p gsRef handlesRef

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
  validMoves   = fmap (\(r1, r2) -> case r2 of 
                          Just r2' -> moveObjRms p r1 r2'
                          Nothing  -> id) validRmPairs
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
    _:_ -> case rs' of
      _:_ -> Right $ (id&&&![addRoom (addExit' r1 d r2)
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
                                else Left $ " does not have permission to remove a door.") where
  pRms = findRmsWith p gs
  dummyDoor = Door "door" dir allowAll
execAction gs p (SetStat n k v) = execSuMod n gs p setStat' where
  setStat' o = setNumField k v o
execAction gs p (MustHaveStatLE n k v) = execSuMod n gs p setUse where
  setUse o = setUsable o $ (isStatLE v k)&&&(isUsable o)
execAction gs p (MustHaveStatGE n k v) = execSuMod n gs p setUse where
  setUse o = setUsable o $ (isStatGE v k)&&&(isUsable o)
execAction gs p (MustHaveNObjs n k v) = execSuMod n gs p setUse where
  setUse o = setUsable o $ (hasNObj v k)&&&(isUsable o)
execAction gs p (Teleports r n) = execSuMod n gs p addTeleport where 
  addTeleport o = addEffect (teleports r) o
execAction gs p (SetsStat n k v) = execSuMod n gs p setsStat where
  setsStat o = addEffect (setStat k v) o
execAction gs _ Inv   = Right gs
execAction gs _ Stats = Right gs
execAction gs _ (AddMe _) = Right gs
execAction gs _ Help  = Right gs
execAction gs _ Save  = Right gs
execAction gs _ Quit  = Right gs

-- | Abstracts executing an object modification initiated by a super-user.
execSuMod :: (Objectable a) => String -> GS -> Player -> (AdvObject -> a) -> ErrGS
execSuMod n gs p f = (if isSuper p
                     then Right $ (modAdvObjs tmpObj f) gs
                     else Left $ notSudoMsg tmpObj "modify") where
  tmpObj = mkObj n ""

-- | Returned when a user does not have permission to issue an instruction.
notSudoMsg :: (Thing a) => a -> String -> String
notSudoMsg o verb = " does not have permission to " ++ verb ++ " a " ++
                    name o ++ "."

getPersonalResp :: GS -> Action -> ErrGS -> String
getPersonalResp _ _ (Left s)  = "You " ++ s
getPersonalResp _ a (Right _) = "You " ++ stdDisp a

getOthersResp :: GS -> Player -> Action -> ErrGS -> Maybe String
getOthersResp _ _ a _ | isPrivate a = Nothing
getOthersResp _ p _ (Left s)        = Just (name p ++ s)
getOthersResp _ p a (Right _)       = Just (name p ++ stdDisp a)

broadcast :: IORef GS -> IORef [(Handle, String)] -> Player -> Action -> ErrGS -> IO ()
broadcast gsRef handlesRef p a ret | isLoud a = do
                       gs <- readIORef gsRef
                       handles <- readIORef handlesRef
                       (forM_ (otherHandles handles) (\h -> maybeSendNetMsg h (othersResp gs)))
                       (case thisHandles handles of
                         (h:_) -> sendNetMsg h $ personalResp gs
                         []    -> return ())
                       return () where
  others handles = Prelude.filter (\(_, n) -> n /= name p) handles
  this handles = Prelude.filter (\(_, n) -> n == name p) handles
  thisHandles handles = fmap fst $ this handles
  otherHandles handles = fmap fst $ others handles
  othersResp gs = getOthersResp gs p a ret
  personalResp gs = getPersonalResp gs a ret
broadcast gsRef handlesRef p a ret = do
                       gs <- readIORef gsRef
                       handles <- readIORef handlesRef
                       (forM_ (sendHandles gs handles) (\h -> maybeSendNetMsg h (othersResp gs)))
                       (case thisHandles handles of
                           (h:_) -> sendNetMsg h $ personalResp gs
                           []    -> return ())
                       return () where
  pRms gs = findRmsWith p gs
  others handles = Prelude.filter (\(_, n) -> n /= name p) handles
  this handles = Prelude.filter (\(_, n) -> n == name p) handles
  thisHandles handles = fmap fst $ this handles
  otherNames handles = fmap snd (others handles)
  othersResp gs = getOthersResp gs p a ret
  personalResp gs = getPersonalResp gs a ret 
  revHandles handles = fmap (\(a', b) -> (b, a')) handles
  sendNames gs handles = fmap show $
              nub $ Prelude.filter (\(TB o) -> elem (name o)
                                               (otherNames handles)) $ 
              foldr (++) [] $ Prelude.map contains (pRms gs)
  sendHandles gs handles = foldr (\h -> ((case h of 
                          Just h' -> [h']
                          Nothing -> [])++)) [] $ 
                Prelude.map (\n -> Prelude.lookup n (revHandles handles)) (sendNames gs handles)

isPrivate :: Action -> Bool
isPrivate Inv   = True
isPrivate Stats = True
isPrivate Help  = True
isPrivate _     = False

isLoud    :: Action -> Bool
isLoud (Yell _) = True
isLoud _        = False

localHandler :: Player -> IORef GS -> IORef [(Handle, String)] -> IO ()
localHandler p gsRef handlesRef = 
  do cmd <- getLine
     (case parse cmd of
         Nothing -> putStrLn parseErrMsg
         Just a -> do
           gs' <- readIORef gsRef
           (case exec gs' a of 
               Left errMsg -> putStrLn errMsg
               Right gs''  -> do writeIORef gsRef gs''
                                 broadcast gsRef handlesRef p a $ exec gs' a))
     localHandler p gsRef handlesRef where
       exec gs a = execAction gs p a

-- | Listens for messages sent on the network (localhost in this case).
-- | Code for listening on the network borrowed from
-- | http://www.catonmat.net/blog/simple-haskell-tcp-server/
remoteHandler :: IORef GS -> IORef [(Handle, String)] -> Socket -> IO ()
remoteHandler gsRef handlesRef socket = do 
                              (handle, _, _) <- accept socket
                              hSetBuffering handle NoBuffering
                              _ <- forkIO $ cmdHandler handle gsRef handlesRef
                              remoteHandler gsRef handlesRef socket

cmdHandler :: Handle -> IORef GS -> IORef [(Handle, String)] -> IO ()
cmdHandler handle gsRef handlesRef =
  do cmd <- hGetLine handle
     (case parse cmd of
         Nothing -> sendNetMsg handle parseErrMsg
         Just a -> do
           gs <- readIORef gsRef
           clients <- readIORef handlesRef
           (case execAction gs (dummyP clients) a of
               Left s    -> sendNetMsg handle s
               Right gs' -> 
                 (case a of
                     (AddMe n) -> 
                       do writeIORef gsRef gs'
                          writeIORef handlesRef ((handle,n):clients) 
                          return ()
                     _ -> do writeIORef gsRef gs'
                             (broadcast gsRef handlesRef 
                              (dummyP clients) a $
                              execAction gs (dummyP clients) a))))
     cmdHandler handle gsRef handlesRef where
  dummyP clients = case Prelude.lookup handle clients of
    Nothing -> mkPlayer "" "" False
    Just n  -> mkPlayer n "" False

maybeSendNetMsg :: Handle -> Maybe String -> IO ()
maybeSendNetMsg handle (Just msg) = sendNetMsg handle msg
maybeSendNetMsg _ Nothing = return ()

sendNetMsg :: Handle -> String -> IO ()
sendNetMsg handle msg = do hPutStrLn handle msg
