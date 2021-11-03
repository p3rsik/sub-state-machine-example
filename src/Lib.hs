{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib
    ( someFunc
    ) where

import           Control.Concurrent     (MVar, forkIO, killThread, newEmptyMVar,
                                         putMVar, takeMVar, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Kind              (Type)

newtype Key = Key { unKey :: Int }

data Created
data InProcess
data Processed

data First
data Second

data EntityState s where
  Created :: { createdKey :: Key } -> EntityState Created
  InProcess :: { inProcessKey :: Key } -> EntityState InProcess
  Processed :: { processedKey :: Key } -> EntityState Processed

data ErasedEntityState where
  ErasedCreated :: EntityState Created -> ErasedEntityState
  ErasedInProcess :: EntityState InProcess -> ErasedEntityState

class LinearState m where
  type State m :: Type -> Type

  createEntity :: Key -> m (State m Created)
  awaitCreatedEntity :: TQueue ErasedEntityState -> m (Maybe (State m Created))
  getCreatedEntity :: ErasedEntityState -> Maybe (State m Created)

  processEntity :: State m Created -> m (State m InProcess)
  awaitInProcessEntity :: TQueue ErasedEntityState -> m (Maybe (State m InProcess))
  getInProcessEntity :: ErasedEntityState -> Maybe (State m InProcess)

  completeEntity :: State m InProcess -> m (State m Processed)

instance LinearState IO where
  type State IO = EntityState

  createEntity k = return $ Created k
  awaitCreatedEntity tq = atomically $ do
    en <- readTQueue tq
    return $ getCreatedEntity @IO en
  getCreatedEntity = \case
    ErasedCreated en -> Just en
    _                -> Nothing

  processEntity Created {..} = return $ InProcess createdKey
  awaitInProcessEntity tq = atomically $ do
    en <- readTQueue tq
    return $ getInProcessEntity @IO en
  getInProcessEntity = \case
    ErasedInProcess en -> Just en
    _                  -> Nothing

  completeEntity InProcess {..} = return $ Processed inProcessKey

eventSender :: TQueue ErasedEntityState -> Key -> IO ()
eventSender tq k = do
  threadDelay 1000000
  putStrLn "> Sending first event"
  atomically $ writeTQueue tq $ ErasedCreated $ Created k
  putStrLn "> Waiting"
  threadDelay 1000000
  putStrLn "> Sending second event"
  atomically $ writeTQueue tq $ ErasedInProcess $ InProcess k

worker :: TQueue ErasedEntityState -> MVar () -> IO ()
worker tq v = do
  putStrLn ">> Awaiting created entity"
  Just cEn <- awaitCreatedEntity tq
  putStrLn ">> Processing created entity"
  void $ processEntity cEn
  putStrLn ">> Awaiting in-process entity"
  Just iEn <- awaitInProcessEntity tq
  putStrLn ">> Finishing entity"
  void $ completeEntity iEn
  putMVar v ()

test :: MVar () -> IO ()
test mv = do
  putStrLn "Setting env up"
  tq <- newTQueueIO
  tId <- forkIO $ worker tq mv
  tId2 <- forkIO $ eventSender tq (Key 0)
  return ()

someFunc :: IO ()
someFunc = do
  mv <- newEmptyMVar
  tid <- forkIO $ test mv
  threadDelay 1000
  killThread tid
  takeMVar mv
  return ()
