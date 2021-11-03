{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib
    ( someFunc
    ) where

import           Control.Concurrent     (MVar, forkIO, newEmptyMVar, putMVar,
                                         takeMVar, threadDelay)
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

class LinearState m where
  type State m :: Type -> Type

  createEntity :: Key -> m (State m Created)
  getCreatedEntity :: Key -> m (State m Created)
  awaitCreatedEntity :: TQueue Key -> m (State m Created)

  processEntity :: State m Created -> m (State m InProcess)
  getInProcessEntity :: Key -> m (State m InProcess)
  awaitInProcessEntity :: TQueue Key -> m (State m InProcess)

  completeEntity :: State m InProcess -> m (State m Processed)

instance LinearState IO where
  type State IO = EntityState
  createEntity k = return $ Created k
  getCreatedEntity k = return $ Created k
  awaitCreatedEntity tq = atomically $ do
    k <- readTQueue tq
    return $ Created k

  processEntity Created {..} = return $ InProcess createdKey
  getInProcessEntity k = return $ InProcess k
  awaitInProcessEntity tq = atomically $ do
    k <- readTQueue tq
    return $ InProcess k

  completeEntity InProcess {..} = return $ Processed inProcessKey

eventSender :: TQueue Key -> Key -> IO ()
eventSender tq k = do
  putStrLn "> Sending first event"
  atomically $ writeTQueue tq k
  putStrLn "> Waiting"
  threadDelay 1000
  putStrLn "> Sending second event"
  atomically $ writeTQueue tq k
  putStrLn "> Waiting"
  threadDelay 1000
  putStrLn "> Sending third event"
  atomically $ writeTQueue tq k

worker :: TQueue Key -> MVar () -> IO ()
worker tq v = do
  putStrLn ">> Awaiting created entity"
  cEn <- awaitCreatedEntity tq
  putStrLn ">> Processing created entity"
  void $ processEntity cEn
  putStrLn ">> Awaiting in-process entity"
  iEn <- awaitInProcessEntity tq
  putStrLn ">> Finishing entity"
  void $ completeEntity iEn
  putMVar v ()

someFunc :: IO ()
someFunc = do
  putStrLn "Setting env up"
  tq <- newTQueueIO
  mv <- newEmptyMVar
  tId <- forkIO $ worker tq mv
  tId2 <- forkIO $ eventSender tq (Key 0)
  takeMVar mv
  return ()
