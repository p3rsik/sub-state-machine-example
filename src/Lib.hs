{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Lib
    ( someFunc
    ) where

import qualified Control.Monad.State as S
import           Data.Kind           (Type)
import           GHC.Generics
import Data.Void (Void)
import Control.Monad (void)

newtype Key = Key { unKey :: Int }


data Created
data InProcess
data Processed

data First
data Second

class SMachine t m where
  data SState (t :: EntityType) m s :: Type

  getCreatedEntity :: Key -> m (SState t m Created)

  createEntity :: Key -> m (SState t m Created)
  processEntity :: SState t m Created -> m (SState t m Processed)

data EntityType = First | Second

type FakeState = S.State EntityType

instance SMachine 'First FakeState where
  data SState 'First FakeState s where
    Created1 :: { created1Key :: Key } -> SState 'First FakeState Created
    Processed1 :: { processed1Key :: Key } -> SState 'First FakeState Processed

  getCreatedEntity k = do
    return $ Created1 k

  createEntity k = do
    S.put First
    return $ Created1 k
  processEntity Created1 {..} = do
    S.put First
    return $ Processed1 created1Key

instance SMachine 'Second FakeState where
  data SState 'Second FakeState s where
    Created2 :: { created2Key :: Key } -> SState 'Second FakeState Created
    Processed2 :: { processed2Key :: Key } -> SState 'Second FakeState Processed

  getCreatedEntity k = do
    return $ Created2 k

  createEntity k = do
    S.put Second
    return $ Created2 k
  processEntity Created2 {..} = do
    S.put Second
    return $ Processed2 created2Key

data EntityState (t :: EntityType) s where
  Created :: { createdKey :: Key } -> EntityState t Created
  InProcess :: { inProcessKey :: Key } -> EntityState t InProcess
  Processed :: { processedKey :: Key } -> EntityState t Processed

class MetaMachine m where
  type State (t :: EntityType) m :: Type -> Type

  getInProcessEntity :: Key -> m (State t m InProcess)

  createNewEntity :: Key -> m (State t m Created)
  delegateEntity :: (SMachine t m) => State t m Created -> (Key -> m (SState t m Created)) -> m (State t m InProcess)
  delegateEntity2 :: (SMachine t m) => State t m InProcess -> (SState t m Created -> m (SState t m Processed)) -> m (State t m Processed)

instance MetaMachine FakeState where
  type State t FakeState = EntityState t

  getInProcessEntity k = return $ InProcess k

  createNewEntity key =
    return $ Created key
  delegateEntity Created {..} f = do
    -- let's imagine that we saved it somewhere
    delegatedEntity <- f createdKey
    return $ InProcess createdKey
  delegateEntity2 InProcess {..} f = do
    -- let's imagine we retrieved stored entity
    delegatedEntity <- getCreatedEntity inProcessKey
    f delegatedEntity
    return $ Processed inProcessKey

example1 :: FakeState ()
example1 = do
  let k = Key 1
  en <- createNewEntity k
  en' <- delegateEntity en $ createEntity @'First
  delegateEntity2 en' processEntity
  return ()

example2 :: FakeState ()
example2 = do
  let k = Key 1
  en <- createNewEntity k
  void . delegateEntity en $ createEntity @'First

someFunc :: IO ()
someFunc = return . const () . flip S.runState First $ do
  example2
  t <- S.get
  return ()
  -- en <- case t of
  --   First -> do
  --     getInProcessEntity @_ @'First $ Key 1
  --   Second -> do
  --     getInProcessEntity @_ @'Second $ Key 1
  -- delegateEntity2 en processEntity
