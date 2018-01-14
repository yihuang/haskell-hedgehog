{-# OPTIONS_HADDOCK not-home #-}
module Hedgehog.Internal.Timeout (
    TimeoutResult(..)
  , waitWithTimeout

  , Timeout(..)
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, waitSTM, waitEither)
import           Control.Concurrent.Async (async, cancel, wait, cancelWith, waitCatch)
import           Control.Exception (asyncExceptionToException, asyncExceptionFromException)
import           Control.Exception.Base (SomeException(..), Exception(..), AsyncException(..))
import           Control.Monad.Catch (catch, throwM, fromException)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Except (ExceptT, throwE)

import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.Bool (bool)

data TimeoutResult a =
    Ok a
  | Bad Int
  | Badder SomeException

waitWithTimeout :: Int -> Async a -> IO (TimeoutResult a)
waitWithTimeout d a = do
  r <- liftIO $ newIORef False
  s <- liftIO . async $ threadDelay d
  e <- liftIO $ waitEither a s
  case e of
    Left a' ->
      pure $ Ok a'
    Right _ -> do
      liftIO $ writeIORef r True
      liftIO $ cancelWith a (Timeout d)
      x <- waitCatch a
      case x of
        Left xx | Just (Timeout i) <- fromException xx -> do
          pure $ Bad i

        Left er ->
          pure $ Badder er

        Right a ->
          pure $ Ok a


newtype Timeout =
  Timeout Int
  deriving (Eq, Show)

instance Exception Timeout where
  toException =
    asyncExceptionToException
  fromException =
    asyncExceptionFromException
