{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hedgehog.Timeout where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Catch (catch)

import           Hedgehog
import           Hedgehog.Internal.Timeout

prop_timeout_ok :: Property
prop_timeout_ok =
  withTests 1 . withTimeout 1000 . property $ do
    liftIO $ threadDelay 100
    success

prop_timeout_fail :: Property
prop_timeout_fail =
--  withTests 1 . withTimeout (seconds 1000) . property $ (do
  withTests 1 . withTimeout 1000 . property $ (do
    liftIO $ threadDelay 100000
    failure) `catch` \(_ :: Timeout) -> success

tests :: IO Bool
tests =
  checkParallel $$(discover)
