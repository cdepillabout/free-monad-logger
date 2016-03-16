{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

import Prelude hiding (log)

import Control.Monad.Free
import Control.Monad.Logger
import qualified Data.ByteString as BS
import Data.Text
import System.Log.FastLogger

someFunc :: IO ()
someFunc = interpreter example1 *> interpreter example2

data LogF next where
    LogAction :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> next -> LogF next

instance Functor LogF where
    fmap :: (a -> b) -> LogF a -> LogF b
    fmap f (LogAction loc logSource logLevel msg next) = LogAction loc logSource logLevel msg $ f next

type Log a = Free LogF a

log :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> Log ()
log loc logSource logLevel msg = liftF $ LogAction loc logSource logLevel msg ()

interpreter :: forall a . Log a -> IO a
interpreter = iterM go
  where
    go :: LogF (IO a) -> IO a
    go (LogAction loc logSource logLevel msg next) = defaultOutput loc logSource logLevel msg *> next

instance MonadLogger (Free LogF) where
    monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> Log ()
    monadLoggerLog = log

defaultOutput :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> IO ()
defaultOutput loc logSource logLevel msg =
    BS.putStr $ fromLogStr $ defaultLogStr loc logSource logLevel $ toLogStr msg

example1 :: Log ()
example1 = do
    $(logDebug) "hello1"
    $(logDebug) "bye1"

example2 :: MonadLogger m => m ()
example2 = do
    $(logDebug) "hello2"
    $(logDebug) "bye2"
