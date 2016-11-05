{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Main where

import Control.Monad.RWS
import Control.Monad.Catch

type AppEnv = ()
type AppState = ()

newtype App  m a = App (RWST AppEnv () AppState m a)
                   deriving ( Functor
                            , Applicative
                            , Monad
                            , MonadReader AppEnv
                            , MonadState  AppState
                            , MonadWriter ()
                            , MonadRWS AppEnv () AppState
                            , MonadThrow
                            , MonadCatch
                            , MonadMask
                            , MonadIO
                            )


runAppT :: (Functor m, Monad m, MonadIO m, MonadMask m)
        => AppEnv
        -> App m ()
        -> m ()

runAppT env (App m) = snd <$> execRWST m env ()


runApp :: (Functor m, Monad m, MonadIO m, MonadMask m)
       => AppEnv
       -> App m ()
       -> m ()

runApp env m = runAppT env m `finally` finalizeAll
  where
    finalizeAll = do
      liftIO $ putStrLn "FINALIZE"

main = do

  runApp () $ do
    liftIO $ putStrLn "ONE"
    liftIO $ putStrLn "TWO"

