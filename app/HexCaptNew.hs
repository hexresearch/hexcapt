{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.Event (Event)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Exception.Lifted
import Control.Monad.Except
-- import Control.Monad.Catch
import Control.Monad.RWS
import Data.Default
import Data.Digest.Pure.SHA
import Data.Digits
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (view)
import Servant.Server

import HEXCapt.Types
import HEXCapt.Config
import HEXCapt.Server

import qualified System.Shell.Iptables as Iptables
import System.Shell.Iptables


runApp :: (MonadIO m, MonadBaseControl IO (App m))
       => HexCaptCfg
       -> App m ()
       -> m ()

runApp cfg m = do
  track <- liftIO $ newTVarIO (S.empty)
  as    <- liftIO $ newTVarIO []
  let env = AppEnv track as cfg
  runAppT env (m `finally` finalize)
  where
    finalize = do
      ts <- asks (view chainTrack) >>= liftIO . (atomically . readTVar)
      as <- asks (view actions) >>= liftIO . (atomically . readTVar)

      liftIO $ mapM_ killThread as

      forM_ ts $ \(ChainRef t c n) -> do
        liftIO $ do
          Iptables.unlinkChain t c n
          Iptables.deleteChain t n

newObj :: Monad m => App m Int
newObj = do
  n <- gets (view chainNum)
  modify (over chainNum succ)
  return n

genChainName :: (MonadIO m)
             => TableName
             -> ChainName
             -> App m ChainName

genChainName t c = do
  let alph = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :: String
  let alphLen = toInteger $ length alph
  n <- newObj
  let digs = fmap ((!!) alph . fromIntegral) $ digits alphLen $ integerDigest $ sha1 [qq|$t$c$n|]
  return $ "hx" <> (take 20 digs)

trackChain ::   MonadIO m
             => TableName
             -> ChainName
             -> ChainName
             -> App m ()
trackChain t c n = do
  tv <- asks (view chainTrack)
  liftIO $ atomically $ modifyTVar tv $ S.insert (ChainRef t c n)

insertChain :: (MonadIO m)
            => TableName
            -> ChainName
            -> Maybe Int
            -> (TableName -> ChainName -> App m a)
            -> App m a

insertChain t c mpos m = do
  nm <- genChainName t c

  liftIO $ do
    putStrLn  [qq|inserting new chain $nm|]
    Iptables.createChain t nm
    Iptables.insertRule t c mpos ([qq|-j $nm|]::String)

  x <- m t nm
  trackChain t c nm
  return x

emptyChain :: MonadIO m => TableName -> ChainName -> App m ()
emptyChain t c = do
  liftIO $ Iptables.insertRule t c Nothing (J RETURN)
  return ()

acceptDNS :: MonadIO m => TableName -> ChainName -> App m ()
acceptDNS t c = do

  cfg <- asks (view config)

  liftIO $ Iptables.insertRule t c Nothing [P (UDP (Just 53)), J ACCEPT]
  liftIO $ Iptables.insertRule t c Nothing [P (TCP (Just 53)), J ACCEPT]

  forM_ [ e | e@(NDNProxyCfg{..}) <- universeBi cfg ] $ \pc -> do
    let tcp = Just $ nListenTCP pc
    let udp = Just $ nListenUDP pc
    liftIO $ do
      -- FIXME: local (redirected) only
      insertRule t c Nothing [P (UDP udp), J ACCEPT]
      insertRule t c Nothing [P (TCP tcp), J ACCEPT]

  liftIO $ insertRule t c Nothing (J RETURN)

dnatDNS :: MonadIO m => TableName -> ChainName -> App m ()
dnatDNS t c = do

  cfg <- asks (view config)
  let ncfgs = filter (not.null.nMarks) [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]
  let eth = hIputIface cfg

  liftIO $ do

    insertRule t c Nothing [I eth, J (CONNMARK RESTORE)]

    forM_ ncfgs $ \ncfg -> do
      let tcp = nListenTCP ncfg
      let udp = nListenUDP ncfg

      forM_ (nMarks ncfg) $ \nmark -> do
        let udpRule  = [I eth, P (UDP (Just 53)), MARK (MarkEQ nmark), J (REDIRECT udp)]
        let tcpRule  = [I eth, P (TCP (Just 53)), MARK (MarkEQ nmark), J (REDIRECT tcp)]
        insertRule t c Nothing udpRule
        insertRule t c Nothing tcpRule

    insertRule t c Nothing (J RETURN)

spawn :: MonadIO m => MonadBaseControl IO (App m) => App m () -> App m ()
spawn action = do
  tt <- asks (view actions)
  tid <- asyncThreadId <$> async action
  liftIO $ atomically $ modifyTVar tt ((:) tid)
  return ()


main = do

  cfg <- loadConfig

  runApp cfg $ do

    spawn $ forever $ do
      liftIO $ do
        putStrLn "JOPA"
        threadDelay  $ 5*1000000

    insertChain "filter" "INPUT" (Just 1) acceptDNS
    insertChain "nat" "PREROUTING" Nothing dnatDNS

    liftIO $ threadDelay (600*1000000)

    return ()

