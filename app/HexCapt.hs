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

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Event (Event)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Concurrent (ThreadId, killThread)
import Control.Exception.Lifted
import Control.Lens
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Trans.Control
import Data.Default
import Data.Digest.Pure.SHA
import Data.Digits
import Data.Generics.Uniplate.Operations
import Data.List (nub,sort)
import qualified Control.Concurrent.Event as Event
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import Servant.Server
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (view)

import HEXCapt.Types
import HEXCapt.Config
import Application.HEXCapt.Types
import Application.HEXCapt.Server
import Application.HEXCapt.NDNProxy

import qualified System.Shell.Iptables as Iptables
import System.Shell.Iptables

runApp :: (MonadIO m, MonadBaseControl IO (App m))
       => HexCaptCfg
       -> App m ()
       -> m ()

runApp cfg m = do
  track <- liftIO $ newTVarIO (S.empty)
  as    <- liftIO $ newTVarIO []
  ms    <- liftIO $ newTVarIO mempty
  mseq  <- liftIO $ newTVarIO True
  ev    <- liftIO $ Event.new

  let env = AppEnv track as ms mseq ev cfg
  runAppT env (m `finally` finalize)
  where
    finalize = do
      ts <- asks (view chainTrack) >>= liftIO . (atomically . readTVar)
      as <- asks (view actions) >>= liftIO . (atomically . readTVar)

      liftIO $ mapM_ killThread as

      forM_ ts $ \(ChainRef t c n) -> do
        liftIO $ do
          putStrLn [qq|delete $t $c $n|]
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
  liftIO $ atomically $ modifyTVar' tv $ S.insert (ChainRef t c n)


untrackChain :: MonadIO m
             => TableName
             -> ChainName
             -> ChainName
             -> App m ()
untrackChain t c n = do
  tv <- asks (view chainTrack)
  liftIO $ atomically $ modifyTVar' tv $ S.delete (ChainRef t c n)

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

removeChain :: MonadIO m
            => TableName
            -> ChainName
            -> ChainName
            -> App m ()
removeChain t c n = do

  liftIO $ do
    Iptables.unlinkChain t c n
    Iptables.deleteChain t n

  untrackChain t c n

emptyChain :: MonadIO m => TableName -> ChainName -> App m ()
emptyChain t c = do
  liftIO $ Iptables.insertRule t c Nothing (J RETURN)
  return ()

protoOf :: String -> Int -> [IPTablesOpt]
protoOf "tcp" p = [P (TCP (Just p))]
protoOf "udp" p = [P (UDP (Just p))]
protoOf _ _     = []

acceptPorts :: MonadIO m => TableName -> ChainName -> App m ()
acceptPorts t c = do

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

  let redirs = nub $ sort $ [ (p,t) | (Redirect{redirProto = p, redirTo = t}) <- universeBi cfg ]
  forM_  redirs $ \(p,to) -> do
    let pp = protoOf p to
    let st = case p of
               "tcp" -> [STATE [NEW]]
               _     -> []
    let acc = [J ACCEPT]
    let rules = mconcat [pp, st, acc]

    liftIO $ insertRule t c Nothing rules

  liftIO $ insertRule t c Nothing (J RETURN)

dnatDNS :: MonadIO m => TableName -> ChainName -> App m ()
dnatDNS t c = do

  cfg <- asks (view config)
  let ncfgs = filter (not.null.nMarks) [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]
  let eth = hIputIface cfg

  liftIO $ do

    insertRule t c Nothing [J (CONNMARK RESTORE)]

    forM_ ncfgs $ \ncfg -> do
      let tcp = nListenTCP ncfg
      let udp = nListenUDP ncfg

      forM_ (nMarks ncfg) $ \nmark -> do
        let udpRule  = [I eth, P (UDP (Just 53)), MARK (MarkEQ nmark), J (REDIRECT udp)]
        let tcpRule  = [I eth, P (TCP (Just 53)), MARK (MarkEQ nmark), J (REDIRECT tcp)]
        insertRule t c Nothing udpRule
        insertRule t c Nothing tcpRule

    insertRule t c Nothing (J RETURN)

dnatExtra :: MonadIO m => TableName -> ChainName -> App m ()
dnatExtra t c = do
  cfg <- asks (view config)
  let eth = hIputIface cfg

  liftIO $ putStrLn [qq|Create extra DNAT|]

  let modes = [ m | m@(HexCaptMode{hmMarks=(_:_)}) <- universeBi cfg ]

  liftIO $ insertRule t c Nothing [J (CONNMARK RESTORE)]

  forM_ modes $ \mo -> do

    let ms = hmMarks mo
    let rr = hmRedirects mo

    forM_ [ (m,r) | m <- ms, r <- rr ] $ \(m,r) -> do
      case r of
        Redirect pp from to fs -> do
          let iface  = [I eth]
              proto  = protoOf pp from
              filts  = fmap filterOf fs
              mark   = [MARK (MarkEQ m)]
              target = [J (REDIRECT to)]
              rule   = mconcat [iface, proto, filts, mark, target]

          liftIO $ insertRule t c Nothing rule

  liftIO $ insertRule t c Nothing (J RETURN)

  where
    filterOf (DstAddr ip) = D ip

fwdFilter :: MonadIO m => TableName -> ChainName -> App m ()
fwdFilter t c = do

  cfg <- asks (view config)
  let eth = hIputIface cfg

  liftIO $ putStrLn [qq|Create FORWARD|]

  liftIO $ insertRule t c Nothing [J (CONNMARK RESTORE)]

  let modes = [ m | m@(HexCaptMode{hmMarks=(_:_)}) <- universeBi cfg ]

  forM_ modes $ \mo -> do

    let ms = hmMarks mo
    let rr = hmFwd mo

    forM_ [ (m,r) | m <- ms, r <- rr ] $ \(m,r) -> do
      case r of
        Forward "tcp" p -> do
          let rule = [I eth, P (TCP (Just p)), MARK (MarkEQ m), J ACCEPT]
          liftIO $ insertRule t c Nothing rule

        Forward "udp" p -> do
          let rule = [I eth, P (UDP (Just p)), MARK (MarkEQ m), J ACCEPT]
          liftIO $ insertRule t c Nothing rule

        Forward s _ -> liftIO $ putStrLn [qq|Unknown protocol: $s |]

  liftIO $ do
    insertRule t c Nothing [P (TCP Nothing), CTSTATE [RELATED,ESTABLISHED], J ACCEPT]
    insertRule t c Nothing (J DROP)
    insertRule t c Nothing (J RETURN)

  return ()

spawn :: MonadIO m => MonadBaseControl IO (App m) => App m () -> App m ()
spawn action = do
  tt <- asks (view actions)
  tid <- asyncThreadId <$> async action
  liftIO $ atomically $ modifyTVar tt ((:) tid)
  return ()


updateMangle :: MonadIO m => TVar [ChainName] -> App m ()
updateMangle track = do
  d <- isDirty

  when d $ do
    liftIO $ putStrLn "updating mangle"

    cfg <- asks (view config)
    let iface = hIputIface cfg
    tms <- asks (view marks)
    ms <- liftIO $ atomically $ readTVar tms
    liftIO $ mapM_ print (M.toList ms)

    insertChain "mangle" "PREROUTING" Nothing $ \t c -> do
      liftIO $ putStrLn [qq|create new mangle table $t $c|]

      forM_ (M.toList ms) $ \(mac,mark) -> do
        let rule = [I iface, MAC (MacEQ (MacSrc mac)), (J (SETMARK mark))]
        liftIO $ insertRule t c Nothing rule

      liftIO $ insertRule t c Nothing [J (CONNMARK SAVE)]
      liftIO $ insertRule t c Nothing [J RETURN]

      tracked <- liftIO $ atomically $ do
        r <- readTVar track
        modifyTVar' track (const [])
        return r

      mapM_ (removeChain "mangle" "PREROUTING") tracked

      liftIO $ atomically $ modifyTVar' track ((:) c)

    clearDirty

main = do

  cfg <- loadConfig
  let zzz = hSleep cfg

  runApp cfg $ do

    ev <- asks (view event)

    spawn $ do
      env <- ask
      liftIO $ runAPI env

    let ncfgs = [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]
    forM_ ncfgs $ \ncfg -> spawn $ do
      liftIO $ procNDNProxy ncfg

    insertChain "filter" "INPUT" (Just 1) acceptPorts
    insertChain "nat" "PREROUTING" Nothing dnatDNS
    insertChain "nat" "PREROUTING" Nothing dnatExtra
    insertChain "filter" "FORWARD" Nothing fwdFilter

    trackMangle <- liftIO $ newTVarIO []
    forever $ do
        updateMangle trackMangle
        liftIO $ threadDelay (1*1000000)
        liftIO $ Event.waitTimeout ev (floor (zzz*1000000))

    return ()

