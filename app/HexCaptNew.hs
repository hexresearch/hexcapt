{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.Event (Event)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.RWS
import Data.Default
import Data.Digest.Pure.SHA
import Data.Digits
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (view)

import HEXCapt.Config
import qualified System.Shell.Iptables as Iptables
import System.Shell.Iptables

data ChainRef = ChainRef TableName ChainName ChainName
                deriving (Eq,Ord,Show)

data AppEnv = AppEnv { _chainTrack :: TVar (S.Set ChainRef)
                     , _actions    :: TVar [Async ()]
                     , _config     :: HexCaptCfg
                     }

makeLenses ''AppEnv

data AppState = AppState { _chainNum   :: Int
                         }

makeLenses ''AppState

instance Default AppState where
  def = AppState { _chainNum = 0
                 }

newtype App a = App (RWST AppEnv () AppState IO a)
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


-- instance (MonadBase m, MonadIO m) => MonadBaseControl IO (App m) where
--     type StM (App m) a = a

-- instance MonadBaseControl IO App where
--     newtype StM App a = StApp { unStApp :: StM (ErrorT String IO) a }

--     liftBaseWith f = App . liftBaseWith $ \r -> f $ liftM StApp . r . undefined

--     restoreM       = App . restoreM . unStApp


runAppT :: AppEnv -> App () -> IO ()
runAppT env (App m) = snd <$> execRWST m env def

runApp :: (Functor m, Monad m, MonadIO m, MonadMask m)
       => HexCaptCfg
       -> App ()
       -> m ()

runApp cfg m = do
  track <- liftIO $ newTVarIO (S.empty)
  as    <- liftIO $ newTVarIO []
  let env = AppEnv track as cfg
  runAppT env m `finally` (finalizeAll track)
  where
    finalizeAll track = do
      ts <- liftIO $ atomically $ readTVar track
      forM_ ts $ \(ChainRef t c n) -> do
        liftIO $ Iptables.unlinkChain t c n
        liftIO $ Iptables.deleteChain t n

newObj :: Monad m => App Int
newObj = do
  n <- gets (view chainNum)
  modify (over chainNum succ)
  return n

genChainName :: (MonadIO m)
             => TableName
             -> ChainName
             -> App ChainName

genChainName t c = do
  let alph = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :: String
  let alphLen = toInteger $ length alph
  n <- newObj
  let digs = fmap ((!!) alph . fromIntegral) $ digits alphLen $ integerDigest $ sha1 [qq|$t$c$n|]
  return $ "hx" <> (take 20 digs)

trackChain ::   TableName
             -> ChainName
             -> ChainName
             -> App ()
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
  liftIO $ putStrLn  [qq|inserting new chain $nm|]
  liftIO $ Iptables.createChain t nm
  liftIO $ Iptables.insertRule t c mpos ([qq|-j $nm|]::String)
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


spawn :: MonadIO m => m () -> App m ()
spawn action = do
--   a <- liftIO $ async action
  error "FUCK"

main = do

  cfg <- loadConfig

  runApp cfg $ do

    spawn $ forever $ do
      liftIO $ do
        putStrLn "JOPA"
        threadDelay  $ 10*1000000

    insertChain "filter" "INPUT" (Just 1) acceptDNS
    insertChain "nat" "PREROUTING" Nothing dnatDNS

    liftIO $ threadDelay (600*1000000)

    return ()

