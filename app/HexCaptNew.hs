{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Main where

import Control.Monad.Catch
import Control.Monad.RWS
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Data.Default
import Data.Digest.Pure.SHA
import Data.Digits
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (view)

import HEXCapt.Config
import qualified System.Shell.Iptables as Iptables
import System.Shell.Iptables (ChainName, TableName)

data ChainRef = ChainRef TableName ChainName ChainName
                deriving (Eq,Ord,Show)

data AppEnv = AppEnv { _chainTrack :: TVar (S.Set ChainRef)
                     , _cfg        :: HexCaptCfg
                     }

makeLenses ''AppEnv

data AppState = AppState { _chainNum   :: Int
                         }

makeLenses ''AppState

instance Default AppState where
  def = AppState { _chainNum = 0
                 }

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

runAppT env (App m) = snd <$> execRWST m env def

runApp :: (Functor m, Monad m, MonadIO m, MonadMask m)
       => HexCaptCfg
       -> App m ()
       -> m ()

runApp cfg m = do
  track <- liftIO $ newTVarIO (S.empty)
  let env = AppEnv track cfg
  runAppT env m `finally` (finalizeAll track)
  where
    finalizeAll track = do
      ts <- liftIO $ atomically $ readTVar track
      forM_ ts $ \(ChainRef t c n) -> do
        liftIO $ Iptables.unlinkChain t c n
        liftIO $ Iptables.deleteChain t n

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

trackChain :: (MonadIO m)
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
            -> App m ChainName

insertChain t c mpos = do
  nm <- genChainName t c
  liftIO $ putStrLn  [qq|inserting new chain $nm|]
  liftIO $ Iptables.createChain t nm
  liftIO $ Iptables.insertRule t c mpos ([qq|-j $nm|]::String)
  trackChain t c nm
  return nm

main = do

  cfg <- loadConfig

  runApp cfg $ do

    insertChain "mangle" "PREROUTING" Nothing
    insertChain "mangle" "PREROUTING" Nothing
    insertChain "mangle" "PREROUTING" Nothing
    insertChain "mangle" "PREROUTING" Nothing
    insertChain "mangle" "PREROUTING" Nothing
    insertChain "mangle" "PREROUTING" Nothing

    liftIO $ threadDelay (600*1000000)

    return ()

