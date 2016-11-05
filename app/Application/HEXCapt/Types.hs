{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
module Application.HEXCapt.Types where


import Control.Concurrent.Event (Event)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent (ThreadId)
import Control.Lens
import Control.Monad.Base
import Control.Monad.RWS
import Control.Monad.Trans.Control
import Data.Default
import qualified Control.Concurrent.Event as Event
import qualified Data.Map as M
import qualified Data.Set as S

import HEXCapt.Config (HexCaptCfg)
import System.Shell.Iptables (TableName,ChainName)

type MAC = String

data ChainRef = ChainRef TableName ChainName ChainName
                deriving (Eq,Ord,Show)

data AppEnv = AppEnv { _chainTrack :: TVar (S.Set ChainRef)
                     , _actions    :: TVar [ThreadId]
                     , _marks      :: TVar (M.Map MAC Int)
                     , _marksSeq   :: TVar Bool
                     , _event      :: Event
                     , _config     :: HexCaptCfg
                     }

makeLenses ''AppEnv

data AppState = AppState { _chainNum   :: Int
                         }

makeLenses ''AppState

instance Default AppState where
  def = AppState { _chainNum = 0
                 }

newtype App m a = App { unApp :: RWST AppEnv () AppState m a }
                  deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadReader AppEnv
                           , MonadState  AppState
                           , MonadWriter ()
                           , MonadRWS AppEnv () AppState
                           , MonadTrans
                           , MonadIO
                           )

instance MonadBase b m => MonadBase b (App m) where
  liftBase = liftBaseDefault

instance MonadTransControl App where
  type StT App a = StT (RWST AppEnv () AppState) a
  liftWith = defaultLiftWith App unApp
  restoreT = defaultRestoreT App

instance MonadBaseControl b m => MonadBaseControl b (App m) where
  type StM (App m) a = ComposeSt App m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

runAppT :: (Monad m)
        => AppEnv
        -> App m ()
        -> m ()

runAppT env (App m) = snd <$> execRWST m env def

isDirty :: MonadIO m => App m Bool
isDirty = do
  tseq <- asks (view marksSeq)
  d <- liftIO $ atomically $ readTVar tseq
  return d

clearDirty :: MonadIO m => App m ()
clearDirty = do
  tseq <- asks (view marksSeq)
  liftIO $ atomically $ modifyTVar' tseq (const False)

updateMarkIO :: AppEnv -> MAC -> Int -> IO ()
updateMarkIO env mac mark = do

  let tmarks = view marks env
  let tseq = view marksSeq env
  let ev = view event env

  atomically $ do
    modifyTVar' tseq (const $ True)
    modifyTVar' tmarks (M.alter alt mac)

  Event.signal ev

  where
    alt v = case (v,mark) of
      (_, 0) -> Nothing
      (_, n) -> Just n



