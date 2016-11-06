{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
module HEXCapt.Types where


import Control.Concurrent.STM.TVar
import Control.Concurrent (ThreadId)
import Control.Lens
import Control.Monad.Base
import Control.Monad.RWS
import Control.Monad.Trans.Control
import Data.Default
import qualified Data.Set as S
import HEXCapt.Config (HexCaptCfg)
import System.Shell.Iptables (TableName,ChainName)

data ChainRef = ChainRef TableName ChainName ChainName
                deriving (Eq,Ord,Show)

data AppEnv = AppEnv { _chainTrack :: TVar (S.Set ChainRef)
                     , _actions    :: TVar [ThreadId]
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
