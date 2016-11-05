{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language TypeOperators #-}
module Application.HEXCapt.Server where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Attoparsec.Text
import Data.Char (isHexDigit)
import Data.List (nub,sort,intercalate)
import Data.Monoid
import Data.String(fromString)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Control.Concurrent.Event as Event
import Servant
import Servant.Server
import Text.InterpolatedString.Perl6 (qq)

import Network.HEXCapt.API
import HEXCapt.Config
import Application.HEXCapt.Types

newtype CaptServT m a = CaptServT { unServ :: ReaderT AppEnv (ExceptT ServantErr m) a
                                  } deriving ( Functor
                                             , Applicative
                                             , Monad
                                             , MonadReader AppEnv
                                             , MonadError ServantErr
                                             , MonadIO
                                             )

type CaptServ api = ServerT api (CaptServT IO)

runCaptServT :: Monad m => AppEnv -> CaptServT m a -> (ExceptT ServantErr m) a
runCaptServT env m = runReaderT (unServ m) env

captToEither :: AppEnv -> CaptServT IO :~> ExceptT ServantErr IO
captToEither env = Nat $ \x -> runCaptServT env x


macParser :: Parser String
macParser = do
  ms <- count 5 macSep
  ml <- macPart
  return $ intercalate ":" (ms <> [ml])

  where macPart = count 2 (satisfy (isHexDigit))
        macSep  = do
          m <- macPart
          char ':'
          return m


runAPI :: AppEnv -> IO ()
runAPI env = do
  let cfg = view config env

  let port = hListen cfg
  let bind = hBind cfg

  let settings = (setPort port  . setHost (fromString bind)) defaultSettings

  runSettings settings (serve (Proxy :: Proxy HEXCaptAPI) (serveAPI env))

serveAPI :: AppEnv -> Server HEXCaptAPI
serveAPI env = enter (captToEither env) serveSetAccess

serveSetAccess :: CaptServ HEXCaptAPI
serveSetAccess (Just mac') (Just mark) = do
  case (parseOnly macParser mac')  of
    Left _  -> throwError err500
    Right m -> do
      env <- ask
      liftIO $ updateMarkIO env m mark
      return [qq|set access $m $mark ok|]

serveSetAccess _ _ = throwError err404
