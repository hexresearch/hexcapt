{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Data.Yaml (decodeFile)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Options.Applicative
import Prelude hiding (FilePath)
import qualified Data.Text as T
import Servant.Client
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (Parser,option)

import Network.HEXCapt.API


data ClientOpts = ClientOpts { clHexCaptURL :: Maybe String
                             , clMac        :: String
                             , clMark       :: Int
                             } deriving (Show)

clientOpts = info (helper <*> opts)
              (    fullDesc
               <> progDesc "hexcapt client utility"
               <> header "hexcapt-client")
  where
    opts = ClientOpts <$> (optional $ strOption ( long "url"  <> help "hexcapt API URL"))
                      <*> (strOption (long "mac"  <> help "MAC address to set acess"))
                      <*> (option auto ( long "access"  <> help "access mark"))


main :: IO ()
main = do

  args  <- execParser clientOpts

  homePath <- home
  let hexCaptYaml = "hexcapt.yaml"
  let etcCfg = "/etc" </> hexCaptYaml
  let homeCfg = homePath </> hexCaptYaml
  let localCfg = hexCaptYaml

  let locations = [localCfg, homeCfg, etcCfg]

  cfgFname <- listToMaybe <$> filterM testfile locations

  defUrl <- case cfgFname of
    Nothing    -> return Nothing
    Just cname -> do
      mbv <- decodeFile (T.unpack $ format fp cname) :: IO (Maybe Value)
      case mbv of
        Nothing -> return Nothing
        Just v -> do
          let listen = (v ^? key "listen" . _Integer) :: Maybe Integer
          let bind   = (v ^? key "bind" . _String) :: Maybe Text
          case (listen, bind) of
            (Just l, Just "0.0.0.0") -> return $ Just [qq|http://localhost:$l|]
            (Just l, Just b) -> return $ Just [qq|http://$b:$l|]
            _ -> return Nothing


  surl <- maybe (die "service URL not set)") return  (clHexCaptURL args <|> defUrl)

  r <- runExceptT $ do
    manager <- liftIO $ newManager defaultManagerSettings
    url <- parseBaseUrl surl
    client (Proxy :: Proxy HEXCaptAPI) (Just (T.pack $ clMac args)) (Just (clMark args)) manager url

  case r of
    Left s  -> die (T.pack $ show s)
    Right r -> putStrLn r

  return ()

