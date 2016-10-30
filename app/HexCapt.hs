{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Char (isSpace)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.List (nub,sort)
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Data.Yaml
import GHC.Generics
import qualified Control.Foldl as Fold
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Text.InterpolatedString.Perl6 (qc,qq)
import Turtle hiding (Parser)


import Debug.Trace

class NDNProxyConfig a where
  ndnpBanTreshhold :: a -> Int
  ndnpBanTreshhold _ = 10
  ndnpListenUDP :: a -> Int
  ndnpListenTCP :: a -> Int
  ndnpListenTCP  = ndnpListenUDP
  ndnpUpstreamDNS :: a -> [(String,String)]


data DNSServer = DNSServer (Maybe String) String
                 deriving (Show,Eq,Generic,Data,Typeable)


data NDNProxyCfg = NDNProxyCfg { nListenTCP :: Int
                               , nListenUDP :: Int
                               , nMarks     :: [Int]
                               , nUpstreamDNS :: [DNSServer]
                               } deriving (Show,Eq,Generic,Data,Typeable)

data HexCaptCfg = HexCaptCfg { ndnproxy :: [NDNProxyCfg]
                             } deriving (Show,Eq,Generic,Data,Typeable)


newtype NDNProxyCfgList = NDNProxyCfgList [NDNProxyCfg]
                          deriving (Show)

instance FromJSON NDNProxyCfgList where
  parseJSON (Object v) = NDNProxyCfgList <$> v .: "ndnproxy"
  parseJSON _          = mzero

instance FromJSON DNSServer where

instance FromJSON NDNProxyCfg where
  parseJSON (Object v) = NDNProxyCfg
                           <$> ((v .: "instance") >>= (.: "tcp-port"))
                           <*> ((v .: "instance") >>= (.: "udp-port"))
                           <*> ((v .: "instance") >>= (.: "marks"))
                           <*> ((v .: "instance") >>= (.:? "upstream-dns") >>= parseUpstreamDNS)

  parseJSON _          = empty


parseUpstreamDNS :: Maybe Value -> Parser [DNSServer]

parseUpstreamDNS (Just o) = do
  mdns <- parseJSON o :: Parser (M.Map String [String])
  return $ foldMap (\(k,v) -> fmap (DNSServer (Just k)) v) (M.toList mdns)

parseUpstreamDNS Nothing = return []


instance FromJSON HexCaptCfg where
  parseJSON o@(Object _) = do
      (NDNProxyCfgList ps) <- parseJSON o :: Parser NDNProxyCfgList
      return $ HexCaptCfg ps

  parseJSON _ = empty


ndnproxyConfigAsText :: NDNProxyConfig cfg => cfg -> Text
ndnproxyConfigAsText c = str
  where
    dnsServers = T.intercalate "\n" $ map dnss (ndnpUpstreamDNS c)
    dnss (s,d) = T.concat ["dns_server = ", (T.pack s), " ", (T.pack d)]

    str = [qc|
rpc_port = 0
timeout = 7001
bantime = 300000
proceed = 500
ban_threshold = {ndnpBanTreshhold c}

{dnsServers}

dns_tcp_port = {ndnpListenTCP c}
dns_udp_port = {ndnpListenUDP c}
|]


instance NDNProxyConfig () where
  ndnpListenTCP _ = 10053
  ndnpListenUDP _ = 10053
  ndnpUpstreamDNS _ = [ ("8.8.8.8", ".")
                      , ("8.8.4.4", ".")
                      ]


instance NDNProxyConfig NDNProxyCfg where
  ndnpListenTCP = nListenTCP
  ndnpListenUDP = nListenUDP
  ndnpUpstreamDNS c = fmap trdns (nUpstreamDNS c)
    where trdns (DNSServer Nothing s)  = (s,".")
          trdns (DNSServer (Just p) s) = (s,p)

procNDNProxy :: (MonadIO io, NDNProxyConfig cfg) => cfg -> io ()
procNDNProxy cfg = forever $ sh $ do
    fname' <- mktempfile "/tmp" "ndnproxy-conf"
    output fname' (return (ndnproxyConfigAsText cfg))
    let fname = format fp fname'
    stdout (return fname)
    stdout (return "\n")
    procs "ndnproxy" ["-c", fname] ""


data RNorm = RNorm { resolvConf :: [DNSServer] }

normalizeConfig :: MonadIO io => HexCaptCfg -> io HexCaptCfg
normalizeConfig cfg = do

  let resolvConfName = "/etc/resolv.conf"
  rexists <- testfile resolvConfName

  rdns <- if rexists
            then do
              sss <- fold (grep (prefix "nameserver") (input resolvConfName)) Fold.list
              return $ nub $ catMaybes $ fmap parseNs sss

            else return []


  let rn = RNorm { resolvConf = rdns }

  let cfg' = runReader (rewriteBiM tr cfg) rn

  let marks = concat [ nm | (NDNProxyCfg{nMarks = nm}) <- universeBi cfg' ]

  when (length marks /= length (S.fromList marks)) $ do
    die $ "Marks must be unique for each ndnproxy instance"

  return cfg'

  where
    tr (a@NDNProxyCfg { nMarks = [] }) = return $ Just (a { nMarks = [0] } )

    tr (a@NDNProxyCfg { nUpstreamDNS = [] }) = do
      res <- asks resolvConf
      return $ Just (a { nUpstreamDNS = res } )

    tr _ = return Nothing

    parseNs :: Text -> Maybe DNSServer
    parseNs s = case T.split isSpace s of
                  ("nameserver":dns:_) -> Just $ DNSServer Nothing (T.unpack dns)
                  _                    -> Nothing


main :: IO ()
main = do

  -- TODO:
  --
  --
  -- set upstream dns (from resolv.conf)
  -- set upstream dns from config (override)
  -- set local domains (from config)
  -- set local ndnproxy ports per instance
  -- start ndnproxy instances, sufficient amount


  -- TODO
  -- 1) start
  -- 2)    launch ndnproxy instances
  -- 2.1)  generate configurations for ndnproxy
  -- 3) insert iptable rules
  -- 4) serve API
  -- 5) keep mac address settings
  -- 6) re-generate iptable rules on change mac addr. status

  homePath <- home
  let hexCaptYaml = "hexcapt.yaml"
  let etcCfg = "/etc" </> hexCaptYaml
  let homeCfg = homePath </> hexCaptYaml
  let localCfg = hexCaptYaml

  let locations = [localCfg, homeCfg, etcCfg]

  cfgFname' <- listToMaybe <$> filterM testfile locations

  cfgFname <- case cfgFname' of
                Just p -> return p
                _      -> die "Config file not found"


  mcfg <- decodeFile (T.unpack $ format fp cfgFname) :: IO (Maybe HexCaptCfg)

  cfg <- case mcfg of
           Nothing -> die $ format ("Can't parse config "%fp ) cfgFname
           Just c  -> normalizeConfig c


  let ncfgs = [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]

  mapConcurrently (procNDNProxy) ncfgs

  return ()


