 {-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception
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
import System.Process hiding (shell)
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (Parser)


import Debug.Trace

class NDNProxyConfig a where
  ndnpBanTreshhold :: a -> Int
  ndnpBanTreshhold _ = 10
  ndnpListenUDP :: a -> Int
  ndnpListenTCP :: a -> Int
  ndnpListenTCP  = ndnpListenUDP
  ndnpUpstreamDNS :: a -> [(String,String)]

  ndnpStaticA :: a -> [(String,String)]
  ndnpStaticA _ = []


data DNSServer = DNSServer (Maybe String) String
                 deriving (Show,Eq,Generic,Data,Typeable)


data StaticAddr = StaticAddr String String
                  deriving (Show,Eq,Generic,Data,Typeable)

data NDNProxyCfg = NDNProxyCfg { nListenTCP :: Int
                               , nListenUDP :: Int
                               , nMarks     :: [Int]
                               , nUpstreamDNS :: [DNSServer]
                               , nStaticA :: [StaticAddr]
                               } deriving (Show,Eq,Generic,Data,Typeable)

data HexCaptCfg = HexCaptCfg { hIputIface :: String
                             , ndnproxy :: [NDNProxyCfg]
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
                           <*> ((v .: "instance") >>= (.:? "static-a") >>= parseStaticAddr)

  parseJSON _          = empty


parseUpstreamDNS :: Maybe Value -> Parser [DNSServer]

parseUpstreamDNS (Just o) = do
  mdns <- parseJSON o :: Parser (M.Map String [String])
  return $ foldMap (\(k,v) -> fmap (DNSServer (Just k)) v) (M.toList mdns)

parseUpstreamDNS Nothing = return []

parseStaticAddr :: Maybe Value -> Parser [StaticAddr]

parseStaticAddr (Just o) = do
  maddr <- parseJSON o :: Parser [(String,String)]
  return $ fmap (uncurry StaticAddr) maddr

parseStaticAddr Nothing = mzero


instance FromJSON HexCaptCfg where
  parseJSON o@(Object v) = do
      iface <- v .: "inputIface" :: Parser String
      (NDNProxyCfgList ps) <- parseJSON o :: Parser NDNProxyCfgList
      return $ HexCaptCfg iface ps

  parseJSON _ = empty


ndnproxyConfigAsText :: NDNProxyConfig cfg => cfg -> Text
ndnproxyConfigAsText c = str
  where
    dnsServers = T.intercalate "\n" $ map dnss (ndnpUpstreamDNS c)
    dnss (s,d) = T.concat ["dns_server = ", (T.pack s), " ", (T.pack d)]

    staticA = T.intercalate "\n" $ map a (ndnpStaticA c)
    a (d,addr) = T.concat ["static_a = ", (T.pack d), " ", (T.pack addr)]

    str = [qc|
rpc_port = 0
timeout = 7001
bantime = 300000
proceed = 500
ban_threshold = {ndnpBanTreshhold c}

{dnsServers}

{staticA}

dns_tcp_port = {ndnpListenTCP c}
dns_udp_port = {ndnpListenUDP c}
|]


instance NDNProxyConfig NDNProxyCfg where
  ndnpListenTCP = nListenTCP
  ndnpListenUDP = nListenUDP
  ndnpStaticA   = fmap (\(StaticAddr a b) -> (a,b)) . nStaticA
  ndnpUpstreamDNS c = fmap trdns (nUpstreamDNS c)
    where trdns (DNSServer Nothing s)  = (s,".")
          trdns (DNSServer (Just p) s) = (s,p)

procNDNProxy :: (MonadIO io, NDNProxyConfig cfg) => cfg -> io ()
procNDNProxy cfg = sh $ do
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


procDnsDnat:: HexCaptCfg -> IO ()
procDnsDnat cfg = runDNAT
  where
    runDNAT = do

      e <- checkDnsDnat cfg
      when e $ dropDnsDnat cfg

      forever $ do
        exists <- checkDnsDnat cfg

        when (not exists) $ do
          createDnsDnat cfg

        sleep 2.0


dnsDNAT_TABLE :: String
dnsDNAT_TABLE = "HEXCAPTDNSDNAT"

checkDnsDnat :: HexCaptCfg -> IO Bool
checkDnsDnat cfg = do
  let checkCmd = grep (prefix (fromString dnsDNAT_TABLE)) $ inshell "iptables -t nat -L PREROUTING 2>/dev/null" ""
  s <- fold checkCmd Fold.head
  return (isJust s)

createDnsDnat :: HexCaptCfg -> IO ()
createDnsDnat cfg = do
  putStrLn "createDnsDnat"

  let ncfgs = [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]

  let eth = hIputIface cfg

  shell [qq|iptables -t nat -N $dnsDNAT_TABLE 2>/dev/null|] mzero
  shell [qq|iptables -t nat -A $dnsDNAT_TABLE -i $eth -j CONNMARK --restore-mark 2>/dev/null|] mzero

  forM_ ncfgs $ \ncfg -> do
    let tcp = nListenTCP ncfg
    let udp = nListenUDP ncfg

    putStrLn [qq|ndnproxy instance|]

    forM_ (nMarks ncfg) $ \nmark -> do
      shell [qq|iptables -t nat -A $dnsDNAT_TABLE -i $eth -p udp --dport 53 -m mark --mark $nmark -j REDIRECT --to-port $udp|] mzero

    forM_ (nMarks ncfg) $ \nmark -> do
      shell [qq|iptables -t nat -A $dnsDNAT_TABLE -i $eth -p tcp --dport 53 -m mark --mark $nmark -j REDIRECT --to-port $tcp|] mzero

  shell [qq|iptables -t nat -A $dnsDNAT_TABLE -j RETURN 2>/dev/null|] mzero

  shell [qq|iptables -t nat -A PREROUTING -j $dnsDNAT_TABLE 2>/dev/null|] mzero

  return ()

dropDnsDnat :: HexCaptCfg -> IO ()
dropDnsDnat cfg = do
  putStrLn "DROP DNAT"
  shell [qq|iptables -t nat -D PREROUTING -j $dnsDNAT_TABLE|] mzero
  shell [qq|iptables -t nat -F $dnsDNAT_TABLE|] mzero
  shell [qq|iptables -t nat -X $dnsDNAT_TABLE|] mzero
  return ()

main :: IO ()
main = do

  -- TODO:
  --
  --
  -- + set upstream dns (from resolv.conf)
  -- + set upstream dns from config (override)
  -- + set local domains (from config)
  -- + set local ndnproxy ports per instance
  -- + start ndnproxy instances, sufficient amount

  -- TODO
  -- + 1) start
  -- + 2)    launch ndnproxy instances
  -- + 2.1)  generate configurations for ndnproxy
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


  finally (mainLoop cfg) (cleanup cfg)

  return ()

  where

    mainLoop cfg = do
      async (procDnsDnat cfg)
      -- TODO: wait ??
      let ncfgs = [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]
      mapConcurrently (procNDNProxy) ncfgs

    cleanup cfg = do
      putStrLn "DO CLEANUP"
      dropDnsDnat cfg

