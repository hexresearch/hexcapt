{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
module HEXCapt.Config where

import Control.Exception (tryJust)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Char (isSpace,isHexDigit)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.List (nub)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Typeable
import Data.Yaml
import GHC.Generics
import qualified Control.Foldl as Fold
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO.Error (isDoesNotExistError)
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (Parser)

import Debug.Trace

import HEXCapt.Types

data Filter = DstAddr String
              deriving (Show, Eq, Generic, Data, Typeable)

data Redirect = Redirect { redirProto :: String
                         , redirFrom  :: Int
                         , redirTo    :: Int
                         , redirFilt  :: [Filter]
                         } deriving (Show, Eq, Generic, Data, Typeable)

data Forward = Forward String Int
                deriving (Show, Eq, Generic, Data, Typeable)

data NDNProxyCfg = NDNProxyCfg { nListenTCP :: Int
                               , nListenUDP :: Int
                               , nMarks     :: [Int]
                               , nUpstreamDNS :: [DNSServer]
                               , nStaticA :: [StaticAddr]
                               , nStaticTTL :: Maybe Int
                               } deriving (Show,Eq,Generic,Data,Typeable)

data HexCaptCfg = HexCaptCfg { hIputIface :: String
                             , hBind    :: String
                             , hListen  :: Int
                             , hSleep   :: Double
                             , hLocalIp :: String
                             , hModes   :: [HexCaptMode]
                             , ndnproxy :: [NDNProxyCfg]
                             } deriving (Show,Eq,Generic,Data,Typeable)


data HexCaptMode = HexCaptMode { hmMarks :: [Int]
                               , hmLocalDomains :: [String]
                               , hmRedirects :: [Redirect]
                               , hmFwd :: [Forward]
                               } deriving (Show,Eq,Generic,Data,Typeable)

data HexCaptModeList = HexCaptModeList [HexCaptMode]
                       deriving Show

newtype NDNProxyCfgList = NDNProxyCfgList [NDNProxyCfg]
                          deriving (Show)

instance FromJSON HexCaptModeList where
  parseJSON (Object v) = HexCaptModeList <$> v .: "modes"
  parseJSON _          = mzero

instance FromJSON HexCaptMode where
  parseJSON (Object v) = HexCaptMode
                           <$> ((v .: "mode") >>= (.: "marks"))
                           <*> ((v .: "mode") >>= (.: "local-domains"))
                           <*> ((v .: "mode") >>= (.:? "redirect") >>= parseRedirects)
                           <*> ((v .: "mode") >>= (.:? "forward") >>= parseForwards)
  parseJSON _ = mzero

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
                           <*> ((v .: "instance") >>= (.:? "static-ttl"))

  parseJSON _          = mzero

parseForwards :: Maybe Value -> Parser [Forward]
parseForwards (Just v) = do
  vs <- parseJSON v :: Parser [(String, Int)]
  return $ fmap (\(p,f) -> Forward p f) vs

parseForwards _ = return []

parseRedirects :: Maybe Value -> Parser [Redirect]
parseRedirects (Just v) = mconcat <$> (parseJSON v >>= mapM pr)

  where
    pr :: [Value] -> Parser [Redirect]
    pr [proto, p1, p2] = do
      pproto <- parseJSON proto :: Parser String
      pp1    <- parseJSON p1 :: Parser Int
      pp2    <- parseJSON p2 :: Parser Int
      return $ [Redirect pproto pp1 pp2 []]

    pr [proto, p1, p2, rules] = do
      redir <- pr [proto, p1, p2]
      fs    <- (parseJSON rules :: Parser [(String, String)]) >>= mapM parseFilter
      return [ Redirect pp ps pt fs | (Redirect pp ps pt _) <- redir ]

    pr _ = mzero

    parseFilter :: (String, String) -> Parser Filter
    parseFilter ("dst", ip) = return (DstAddr ip)
    parseFilter _ = mzero


parseRedirects _ = return []

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
      iface   <- v .: "input-iface" :: Parser String
      bind    <- v .: "bind"        :: Parser String
      listen  <- v .: "listen"      :: Parser Int
      localIp <- v .: "local-ip"    :: Parser String
      zzz     <- v .:? "sleep"      :: Parser (Maybe Double)

      (NDNProxyCfgList ps) <- parseJSON o :: Parser NDNProxyCfgList
      (HexCaptModeList ms) <- parseJSON o :: Parser HexCaptModeList

      -- common domains for all ndnproxy instances
      let lds = [ (Nothing, mkAddrs localIp ld) | HexCaptMode { hmMarks = [], hmLocalDomains = ld } <- ms ]

      -- per mark domain list
      let mds = concat [ mkDict all localIp ld | HexCaptMode { hmMarks = all@(m:ms), hmLocalDomains = ld } <- ms ]

      let dmap = fmap (foldMap expandDomain) $ M.fromList (lds <> mds)

      let ps' = fmap (\cfg -> cfg { nStaticA = nStaticA cfg <> getDomains (nMarks cfg) dmap } ) ps

      return $ HexCaptCfg iface
                          bind
                          listen
                          (fromMaybe 1.0 zzz)
                          localIp
                          ms
                          ps'

    where
      mkAddrs :: String -> [String] -> [StaticAddr]
      mkAddrs ip as = zipWith StaticAddr as (repeat ip)

      mkDict :: [Int] -> String -> [String] -> [(Maybe Int, [StaticAddr])]
      mkDict ms ip as = fmap (\m -> (Just m, mkAddrs ip as)) ms

      getDomains :: [Int] -> M.Map (Maybe Int) [StaticAddr] -> [StaticAddr]
      getDomains ms mm = concat $ catMaybes $ M.lookup Nothing mm : fmap (flip M.lookup mm . Just) ms

      expandDomain :: StaticAddr -> [StaticAddr]
      expandDomain a@(StaticAddr dom ip) = catMaybes [Just a, flip StaticAddr ip <$> L.stripPrefix "*." dom]

  parseJSON _ = mzero


instance NDNProxyConfig NDNProxyCfg where
  ndnpListenTCP = nListenTCP
  ndnpListenUDP = nListenUDP

  ndnpStaticA = fmap (\(StaticAddr a b) -> (a,b)) . nStaticA

  ndnpStaticTTL = nStaticTTL

  ndnpUpstreamDNS c = fmap trdns (nUpstreamDNS c)
    where trdns (DNSServer Nothing s)  = (s,".")
          trdns (DNSServer (Just p) s) = (s,p)


ndnproxyConfigAsText :: NDNProxyConfig cfg => cfg -> Text
ndnproxyConfigAsText c = str
  where
    dnsServers = T.intercalate "\n" $ map dnss (ndnpUpstreamDNS c)
    dnss (s,d) = T.concat ["dns_server = ", (T.pack s), " ", (T.pack d)]

    staticTTL :: String
    staticTTL = maybe "" (\ttl -> [qq|static_ttl = $ttl|]) (ndnpStaticTTL c)

    staticA = T.intercalate "\n" $ map a (ndnpStaticA c)
    a (d,addr) = T.concat ["static_a = ", (T.pack d), " ", (T.pack addr)]

    str = [qc|
rpc_port = 0
timeout = 5000
bantime = 120000
proceed = 1000

ban_threshold = {ndnpBanTreshhold c}

{dnsServers}

{staticA}
{staticTTL}

dns_tcp_port = {ndnpListenTCP c}
dns_udp_port = {ndnpListenUDP c}
|]


data RNorm = RNorm { resolvConf :: [DNSServer] }

loadConfig :: IO HexCaptCfg
loadConfig = do
  homePath' <-  tryJust (guard . isDoesNotExistError) home

  let homePath = case homePath' of
                  Left _  -> ""
                  Right p -> p

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

  return cfg


  where
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

      let marks = concat $ filter (not.null) [ nm | (NDNProxyCfg{nMarks = nm}) <- universeBi cfg' ]

      when (length marks /= length (S.fromList marks)) $ do
        die $ "Marks must be unique for each ndnproxy instance"

      return cfg'

      where
        tr (a@NDNProxyCfg { nUpstreamDNS = [] }) = do
          res <- asks resolvConf
          return $ Just (a { nUpstreamDNS = res } )

        tr _ = return Nothing

        parseNs :: Text -> Maybe DNSServer
        parseNs s = case T.split isSpace s of
                      ("nameserver":dns:_) -> Just $ DNSServer Nothing (T.unpack dns)
                      _                    -> Nothing
