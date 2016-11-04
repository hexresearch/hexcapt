{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Event (Event)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Lens hiding (has)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Char (isSpace,isHexDigit)
import Data.Data
import Data.Default
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.List (nub,sort,intercalate)
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Data.Yaml
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Control.Concurrent.Event as Event
import qualified Control.Foldl as Fold
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Servant
import Servant.Server
import System.Process hiding (shell)
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Text.Read
import Turtle hiding (Parser,view)

import Network.HEXCapt.API

import Debug.Trace

type MAC = String

data MacDB = MacDB { _mdbVersion :: Int
                   , _mdbMarks   :: M.Map MAC Int
                   }

makeLenses ''MacDB

instance Default MacDB where
  def = MacDB 1 M.empty

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
                             , hBind    :: String
                             , hListen  :: Int
                             , hSleep   :: Double
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
      iface  <- v .: "input-iface" :: Parser String
      bind   <- v .: "bind" :: Parser String
      listen <- v .: "listen" :: Parser Int
      zzz    <- v .:? "sleep" :: Parser (Maybe Double)

      locald <- (v .:? "local-domains") >>= parseStaticAddr

      (NDNProxyCfgList ps) <- parseJSON o :: Parser NDNProxyCfgList

      let ps' = fmap (\cfg -> cfg { nStaticA = nStaticA cfg <> locald} ) ps

      return $ HexCaptCfg iface bind listen (fromMaybe 1.0 zzz) ps'

  parseJSON _ = empty

macDbUpdate :: MAC -> Int -> MacDB -> MacDB
macDbUpdate mac mark db = (over mdbVersion succ . set mdbMarks marks') db
  where
    marks' = M.alter alt mac (db ^. mdbMarks)

    alt v = case (v,mark) of
      (_, 0) -> Nothing
      (_, n) -> Just n


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
    stdout (return (fname <> "\n"))
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


cleanupDnsDnat :: HexCaptCfg -> IO ()
cleanupDnsDnat cfg = do
  e <- checkDnsDnat cfg
  when e $ dropDnsDnat cfg

setupDnsDnat :: HexCaptCfg -> IO ()
setupDnsDnat cfg = do
  exists <- checkDnsDnat cfg
  when (not exists) $ do
    createDnsDnat cfg

cleanupFilter :: HexCaptCfg -> IO ()
cleanupFilter cfg = do
  e <- checkFilter cfg
  when e $ do
    dropFilter cfg

setupFilter :: HexCaptCfg -> IO ()
setupFilter cfg = do
    exists <- checkFilter cfg
    when (not exists) $ do
      createFilter cfg

cleanupMangle :: HexCaptCfg -> IO ()
cleanupMangle cfg = do
  checkMangle cfg >>= mapM_ (dropMangle cfg)

setupMangle :: HexCaptCfg -> TVar Int -> TVar MacDB -> IO ()
setupMangle cfg tv db = do
    let eth = hIputIface cfg

    vPrev <- readTVarIO tv
    mdb <- readTVarIO db
    let vCurr = mdb ^. mdbVersion

    when (vPrev /= vCurr) $ do
      nums <- checkMangle cfg

      putStrLn [qq|update MANGLE $vPrev $vCurr ...|]

      let newChain = [qq|$mangle_CHAIN$vCurr|]

      shell [qq|iptables -w -t mangle -N $newChain|] mzero
      shell [qq|iptables -w -t mangle -F $newChain|] mzero

      forM_ (M.toList (mdb ^. mdbMarks)) $ \(mac,mark) -> do
        putStrLn [qq|add mark rule $eth $mac $mark|]
        shell [qq|iptables -w -t mangle -A $newChain -i $eth -m mac --mac-source $mac -j MARK --set-mark $mark|] mzero

      atomically $ writeTVar tv vCurr

      shell [qq|iptables -w -t mangle -A $newChain -i $eth -j CONNMARK --save-mark|] mzero
      shell [qq|iptables -w -t mangle -A $newChain -j RETURN|] mzero
      shell [qq|iptables -w -t mangle -A PREROUTING -j $newChain|] mzero

      mapM_ (dropMangle cfg) nums

      return ()

dnsDNAT_CHAIN :: String
dnsDNAT_CHAIN = "HEXCAPTDNSDNAT"

mangle_CHAIN :: String
mangle_CHAIN = "HEXCAPTMANGLE"

filter_CHAIN :: String
filter_CHAIN = "HEXCAPTFILTER"

checkFilter :: HexCaptCfg -> IO Bool
checkFilter cfg = do
  let checkCmd = grep (prefix (fromString filter_CHAIN)) $ inshell "iptables -t filter -L INPUT 2>/dev/null" ""
  s <- fold checkCmd Fold.head
  return (isJust s)

dropFilter :: HexCaptCfg -> IO ()
dropFilter cfg = do
  shell [qq|iptables -w -t filter -D INPUT -j $filter_CHAIN|] mzero
  shell [qq|iptables -w -t filter -F $filter_CHAIN|] mzero
  shell [qq|iptables -w -t filter -X $filter_CHAIN|] mzero
  return ()

createFilter :: HexCaptCfg -> IO ()
createFilter cfg = do
  putStrLn "createFilter"

  let ncfgs = [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]

--   let eth = hIputIface cfg

  shell [qq|iptables -w -t filter -N $filter_CHAIN |] mzero
  shell [qq|iptables -w -t filter -F $filter_CHAIN |] mzero

  forM_ ncfgs $ \ncfg -> do
    let tcp = nListenTCP ncfg
    let udp = nListenUDP ncfg

    shell [qq|iptables -w -t filter -A $filter_CHAIN -p udp --dport $udp -j ACCEPT|] mzero
    shell [qq|iptables -w -t filter -A $filter_CHAIN -p tcp --dport $tcp -j ACCEPT|] mzero

  shell [qq|iptables -w -t filter -A $filter_CHAIN -j RETURN|] mzero
  shell [qq|iptables -w -t filter -A INPUT -j $filter_CHAIN|] mzero

  return ()

dropMangle :: HexCaptCfg -> (Text,Text) -> IO ()
dropMangle cfg (n,nm) = do
  putStrLn [qq|delete chain {(n,nm)}|]
  shell [qq|iptables -w -t mangle -D PREROUTING -j $nm|] mzero
  shell [qq|iptables -w -t mangle -F $nm|] mzero
  shell [qq|iptables -w -t mangle -X $nm|] mzero
  return ()

checkMangle :: HexCaptCfg -> IO [(Text,Text)]
checkMangle cfg = do
  let checkCmd = grep (has $ fromString mangle_CHAIN) $ inshell "iptables -w -t mangle -L PREROUTING --line-numbers" ""
  foldMap (mk . filter (not.T.null) . T.split isSpace) <$> fold checkCmd Fold.list

  where
    mk :: [Text] -> [(Text,Text)]
    mk (num:name:_) = [(num,name)]
    mk _            = []

checkDnsDnat :: HexCaptCfg -> IO Bool
checkDnsDnat cfg = do
  let checkCmd = grep (prefix (fromString dnsDNAT_CHAIN)) $ inshell "iptables -w -t nat -L PREROUTING" ""
  s <- fold checkCmd Fold.head
  return (isJust s)

createDnsDnat :: HexCaptCfg -> IO ()
createDnsDnat cfg = do
  putStrLn "createDnsDnat"

  let ncfgs = filter (not.null.nMarks) [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]

  let eth = hIputIface cfg

  shell [qq|iptables -w -t nat -N $dnsDNAT_CHAIN |] mzero
  shell [qq|iptables -w -t nat -F $dnsDNAT_CHAIN |] mzero
  shell [qq|iptables -w -t nat -A $dnsDNAT_CHAIN -i $eth -j CONNMARK --restore-mark|] mzero

  forM_ ncfgs $ \ncfg -> do
    let tcp = nListenTCP ncfg
    let udp = nListenUDP ncfg

    forM_ (nMarks ncfg) $ \nmark -> do
      shell [qq|iptables -w -t nat -A $dnsDNAT_CHAIN -i $eth -p udp --dport 53 -m mark --mark $nmark -j REDIRECT --to-port $udp|] mzero

    forM_ (nMarks ncfg) $ \nmark -> do
      shell [qq|iptables -w -t nat -A $dnsDNAT_CHAIN -i $eth -p tcp --dport 53 -m mark --mark $nmark -j REDIRECT --to-port $tcp|] mzero

  shell [qq|iptables -w -t nat -A $dnsDNAT_CHAIN -j RETURN|] mzero
  shell [qq|iptables -w -t nat -A PREROUTING -j $dnsDNAT_CHAIN|] mzero

  return ()

dropDnsDnat :: HexCaptCfg -> IO ()
dropDnsDnat cfg = do
  shell [qq|iptables -w -t nat -D PREROUTING -j $dnsDNAT_CHAIN|] mzero
  shell [qq|iptables -w -t nat -F $dnsDNAT_CHAIN|] mzero
  shell [qq|iptables -w -t nat -X $dnsDNAT_CHAIN|] mzero
  return ()

macParser :: A.Parser String
macParser = do
  ms <- A.count 5 macSep
  ml <- macPart
  return $ intercalate ":" (ms <> [ml])

  where macPart = A.count 2 (A.satisfy (isHexDigit))
        macSep  = do
          m <- macPart
          A.char ':'
          return m

serveSetAccess :: HexCaptCfg -> TVar MacDB -> Event -> Server HEXCaptAPI
serveSetAccess cfg db ev (Just mac') (Just mark) = do
  case (A.parseOnly macParser mac')  of
    Left _  -> throwError err404
    Right m -> do
      liftIO $ atomically $ modifyTVar' db (macDbUpdate m mark)
      liftIO $ Event.signal ev
      return [qq|set access $m $mark ok|]

serveSetAccess _ _ _ _ _ = throwError err404

webapp :: HexCaptCfg -> TVar MacDB -> Event -> Application
webapp cfg db ev = do
  serve (Proxy :: Proxy HEXCaptAPI) (serveSetAccess cfg db ev)

main :: IO ()
main = do

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


  db <- newTVarIO def :: IO (TVar MacDB)

  let port = hListen cfg
  let bind = hBind cfg

  let settings = (setPort port  . setHost (fromString bind)) defaultSettings
  let ncfgs = [ e | e@(NDNProxyCfg{nUpstreamDNS = (_:_)}) <- universeBi cfg]

  ev <- Event.new :: IO Event

  aw <- async (runSettings settings (webapp cfg db ev))
  ans <- mapM (async . procNDNProxy) ncfgs
  let ass =  aw : ans

  finally (mainLoop cfg db ev) (cleanup cfg ass)

  return ()

  where

    mainLoop cfg db ev = do

      let zzz = hSleep cfg

      cleanupDnsDnat cfg
      cleanupFilter cfg
      cleanupMangle cfg

      tv <- readTVarIO db >>= newTVarIO . view mdbVersion

      forever $ do
        setupMangle cfg tv db
        setupDnsDnat cfg
        setupFilter cfg

        _ <- Event.waitTimeout ev (floor (zzz*1000000))
        return ()

      return ()

    cleanup cfg as = do
      putStrLn "DO CLEANUP ON EXIT"
      mapM_ cancel as
      cleanupDnsDnat cfg
      cleanupFilter cfg
      cleanupMangle cfg

