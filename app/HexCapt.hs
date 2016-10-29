{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Main where

import Turtle
import Control.Concurrent.Async
import Text.InterpolatedString.Perl6 (qc,qq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO


class NDNProxyConfig a where
  ndnpBanTreshhold :: a -> Int
  ndnpBanTreshhold _ = 10
  ndnpListenUDP :: a -> Int
  ndnpListenTCP :: a -> Int
  ndnpUpstreamDNS :: a -> [(String,String)]


ndnproxyConfigAsText :: NDNProxyConfig cfg => cfg -> Text
ndnproxyConfigAsText c = str
  where
    dnsServers = T.intercalate "\n" $ map dnss (ndnpUpstreamDNS c)
    dnss (s,d) = T.concat ["dns_server = ", (T.pack s), " ", (T.pack d)]

    str = [qc|
rpc_port = 0
timeout = 7000
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

 -- sh $ do
 --   as1 <- fork $ proc "/home/dmz/ndm/ndnproxy/src/build_bin/ndnproxy" ["-c","example.conf"] ""
 --   as2 <- fork $ proc "/home/dmz/ndm/ndnproxy/src/build_bin/ndnproxy" ["-c","example.conf"] ""
 --   (_, r) <- liftIO $ waitAny [as1, as2]
 --   case r of
 --     ExitSuccess  -> return ()
 --     _            -> die "can't run ndnproxy"

  IO.putStrLn $ ndnproxyConfigAsText ()
