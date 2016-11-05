{-# Language DeriveGeneric, DeriveDataTypeable #-}
module HEXCapt.Types where

import Data.Generics.Uniplate.Data
import Data.Data
import GHC.Generics

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



