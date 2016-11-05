{-# Language OverloadedStrings #-}
module Application.HEXCapt.NDNProxy where

import Turtle

import HEXCapt.Types
import HEXCapt.Config
import Application.HEXCapt.Types

procNDNProxy :: (MonadIO io, NDNProxyConfig cfg) => cfg -> io ()
procNDNProxy cfg = sh $ do
    fname' <- mktempfile "/tmp" "ndnproxy-conf"
    output fname' (return (ndnproxyConfigAsText cfg))
    let fname = format fp fname'
    stdout (return (fname <> "\n"))
    procs "ndnproxy" ["-c", fname] ""
