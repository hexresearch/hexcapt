{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
module System.Shell.Iptables where

import Text.InterpolatedString.Perl6 (qq)
import Turtle

type TableName = String
type ChainName = String

createChain :: TableName -> ChainName -> IO ()
createChain t c = sh $ do
  shell [qq|iptables -t $t -N $c|] empty
  return ()

deleteChain :: TableName -> ChainName -> IO ()
deleteChain t c = sh $ do
  shell [qq|iptables -t $t -F $c|] empty
  shell [qq|iptables -t $t -X $c|] empty
  return ()

