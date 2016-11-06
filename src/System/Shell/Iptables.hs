{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
module System.Shell.Iptables where

import Control.Monad
import Data.Char (isSpace)
import Data.Maybe
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq,qc,ShowQ(..))
import Turtle

type TableName = String
type ChainName = String

createChain :: TableName -> ChainName -> IO ()
createChain t c = sh $ do
  shell [qq|iptables -t $t -N $c|] empty
  return ()

deleteChain :: TableName -> ChainName -> IO ()
deleteChain t c = sh $ do
  liftIO $ flushChain t c
  shell [qq|iptables -t $t -X $c|] empty
  return ()

flushChain :: TableName -> ChainName -> IO ()
flushChain t c = sh $ do
  shell [qq|iptables -t $t -F $c|] empty

insertRule :: ShowQ a
           => TableName
           -> ChainName
           -> Maybe Int
           -> a
           -> IO ()
insertRule t c mp x = sh $ do
  let app = maybe ([qq|-A $c|]) (\p -> [qq|-I $c $p|]) mp :: String
  shell [qc|iptables -t {t} {app} {x} |] empty


unlinkChain :: TableName
            -> ChainName
            -> ChainName
            -> IO ()
unlinkChain t c n = sh $ go

  where
    cmd = (grep (has (fromString n)) (inshell [qq|iptables -t $t -L $c --line-numbers|] empty))

    go = do
      rn <- fold cmd Fold.head
      case rn of
        Nothing -> return ()
        Just s  -> do
          case T.split isSpace s of
            (i:_) -> do
              shell [qq|iptables -t $t -D $c $i|] empty
              go
            _ -> return ()
