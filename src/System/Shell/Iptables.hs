{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
module System.Shell.Iptables where

import Control.Monad
import Data.Char (isSpace)
import Data.Maybe
import Data.List
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import Text.InterpolatedString.Perl6 (qq,qc,ShowQ(..))
import Turtle

type TableName = String
type ChainName = String

data ConnmarkOpt = SAVE | RESTORE

data Proto = UDP (Maybe Int)
           | TCP (Maybe Int)

data MarkOpt = MarkEQ Int

data MacPart = MacSrc String

data MacOpt = MacEQ MacPart

data IPTablesCmd = RETURN
                 | ACCEPT
                 | REDIRECT Int
                 | SETMARK Int
                 | CONNMARK ConnmarkOpt

data IPTablesOpt =   J IPTablesCmd
                   | I String
                   | P Proto
                   | MARK MarkOpt
                   | MAC MacOpt

instance ShowQ MacOpt where
  showQ (MacEQ (MacSrc x)) = [qq|--mac-source $x|]

instance ShowQ MarkOpt where
  showQ (MarkEQ x) = [qq|--mark $x|]

instance ShowQ Proto where
  showQ (UDP dst) = [qq|udp --dport $d|]
    where d :: String
          d = maybe "" show dst

  showQ (TCP dst) = [qq|tcp --dport $d|]
    where d :: String
          d = maybe "" show dst

instance ShowQ [IPTablesOpt] where
  showQ ops = intercalate [qq| |] (fmap showQ ops)

instance ShowQ ConnmarkOpt where
  showQ SAVE = [qq|--save-mark|]
  showQ RESTORE = [qq|--restore-mark|]

instance ShowQ IPTablesCmd where
  showQ RETURN = [qq|RETURN|]
  showQ ACCEPT = [qq|ACCEPT|]
  showQ (REDIRECT n) = [qq|REDIRECT --to-port $n|]
  showQ (CONNMARK x) = [qq|CONNMARK $x|]
  showQ (SETMARK x) = [qq|MARK --set-mark $x |]

instance ShowQ IPTablesOpt where
  showQ (I x) = [qq|-i $x|]
  showQ (J x) = [qq|-j $x|]
  showQ (P x) = [qq|-p $x|]
  showQ (MARK x) = [qq|-m mark $x |]
  showQ (MAC x) = [qq|-m mac $x |]

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
