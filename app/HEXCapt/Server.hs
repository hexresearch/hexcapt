{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
module HEXCapt.Server where

import Data.Monoid
import Data.Attoparsec.Text
import Data.Char (isHexDigit)
import Data.List (nub,sort,intercalate)
import Servant.Server
import Text.InterpolatedString.Perl6 (qq)

import Network.HEXCapt.API

macParser :: Parser String
macParser = do
  ms <- count 5 macSep
  ml <- macPart
  return $ intercalate ":" (ms <> [ml])

  where macPart = count 2 (satisfy (isHexDigit))
        macSep  = do
          m <- macPart
          char ':'
          return m

-- serveSetAccess :: HexCaptCfg -> TVar MacDB -> Event -> Server HEXCaptAPI
-- serveSetAccess cfg db ev (Just mac') (Just mark) = do
--   case (parseOnly macParser mac')  of
--     Left _  -> throwError err404
--     Right m -> do
--       liftIO $ atomically $ modifyTVar' db (macDbUpdate m mark)
--       liftIO $ Event.signal ev
--       return [qq|set access $m $mark ok|]

-- serveSetAccess _ _ _ _ _ = throwError err404

-- webapp :: HexCaptCfg -> TVar MacDB -> Event -> Application
-- webapp cfg db ev = do
--   serve (Proxy :: Proxy HEXCaptAPI) (serveSetAccess cfg db ev)

