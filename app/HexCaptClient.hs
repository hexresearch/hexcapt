{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes, ExtendedDefaultRules #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Options.Applicative
import Prelude hiding (FilePath)
import Servant.Client
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Turtle hiding (Parser)

import Network.HEXCapt.API

main :: IO ()
main = do
  putStrLn "I'm hexcapt client"

  homePath <- home
  let hexCaptYaml = "hexcapt.yaml"
  let etcCfg = "/etc" </> hexCaptYaml
  let homeCfg = homePath </> hexCaptYaml
  let localCfg = hexCaptYaml

  let locations = [localCfg, homeCfg, etcCfg]

  cfgFname <- listToMaybe <$> filterM testfile locations

  print cfgFname

--   mcfg <- decodeFile (T.unpack $ format fp cfgFname) :: IO (Maybe HexCaptCfg)

  return ()

