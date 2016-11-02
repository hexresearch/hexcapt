{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Network.HEXCapt.API where

import Data.Text (Text)
import Servant.API

type HEXCaptAPI = "hexcapt" :> "set-access"
                            :> QueryParam "mac-addr" Text
                            :> QueryParam "mark" Int
                            :> Get '[JSON] String

