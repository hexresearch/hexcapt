module Data.Attoparsec.HEXCapt where

import Data.Attoparsec.Text
import Data.Char
import Data.List (intercalate)
import Data.Monoid

macParser :: Parser String
macParser = do
  ms <- count 5 macSep
  ml <- macPart
  return $ intercalate ":" (ms <> [ml])

  where macPart = count 2 (satisfy (isHexDigit))
        macSep  = do
          m <- macPart
          char ':'
          return (fmap toLower m)

