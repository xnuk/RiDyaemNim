module CmdUtil (Util(..), Plugin(..), isCmdOf, getParse) where

import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString.Lazy as L (ByteString)

data Util = Util { rawBody :: ByteString
                 , exact :: String -> Bool
                 , nick :: ByteString
                 , str :: ByteString
                 , body :: [ByteString] }

data Plugin = Plugin [ByteString] (Util -> IO [L.ByteString])

isCmdOf :: Plugin -> ByteString -> Bool
isCmdOf (Plugin xs _) s = any (`isPrefixOf` s) xs

getParse :: Plugin -> Util -> IO [L.ByteString]
getParse (Plugin _ f) = f
