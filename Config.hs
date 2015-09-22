module Config (Config(..), conf) where

import Network.Socket (HostName, PortNumber)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)

data Config = Config { server :: HostName
                     , port :: PortNumber
                     , nick :: ByteString
                     , channels :: [ByteString]
                     }

conf :: Config
conf = Config { server = "irc.uriirc.org"
              , port = 16667
              , nick = fromString "리덈늼"
              , channels = map fromString ["#hyeon", "#foobar"]
              }
