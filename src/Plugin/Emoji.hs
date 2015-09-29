module Plugin.Emoji (plugin) where

import Prelude hiding (lookup)

import Control.Arrow ((***))

import CmdUtil (Util(Util, str), Plugin(..))
import Data.Map.Strict as Map (Map, fromList, lookup, keys)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (ByteString)

emojis :: Map ByteString ByteString
emojis = fromList . map (fromString *** (fromString . (++ " "))) $
    [ ("d(ㅇㅅㅇ",    "ㅇㅅㅇ)b")
    , ("ㅇㅅ<",       ">ㅅㅇ")
    , ("/ㅇㅅㅇ)/",   "\\(ㅇㅅㅇ\\")
    , ("/ㅇㅅㅇ/",    "\\ㅇㅅㅇ\\")
    , ("ㅇㅅㅇ)/",    "\\(ㅇㅅㅇ")
    , ("ㅇㅅㅇ/",     "\\ㅇㅅㅇ")
    , ("\\(ㅇㅅㅇ\\", "/ㅇㅅㅇ)/")
    , ("\\ㅇㅅㅇ\\",  "/ㅇㅅㅇ/")
    , ("\\(ㅇㅅㅇ",    "ㅇㅅㅇ)/")
    , ("\\ㅇㅅㅇ",    "ㅇㅅㅇ/")
    ]

plugin :: Plugin
plugin = Plugin (keys emojis) (\(Util { str = s }) ->
    return $ case lookup s emojis of
               Just a -> [fromStrict a]
               Nothing -> []
    )
