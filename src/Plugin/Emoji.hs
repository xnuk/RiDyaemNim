module Plugin.Emoji (plugin) where

import Prelude hiding (lookup)

import Control.Arrow ((***))

import CmdUtil (Util(Util, str), Plugin(..))
import Data.Map.Strict as Map (Map, fromList, lookup, keys)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (ByteString)
import System.Random (newStdGen, randomR)

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

tableFlipCmds :: [ByteString]
tableFlipCmds = map fromString
    [ "ㅇㅁㅇ)/ㅛ"
    , "/ㅇㅁㅇ)/ㅛ"
    , "(/ㅇㅁㅇ)/ㅛ"
    , ":table",     "!table"
    , ":flip",      "!flip"
    , ":tableflip", "!tableflip"
    ]

tableFlips :: [L.ByteString]
tableFlips = map (fromStrict . fromString)
    [ "\40\9583\176\9633\176\65289\9583\65077\32\9531\9473\9531" -- (╯°□°）╯︵ ┻━┻
    , "\9531\9473\9531\32\65077\12541\40\96\1044\180\41\65417\65077\65279\32\9531\9473\9531" -- ┻━┻ ︵ヽ(`Д´)ﾉ︵﻿ ┻━┻
    , "\32\9516\9472\9516\12494\40\32\186\32\95\32\186\12494\41" -- ┬─┬ノ( º _ ºノ)
    , "\40\9583\3232\95\3248\3267\41\9583\65077\32\9531\9473\9531" -- (╯ಠ_ರೃ)╯︵ ┻━┻
    , "\40\9583\176\9633\176\65289\9583\65077\32\47\40\46\9633\46\32\92\41" -- (╯°□°）╯︵ /(.□. \)
    , "\40\9499\10061\7461\10061\65279\41\9499\24417\9531\9473\9531" -- (┛❍ᴥ❍﻿)┛彡┻━┻
    , "\40\664\8711\664\41\12463\32\24417\32\9531\9473\9531" -- (ʘ∇ʘ)ク 彡 ┻━┻
    , "\47\40\242\46\243\41\9499\24417\9531\9473\9531" -- /(ò.ó)┛彡┻━┻
    , "\40\9499\9673\1044\9673\41\9499\24417\9531\9473\9531" -- (┛◉Д◉)┛彡┻━┻
    , "\40\12494\210\30410\211\41\12494\24417\9620\9620\9615" -- (ノÒ益Ó)ノ彡▔▔▏
    ]

plugin :: Plugin
plugin = Plugin (keys emojis ++ tableFlipCmds) (\(Util { str = s }) ->
    case lookup s emojis of
        Just a -> return [fromStrict a]
        Nothing -> if s `elem` tableFlipCmds
                       then newStdGen >>= return . randomR (0, length tableFlips - 1) >>= \(x, _) -> return [tableFlips !! x]
                       else return []
    )
