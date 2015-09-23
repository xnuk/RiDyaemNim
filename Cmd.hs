{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Cmd (parse) where

import qualified PluginDaumDic as DaumDic

import qualified Data.ByteString as BS hiding (drop)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import qualified Data.Text as T
import Data.Text.Encoding

import "interpolatedstring-perl6" Text.InterpolatedString.Perl6

import Network.HTTP.Types.URI (renderQuery)

strip :: BS.ByteString -> BS.ByteString
strip = encodeUtf8 . T.strip . decodeUtf8

stio :: [BS.ByteString] -> IO [BSLazy.ByteString]
stio = return . map BSLazy.fromStrict

strio :: [String] -> IO [BSLazy.ByteString]
strio = return . map BSLazy.fromString

st :: [BS.ByteString] -> [BSLazy.ByteString]
st = map BSLazy.fromStrict

botlist :: [BS.ByteString]
botlist = map BS.fromString [
    "김젼봇",
    "lambdaChan"]


parse :: BS.ByteString -> BS.ByteString -> IO [BSLazy.ByteString]
parse str nick
    | cmd ":hello" || cmd "!hello" = stio [[qq|Hello, $nick!|]]
    | cmd ":dic " || cmd "!dic " = DaumDic.parse (strip $ BS.drop 5 str)
    | cmd ":latex " || cmd "!latex " = stio [[q|http://latex.codecogs.com/png.latex|] `BS.append` renderQuery True [((strip $ BS.drop 7 str), Nothing)]]
    | exactly "d(ㅇㅅㅇ" && isHuman = strio ["ㅇㅅㅇ)b "]
    | exactly "ㅇㅅ<" && isHuman = strio [">ㅅㅇ "]
    | exactly "/ㅇㅅㅇ)/" && isHuman = strio ["\\(ㅇㅅㅇ\\ "]
    | exactly "\\(ㅇㅅㅇ\\" && isHuman = strio ["/ㅇㅅㅇ)/ "]
    | otherwise = return []
    where cmd = (`BS.isPrefixOf` str) . BS.fromString
          exactly = (str ==) . BS.fromString
          isHuman = nick `notElem` botlist

