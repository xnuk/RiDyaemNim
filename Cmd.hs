{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Cmd (parse) where

import qualified PluginDaumDic as DaumDic

import qualified Data.ByteString as BS hiding (drop)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as T
import Data.Text.Encoding

import "interpolatedstring-perl6" Text.InterpolatedString.Perl6

import Network.HTTP.Types.URI (renderQuery)

strip :: BS.ByteString -> BS.ByteString
strip = encodeUtf8 . T.strip . decodeUtf8

stio :: [BS.ByteString] -> IO [BSLazy.ByteString]
stio = return . map BSLazy.fromStrict

st :: [BS.ByteString] -> [BSLazy.ByteString]
st = map BSLazy.fromStrict

parse :: BS.ByteString -> BS.ByteString -> IO [BSLazy.ByteString]
parse str nick
    | cmd [q|:hello|] || cmd [q|!hello|] = stio [[qq|Hello, $nick!|]]
    | cmd [q|:dic |] || cmd [q|!dic |] = DaumDic.parse (strip $ BS.drop 5 str)
    | cmd [q|:latex |] || cmd [q|!latex |] = stio [[q|http://latex.codecogs.com/png.latex|] `BS.append` renderQuery True [((strip $ BS.drop 7 str), Nothing)]]
    | otherwise = return []
    where cmd = (`BS.isPrefixOf` str)

