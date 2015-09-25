{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Cmd (parse) where

import qualified Plugin.DaumDic as DaumDic
import qualified Plugin.Emoji as Emoji
import qualified CmdUtil as U (Util(..))
import CmdUtil (Util(Util), isCmdOf, getParse)

import qualified Data.ByteString as BS hiding (drop, length)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import qualified Data.Text as T
import Data.Text.Encoding

import "interpolatedstring-perl6" Text.InterpolatedString.Perl6

import Network.HTTP.Types.URI (renderQuery)
import System.Random (newStdGen, randomR)

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
    "김젼봇_",
    "lambdaChan"]


parse :: BS.ByteString -> BS.ByteString -> IO [BSLazy.ByteString]
parse str nick
    | nick `elem` botlist = return []
    | cmd ":hello" || cmd "!hello" = stio [[qq|Hello, $nick!|]]
    | isCmd DaumDic.plugin = getParse DaumDic.plugin util
    | cmd ":latex " || cmd "!latex " = stio [[q|http://latex.codecogs.com/png.latex|] `BS.append` renderQuery True [((strip $ BS.drop 7 str), Nothing)]]
    | cmd ":랜덤 " || cmd "!랜덤 " || cmd ":random " || cmd "!random " =
        if null body
           then return []
           else newStdGen >>= return . randomR (0, length body - 1) >>= \(x, _) -> stio [body !! x]

    | isCmd Emoji.plugin = getParse Emoji.plugin util
    | otherwise = return []
    where cmd = (`BS.isPrefixOf` str) . BS.fromString
          exact = (str ==) . BS.fromString
          body = map encodeUtf8 . tail . T.words $ decodeUtf8 str
          rawBody = strip $ BS.drop (BS.length (head body) + 1) str
          isCmd = (`isCmdOf` str)
          util = Util { U.rawBody = rawBody
                      , U.exact = exact
                      , U.nick = nick
                      , U.str = str
                      , U.body = body }

