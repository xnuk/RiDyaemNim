{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Cmd (parse) where

import Control.Monad (liftM)

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

soragodung :: [BS.ByteString]
soragodung = map BS.fromString [
    "하지 마.",
    "돼.",
    "그래.",
    "허락할게.",
    "나중에 해.",
    "다시 한번 물어봐.",
    "당장 시작해.",
    "하고 싶은 대로 해.",
    "맘대로 해.",
    "안 돼.",
    "포기해.",
    "가만히 있어.",
    "둘 다 하지마.",
    "아아안 돼애애.",
    "굶어.",
    "아니.",
    "응.",
    "다시 한번 물어봐."]

parse :: BS.ByteString -> BS.ByteString -> IO [BSLazy.ByteString]
parse str nick
    | nick `elem` botlist = return []
    | cmd ":hello" || cmd "!hello" = stio [[qq|Hello, $nick!|]]
    -- | cmd ";" = getParse DaumDic.plugin (util { U.rawBody = encodeUtf8 . (\x -> if null x then T.empty else head x) . T.words . T.tail $ decodeUtf8 str })
    | isCmd DaumDic.plugin = getParse DaumDic.plugin util
    | cmd ":latex " || cmd "!latex " = stio [[q|http://latex.codecogs.com/png.latex|] `BS.append` renderQuery True [(strip $ BS.drop 7 str, Nothing)]]
    | cmd ":랜덤 " || cmd "!랜덤 " || cmd ":random " || cmd "!random " =
        if null body
           then return []
           else liftM (randomR (0, length body - 1)) newStdGen >>= \(x, _) -> stio [body !! x]
    | cmd "소라고둥님" || cmd "소라고둥 님" ||
        cmd "마법의 소라고둥 님" || cmd "마법의소라고둥 님" ||
            cmd "마법의 소라고둥님" || cmd "마법의소라고둥님" =
                liftM (randomR (0, length soragodung)) newStdGen >>= \(x, _) -> stio [soragodung !! x]
    | isCmd Emoji.plugin = getParse Emoji.plugin util
    | otherwise = return []
    where cmd = (`BS.isPrefixOf` str) . BS.fromString
          exact = (str ==) . BS.fromString
          body = map encodeUtf8 . tail . T.words $ decodeUtf8 str
          rawBody = encodeUtf8 . T.unwords . map decodeUtf8 $ body -- should be fixed
          isCmd = (`isCmdOf` str)
          util = Util { U.rawBody = rawBody
                      , U.exact = exact
                      , U.nick = nick
                      , U.str = str
                      , U.body = body }

