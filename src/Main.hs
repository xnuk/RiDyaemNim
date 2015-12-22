{-# LANGUAGE PackageImports, QuasiQuotes, TupleSections #-}
module Main where

import qualified Network.Socket as Socket
import Network.Socket (getAddrInfo, AddrInfo(..), SocketType(Stream))

import "tls" Network.TLS (defaultParamsClient, ClientParams(..), handshake, Supported(..), contextNew, sendData, recvData, sharedValidationCache, ValidationCache(..), ValidationCacheResult(ValidationCachePass))
import "tls" Network.TLS.Extra.Cipher

import Data.Default (def)
import Control.Monad
import Control.Exception
import Data.Maybe (isJust)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import qualified Data.Text as T
import Data.Text.Encoding

import "interpolatedstring-perl6" Text.InterpolatedString.Perl6 (qq, q)

import "regex-pcre" Text.Regex.PCRE.ByteString (Regex, regexec)
import qualified "regex-pcre" Text.Regex.PCRE.ByteString as PCRE

import qualified Config
import Config (conf)

import Cmd (parse)

data Prefix = Servername BS.ByteString | Nickname BS.ByteString | NoPrefix
data Param = Param [BS.ByteString] BS.ByteString
data Msg = Msg Prefix BS.ByteString Param

instance Show Prefix where
    show (Servername a) = BS.toString a ++ ">"
    show (Nickname a) = '<' : BS.toString a ++ ">"
    show NoPrefix = ""

instance Show Param where
    show (Param arr text) = BS.toString (encodeUtf8 . T.unwords . map decodeUtf8Chan $ arr) ++ " :" ++ BS.toString text

instance Show Msg where
    show (Msg prefix cmd param) = show prefix ++ ' ' : BS.toString cmd ++ ' ' : show param

decodeUtf8Chan :: BS.ByteString -> T.Text
decodeUtf8Chan = decodeUtf8With $ \_ a -> case a of Just _  -> Just '♥'
                                                    Nothing -> Nothing

getCmd :: Msg -> BS.ByteString
getCmd (Msg _ cmd _) = cmd

getNickname :: Msg -> Maybe BS.ByteString
getNickname (Msg (Nickname nick) _ _) = Just nick
getNickname _ = Nothing

getText :: Msg -> BS.ByteString
getText (Msg _ _ (Param _ text)) = text

getMiddle :: Msg -> [BS.ByteString]
getMiddle (Msg _ _ (Param arr _)) = arr

if' :: Bool -> a -> a-> a
if' True a _ = a
if' False _ a = a

infixl 6 =~~, =~

(=~~) :: BS.ByteString -> BS.ByteString -> IO (Maybe (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString]))
a =~~ b = liftM (\(Right x) -> x)
    (PCRE.compile PCRE.compUTF8 PCRE.execBlank b
    >>= (let f (Left (ix, msg)) = error [qq|$msg at $ix|]
             f (Right regex) = regexec regex a
         in f))

(=~) :: BS.ByteString -> IO Regex -> IO (Maybe (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString]))
a =~ regex = do
    reg <- regex
    liftM (\m -> case m of Right x -> x
                           Left (_, s) -> error s) $ regexec reg a

toRegex :: BS.ByteString -> IO Regex
toRegex x = PCRE.compile PCRE.compUTF8 PCRE.execBlank x
    >>= (let f (Left (ix, msg)) = error [qq|$msg at $ix|]
             f (Right regex) = return regex
         in f)

regexMsg :: IO Regex
regexMsg = toRegex [q|^(?::([^\s]+)[ \t]+)?([a-zA-Z]+|\d\d\d)((?:[ \t][^:\s][^\s]*)*)(?:[ \t]+:([^\r\n]*))?\r\n|]

findAllMsg :: BS.ByteString -> IO ([Msg], BS.ByteString)
findAllMsg str = do
    x <- (encodeUtf8 . decodeUtf8Chan) str =~ regexMsg
    case x of
      Nothing -> return ([], str)
      Just (_, _, next, [prefix, cmd, middle, text]) -> do
          pref <- prefix =~~ [q|^[^!@\s]+|]
          let prefix' = case pref of
                          Nothing -> if' (prefix == BS.empty) NoPrefix (Servername prefix)
                          Just (_, match, _, _) -> Nickname match
          let prm = map encodeUtf8 . T.words . decodeUtf8Chan $ middle
          (arr, chunk) <- findAllMsg next
          return (Msg prefix' cmd (Param prm text) : arr, chunk)
      _ -> undefined

main :: IO ()
main =
    liftM (filter ((== Stream) . addrSocketType))
        (getAddrInfo Nothing
            (Just $ Config.server conf)
            (Just . show $ Config.port conf))
    >>= connect start

connect :: ((BSLazy.ByteString -> IO ()) -> IO BS.ByteString -> IO ()) -> [AddrInfo] -> IO ()
connect _ [] = return ()
connect func ((AddrInfo { addrAddress = addr
                        , addrFamily = family
                        , addrSocketType = sockType
                        , addrProtocol = protocol }) : infos) = do
    socket <- Socket.socket family sockType protocol
    c <- try $ Socket.connect socket addr
    case c of
      Left (SomeException _) -> Socket.close socket >> connect func infos
      Right _ -> do
        ctx <- contextNew socket $
            (defaultParamsClient (Config.server conf) BS.empty)
            { clientSupported = def { supportedCiphers = ciphersuite_all
                                    , supportedSecureRenegotiation = False
                                    }
            , clientShared = def { sharedValidationCache = ValidationCache
                                            (\_ _ _ -> return ValidationCachePass)
                                            (\_ _ _ -> return ()) }
            }
        handshake ctx
        func (sendData ctx) (recvData ctx)

start :: (BSLazy.ByteString -> IO ()) -> IO BS.ByteString -> IO ()
start send' recv' = do
    let (Config.Config { Config.nick = nick
                       , Config.channels = channels
                       }) = conf
    (msgs, chunk) <- recv' >>= findAllMsg
    mapM_ print msgs
    send [q|NICK unlambdachan|]
    send [q|USER unlambdachan Xnuktest XnukChan :a s|]
    (msgs', chunk') <- recv' >>= findAllMsg . BS.append chunk
    mapM_ print msgs'
    send [qq|NICK $nick|]
    send [qq|JOIN {BSLazy.intercalate (BSLazy.fromString ",") channels}|]
    loop chunk'
    where
        send = send' . (`BSLazy.append` BSLazy.fromString "\r\n")
        loop :: BS.ByteString -> IO ()
        loop ch = do
            (msgs, chunk) <- recv' >>= findAllMsg . BS.append ch
            let messages = filter (\x -> getCmd x == [q|PRIVMSG|] && isJust (getNickname x)) msgs
            let ping = ((> 0) . length) $ filter (([q|PING|] ==) . getCmd) msgs
            if' ping (send [q|PONG :|]) (return ())
            mapM_ print . filter (([q|PING|] /=) . getCmd) $ msgs

            let chats = mapM (\(Msg (Nickname nick) _ (Param [channel] text)) -> liftM (, channel) (parse text nick)) messages
            chats >>= mapM_ (\(xs, channel) -> mapM_ (\x -> send [qq|PRIVMSG $channel :$x|]) xs)
            loop chunk
