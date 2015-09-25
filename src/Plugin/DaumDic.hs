{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Plugin.DaumDic (plugin) where

import CmdUtil (Util(Util, rawBody), Plugin(Plugin))

import Control.Monad
import Data.Maybe (isJust)

import "wreq" Network.Wreq (getWith, responseBody, defaults, param)
import "aeson" Data.Aeson (decode, FromJSON(..), Value(Object), (.:))
import "lens" Control.Lens ((^.), (.~), (&))

import Text.Regex.PCRE.ByteString.Lazy (compile, compUTF8, execBlank, regexec)

import "interpolatedstring-perl6" Text.InterpolatedString.Perl6

import qualified Data.ByteString.UTF8 as BSU (fromString)
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text)

data DaumDic = DaumDic { definitions :: [Text] }

instance FromJSON DaumDic where
    parseJSON (Object v) = DaumDic <$> v .: [q|items|]
    parseJSON _ = mzero

bold :: BSLazy.ByteString
bold = BSLazy.fromString "\STX"

mth :: BSLazy.ByteString -> IO (Maybe (BSLazy.ByteString, BSLazy.ByteString))
mth str = compile compUTF8 execBlank (BSLazy.fromString "^[a-z]+\\|([^\\|]+)\\|(.+)$")
    >>= (let f (Left (ix, msg)) = error [qq|$msg at $ix|]
             f (Right regex) = return regex
         in f)
    >>= liftM (\m -> case m of Right (Just (_, _, _, [w, ex])) -> Just (w, ex)
                               Right _ -> Nothing
                               Left (_, s) -> error s) . flip regexec str

parse :: Util -> IO [BSLazy.ByteString]
parse (Util { rawBody = str }) =
    getWith (defaults
    & param [q|mod|]  .~ [[q|json|]]
    & param [q|code|] .~ [[q|utf_in_out|]]
    & param [q|enc|]  .~ [[q|utf|]]
    & param [q|cate|] .~ [[q|eng|]]
    & param [q|q|]    .~ [decodeUtf8 str]) [q|http://suggest.dic.daum.net/dic_all_ctsuggest|]
    >>= (\response -> case (decode (response ^. responseBody) :: Maybe DaumDic) of
                      Nothing -> return []
                      Just (DaumDic { definitions = xs }) -> do
                          ys <- mapM (mth . BSLazy.fromStrict . encodeUtf8) xs
                          let Just ps = sequence . filter isJust $ ys
                          return . map (BSLazy.append [qq|$bold$str$bold  |] . snd) . filter ((== BSLazy.fromStrict str) . fst) $ ps
        )

plugin :: Plugin
plugin = Plugin (map (BSU.fromString . (:"dic ")) ":!") parse
