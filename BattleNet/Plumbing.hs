module BattleNet.Plumbing where

import BattleNet.ApiKey

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Text hiding (intersperse)
import Data.Aeson
import qualified  Data.Aeson as Aeson
import Network.HTTP.Conduit hiding (path, queryString)

apiEndpointUrl' :: Text -> BattleNetConnectionInfo -> [Text] -> [(Text, Text)] -> Text
apiEndpointUrl' baseDomain settings parts queryString = Data.Text.concat $ mconcat
        [ ["https://"
          , bnetRegion settings
          , "."
          , baseDomain
          , "/"]
        , renderedParts parts
        , "?" : renderedQueryStringParams queryString
        ]
    where renderedParts = Data.List.intersperse "/"
          renderedQueryStringParam (k, v) = [k, "=", v]
          renderedQueryStringParams = mconcat . intersperse ["&"] . fmap renderedQueryStringParam

apiEndpointUrl :: [Text] -> [(Text, Text)] -> BattleNetConnectionInfo -> Text
apiEndpointUrl parts queryString settings = apiEndpointUrl' "api.battle.net" settings parts (("apikey", bnetApiKeyText $ bnetApiKey settings) : queryString)

apiEndpoint :: FromJSON a => [Text] -> [(Text, Text)] -> Manager -> BattleNetConnectionInfo -> IO a
apiEndpoint parts queryString manager settings = do
    path <- parseUrl $ unpack $ apiEndpointUrl parts queryString settings
    response <- responseBody <$> httpLbs path manager
    let decoded = Aeson.decode response
    return $ fromMaybe (error $ mconcat ["Invalid response from ", show path, ": ", show response]) decoded
