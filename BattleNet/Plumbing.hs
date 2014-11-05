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

apiEndpointUrl' :: Text -> BattleNetApiKey -> [Text] -> [(Text, Text)] -> Text
apiEndpointUrl' baseDomain key parts queryString = Data.Text.concat $ mconcat
        [ ["https://"
          , bnetRegion key
          , "."
          , baseDomain
          , "/"]
        , renderedParts parts
        , "?" : renderedQueryStringParams queryString
        ]
    where renderedParts = Data.List.intersperse "/"
          renderedQueryStringParam (k, v) = [k, "=", v]
          renderedQueryStringParams = mconcat . intersperse ["&"] . fmap renderedQueryStringParam

apiEndpointUrl :: [Text] -> [(Text, Text)] -> BattleNetApiKey -> Text
apiEndpointUrl parts queryString key = apiEndpointUrl' "api.battle.net" key parts (("apikey", bnetApiKey key) : queryString)

apiEndpoint :: FromJSON a => [Text] -> [(Text, Text)] -> Manager -> BattleNetApiKey -> IO a
apiEndpoint parts queryString manager key = do
    path <- parseUrl $ unpack $ apiEndpointUrl parts queryString key
    response <- responseBody <$> httpLbs path manager
    let decoded = Aeson.decode response
    return $ fromMaybe (error $ mconcat ["Invalid response from ", show path, ": ", show response]) decoded
