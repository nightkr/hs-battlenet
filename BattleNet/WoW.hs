module BattleNet.WoW(WoWCharacterInfo(..), character) where

import BattleNet.ApiKey
import BattleNet.Plumbing
import BattleNet.WoWStatic
import Data.Aeson
import Data.Map
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit

data WoWCharacterInfo = WoWCharacterInfo
    { characterName :: Text
    , characterRealm :: Text
    , characterClass :: WoWClassInfoId
    , characterThumbnail :: Text
    } deriving Show

instance FromJSON WoWCharacterInfo where
    parseJSON (Object v) = do
        characterName <- v .: "name"
        characterRealm <- v .: "realm"
        characterClass <- WoWClassInfoId <$> v .: "class"
        characterThumbnail <- v .: "thumbnail"
        return WoWCharacterInfo
            { characterName = characterName
            , characterRealm = characterRealm
            , characterClass = characterClass
            , characterThumbnail = characterThumbnail
            }
    parseJSON _ = mzero

character :: Text -> Text -> Manager -> BattleNetApiKey -> IO WoWCharacterInfo
character name realm = apiEndpoint ["wow", "character", name, realm] []