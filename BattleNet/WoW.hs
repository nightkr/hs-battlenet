module BattleNet.WoW(WoWCharacterInfo(..), character, userCharacters, guildMembers) where

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

newtype UserCharactersWrapper = UserCharactersWrapper [WoWCharacterInfo]

instance FromJSON UserCharactersWrapper where
    parseJSON (Object v) = UserCharactersWrapper <$> v .: "characters"
    parseJSON _ = mzero

data WoWGuildMemberInfo = WoWGuildMemberInfo
    { memberName :: Text
    , memberRealm :: Text
    , memberRank :: Int
    }

instance FromJSON WoWGuildMemberInfo where
    parseJSON (Object v) = do
        member <- v .: "character"
        memberName <- member .: "name"
        memberRealm <- member .: "realm"
        memberRank <- v .: "rank"
        return WoWGuildMemberInfo
            { memberName = memberName
            , memberRealm = memberRealm
            , memberRank = memberRank
            }

newtype GuildMemberWrapper = GuildMemberWrapper [WoWGuildMemberInfo]

instance FromJSON GuildMemberWrapper where
    parseJSON (Object v) = GuildMemberWrapper <$> v .: "members"
    parseJSON _ = mzero

character :: Text -> Text -> Manager -> BattleNetApiKey -> IO WoWCharacterInfo
character realm name = apiEndpoint ["wow", "character", realm, name] []

userCharacters :: Text -> Manager -> BattleNetApiKey -> IO [WoWCharacterInfo]
userCharacters accessToken manager key = unwrapChars <$> apiEndpoint ["wow", "user", "characters"] [("access_token", accessToken)] manager key
    where unwrapChars (UserCharactersWrapper chars) = chars

guildMembers :: Text -> Text -> Manager -> BattleNetApiKey -> IO [WoWGuildMemberInfo]
guildMembers realm name manager key = unwrapMembers <$> apiEndpoint ["wow", "guild", realm, name] [("fields", "members")] manager key
    where unwrapMembers (GuildMemberWrapper chars) = chars