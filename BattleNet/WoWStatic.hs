module BattleNet.WoWStatic( WoWClassInfoId(..)
                          , WoWClassInfo(..), classes
                          , WoWTalentInfo(..), talents
                          ) where

import BattleNet.ApiKey
import BattleNet.Plumbing
import Data.Aeson
import Data.Map
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit

newtype WoWClassInfoId = WoWClassInfoId Int deriving (Show, Ord, Eq)

data WoWClassInfo = WoWClassInfo
    { classId :: WoWClassInfoId
    , className :: Text
    } deriving Show

newtype WoWClassesInfo = WoWClassesInfo [WoWClassInfo]
extractClassesInfo :: WoWClassesInfo -> [WoWClassInfo]
extractClassesInfo (WoWClassesInfo x) = x

data WoWTalentInfo = WoWTalentInfo
    { talentInfoClass :: Text
    } deriving Show

instance FromJSON WoWClassInfo where
    parseJSON (Object v) = do
        classId <- WoWClassInfoId <$> v .: "id"
        className <- v .: "name"
        return WoWClassInfo
            { classId = classId
            , className = className
            }
    parseJSON _ = mzero

instance FromJSON WoWClassesInfo where
    parseJSON (Object v) = WoWClassesInfo <$> (v .: "classes" >>= parseJSON)
    parseJSON _ = mzero

instance FromJSON WoWTalentInfo where
    parseJSON (Object v) = do
        talentInfoClass <- v .: "class"
        return WoWTalentInfo
            { talentInfoClass = talentInfoClass
            }
    parseJSON _ = mzero

classes :: Manager -> BattleNetApiKey -> IO [WoWClassInfo]
classes manager key = extractClassesInfo <$> apiEndpoint ["wow", "data", "character", "classes"] [] manager key

talents :: Manager -> BattleNetApiKey -> IO (Map WoWClassInfoId WoWTalentInfo)
talents manager key = mapKeys (WoWClassInfoId . read) <$> apiEndpoint ["wow", "data", "talents"] [] manager key
