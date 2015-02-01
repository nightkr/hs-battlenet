module BattleNet.ApiKey(BattleNetApiKey(..), BattleNetConnectionInfo(..)) where

import Data.Text

data BattleNetApiKey = BattleNetApiKey
    { bnetApiKeyText :: Text
    , bnetApiSecret :: Text
    }


data BattleNetConnectionInfo = BattleNetConnectionInfo
    { bnetApiKey :: BattleNetApiKey
    , bnetRegion :: Text
    }