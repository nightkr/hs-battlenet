module BattleNet.ApiKey(BattleNetApiKey(..)) where

import Data.Text

data BattleNetApiKey = BattleNetApiKey
    { bnetApiKey :: Text
    , bnetApiSecret :: Text
    , bnetRegion :: Text
    }
