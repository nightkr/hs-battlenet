module BattleNet.Yesod(YesodBattleNet(..), parseBattleNetConf, yesodBnet) where

import BattleNet
import Yesod.Core
import Control.Monad
import Data.Yaml
import Network.HTTP.Conduit

class YesodBattleNet app where
    battleNetKey :: app -> BattleNetApiKey
    battleNetHttp :: app -> Manager

parseBattleNetConf :: Value -> Parser BattleNetApiKey
parseBattleNetConf (Object v) = do
     key <- v .: "key"
     secret <- v .: "secret"
     region <- v .: "region"
     return BattleNetApiKey
         { bnetApiKey = key
         , bnetApiSecret = secret
         , bnetRegion = region
         }
parseBattleNetConf _ = mzero

yesodBnet :: (MonadHandler m, YesodBattleNet (HandlerSite m)) => (Manager -> BattleNetApiKey -> a) -> m a
yesodBnet endpoint = do
    yesod <- getYesod
    let key = battleNetKey yesod
        manager = battleNetHttp yesod
    return $ endpoint manager key
