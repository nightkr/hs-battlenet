module BattleNet.Yesod(YesodBattleNet(..), yesodBnet) where

import BattleNet
import Yesod.Core
import Control.Monad
import Network.HTTP.Conduit

class YesodBattleNet app where
    battleNetKey :: app -> BattleNetApiKey
    battleNetHttp :: app -> Manager

yesodBnet :: (MonadHandler m, YesodBattleNet (HandlerSite m)) => (Manager -> BattleNetApiKey -> a) -> m a
yesodBnet endpoint = do
    yesod <- getYesod
    let key = battleNetKey yesod
        manager = battleNetHttp yesod
    return $ endpoint manager key
