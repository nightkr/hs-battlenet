module BattleNet.Yesod(YesodBattleNet(..), yesodBnet) where

import BattleNet
import Yesod.Core
import Control.Monad
import Network.HTTP.Conduit

class YesodBattleNet app where
    battleNetKey :: app -> BattleNetApiKey
    battleNetHttp :: app -> Manager

yesodBnet :: (MonadHandler m, YesodBattleNet (HandlerSite m)) => (Manager -> BattleNetApiKey -> IO a) -> m a
yesodBnet endpoint = do
    yesod <- getYesod
    let key = battleNetKey yesod
        manager = battleNetHttp yesod
    liftIO $ endpoint manager key
