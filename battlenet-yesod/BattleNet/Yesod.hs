module BattleNet.Yesod(YesodBattleNet(..), yesodBnetWithApp, yesodBnet) where

import BattleNet
import Yesod.Core
import Control.Monad
import Network.HTTP.Conduit

class YesodBattleNet app where
    battleNetKey :: app -> BattleNetApiKey
    battleNetHttp :: app -> Manager

yesodBnetWithApp :: (YesodBattleNet app) => app -> (Manager -> BattleNetApiKey -> IO a) -> IO a
yesodBnetWithApp yesod endpoint =
        endpoint manager key
    where key = battleNetKey yesod
          manager = battleNetHttp yesod

yesodBnet :: (MonadHandler m, YesodBattleNet (HandlerSite m)) => (Manager -> BattleNetApiKey -> IO a) -> m a
yesodBnet endpoint = getYesod >>= liftIO . (flip yesodBnetWithApp endpoint)
