module BattleNet.Yesod(YesodBattleNet(..), yesodBnetWithApp, yesodBnet) where

import BattleNet
import Yesod.Core
import Control.Monad
import Data.Text
import Network.HTTP.Conduit

class YesodBattleNet app where
    battleNetKey :: app -> BattleNetApiKey
    battleNetHttp :: app -> Manager

yesodBnetWithApp :: (YesodBattleNet app) => Text -> app -> (Manager -> BattleNetConnectionInfo -> IO a) -> IO a
yesodBnetWithApp region yesod endpoint =
        endpoint manager settings
    where settings = BattleNetConnectionInfo (battleNetKey yesod) region
          manager = battleNetHttp yesod

yesodBnet :: (MonadHandler m, YesodBattleNet (HandlerSite m)) => Text -> (Manager -> BattleNetConnectionInfo -> IO a) -> m a
yesodBnet region endpoint = getYesod >>= liftIO . flip (yesodBnetWithApp region) endpoint
