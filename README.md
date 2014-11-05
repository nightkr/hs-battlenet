hs-battlenet
============

Battle.Net API client for Haskell.

Usage
-----

Everything you need for basic usage should be exported through the `BattleNet` and `Network.HTTP.Conduit` (from `http-conduit`) modules, so first import them:

```haskell
import BattleNet
import Network.HTTP.Conduit
```

In order to do anything useful, you need to supply the library with a context, so let's set up that (you can get an API key from [dev.battle.net](https://dev.battle.net/apps/myapps)):

```haskell
key = BattleNetApiKey
    { bnetApiKey = "BLAH"
    , bnetApiSecret = "BLAH"
    , bnetRegion = "eu"
    }

main = do
    manager <- newManager defaultManagerSettings
```

Now we're ready to retrieve data from the API, both static data:

```haskell
    classes manager key >>= putStrLn . show
```

and data about a player:

```haskell
    character "Teozkr" "Draenor" manager key >>= putStrLn . show
```

It's not feature-complete yet, but feel free to send a pull request for anything you feel is missing.

Yesod
-----

The companion package `battlenet-yesod` is a helper for Yesod projects that automatically loads the manager and key from your App.

```haskell
instance YesodBattleNet App where
    battleNetKey = (IMPL)
    battleNetHttp = httpManager

getClassesR :: Handler String
getClassesR = show <$> yesodBnet classes
```
