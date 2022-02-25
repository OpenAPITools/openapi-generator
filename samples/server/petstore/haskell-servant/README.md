# Auto-Generated OpenAPI Bindings to `OpenAPIPetstore`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the OpenAPIPetstore API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

Otherwise, if you already have a Stack project, you can include this package under the `packages` key in your `stack.yaml`:
```yaml
packages:
- location:
    git: https://github.com/yourGitOrg/yourGitRepo
    commit: somecommit
```

## Main Interface

The main interface to this library is in the `OpenAPIPetstore.API` module, which exports the OpenAPIPetstoreBackend type. The OpenAPIPetstoreBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createOpenAPIPetstoreClient` function, which will generate a function for every endpoint of the API.
Then these functions can be invoked with `runOpenAPIPetstoreClientWithManager` or more conveniently with `callOpenAPIPetstoreClient`
(depending if you want an `Either` back or you want to catch) to access the API endpoint they refer to, if the API is served
at the `url` you specified.

For example, if `localhost:8080` is serving the OpenAPIPetstore API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import OpenAPIPetstore.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "http://localhost:8080/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  OpenAPIPetstoreBackend{..} <- API.createOpenAPIPetstoreClient

  -- Any OpenAPIPetstore API call can go here, e.g. here we call `getSomeEndpoint`
  API.callOpenAPIPetstore (mkClientEnv manager url) getSomeEndpoint
```

## Creating a Server

In order to create a server, you must use the `runOpenAPIPetstoreMiddlewareServer` function. However, you unlike the client, in which case you *got* a `OpenAPIPetstoreBackend`
from the library, you must instead *provide* a `OpenAPIPetstoreBackend`. For example, if you have defined handler functions for all the
functions in `OpenAPIPetstore.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import OpenAPIPetstore.API
-- required dependency: wai
import Network.Wai (Middleware)
-- required dependency: wai-extra
import Network.Wai.Middleware.RequestLogger (logStdout)

-- A module you wrote yourself, containing all handlers needed for the OpenAPIPetstoreBackend type.
import OpenAPIPetstore.Handlers

-- If you would like to not use any middlewares you could use runOpenAPIPetstoreServer instead

-- Combined middlewares
requestMiddlewares :: Middleware
requestMiddlewares = logStdout

-- Run a OpenAPIPetstore server on localhost:8080
main :: IO ()
main = do
  let server = OpenAPIPetstoreBackend{..}
      config = Config "http://localhost:8080/"
  runOpenAPIPetstoreMiddlewareServer config requestMiddlewares server
```
