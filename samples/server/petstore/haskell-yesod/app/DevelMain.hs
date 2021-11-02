-- | Running your app inside GHCi.
--
-- This option provides significantly faster code reload compared to
-- @yesod devel@. However, you do not get automatic code reload
-- (which may be a benefit, depending on your perspective). To use this:
--
-- 1. Start up GHCi
--
-- $ stack ghci open-api-petstore:lib --no-load --work-dir .stack-work-devel
--
-- 2. Load this module
--
-- > :l app/DevelMain.hs
--
-- 3. Run @update@
--
-- > DevelMain.update
--
-- 4. Your app should now be running, you can connect at http://localhost:3000
--
-- 5. Make changes to your code
--
-- 6. After saving your changes, reload by running:
--
-- > :r
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci
--
-- WARNING: GHCi does not notice changes made to your template files.
-- If you change a template, you'll need to either exit GHCi and reload,
-- or manually @touch@ another Haskell module.

module DevelMain where

import Prelude
import Application (getApplicationRepl, shutdownApp)

import Control.Monad ((>=>))
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp
import GHC.Word

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start


    -- | Start the server in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done = do
        (port, site, app) <- getApplicationRepl
        forkFinally
            (runSettings (setPort port defaultSettings) app)
            -- Note that this implies concurrency
            -- between shutdownApp and the next app that is starting.
            -- Normally this should be fine
            (\_ -> putMVar done () >> shutdownApp site)

-- | kill the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no Yesod app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
