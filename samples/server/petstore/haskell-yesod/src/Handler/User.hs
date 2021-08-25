{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import           Import


-- | Create user
--
-- This can only be done by the logged in user.
-- operationId: createUser
postUserR :: Handler Value
postUserR = notImplemented

-- | Creates list of users with given input array
--
-- operationId: createUsersWithArrayInput
postUserCreateWithArrayR :: Handler Value
postUserCreateWithArrayR = notImplemented

-- | Creates list of users with given input array
--
-- operationId: createUsersWithListInput
postUserCreateWithListR :: Handler Value
postUserCreateWithListR = notImplemented

-- | Delete user
--
-- This can only be done by the logged in user.
-- operationId: deleteUser
deleteUserByTextR :: Text -- ^ The name that needs to be deleted
                  -> Handler Value
deleteUserByTextR username = notImplemented

-- | Get user by user name
--
-- operationId: getUserByName
getUserByTextR :: Text -- ^ The name that needs to be fetched. Use user1 for testing.
               -> Handler Value
getUserByTextR username = notImplemented

-- | Logs user into the system
--
-- operationId: loginUser
getUserLoginR :: Handler Value
getUserLoginR = notImplemented

-- | Logs out current logged in user session
--
-- operationId: logoutUser
getUserLogoutR :: Handler Value
getUserLogoutR = notImplemented

-- | Updated user
--
-- This can only be done by the logged in user.
-- operationId: updateUser
putUserByTextR :: Text -- ^ name that need to be deleted
               -> Handler Value
putUserByTextR username = notImplemented
