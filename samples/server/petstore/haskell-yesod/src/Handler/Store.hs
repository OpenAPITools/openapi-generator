{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Store where

import           Import


-- | Delete purchase order by ID
--
-- For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
-- operationId: deleteOrder
deleteStoreOrderByTextR :: Text -- ^ ID of the order that needs to be deleted
                        -> Handler Value
deleteStoreOrderByTextR orderId = notImplemented

-- | Returns pet inventories by status
--
-- Returns a map of status codes to quantities
-- operationId: getInventory
getStoreInventoryR :: Handler Value
getStoreInventoryR = notImplemented

-- | Find purchase order by ID
--
-- For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
-- operationId: getOrderById
getStoreOrderByInt64R :: Int64 -- ^ ID of pet that needs to be fetched
                      -> Handler Value
getStoreOrderByInt64R orderId = notImplemented

-- | Place an order for a pet
--
-- operationId: placeOrder
postStoreOrderR :: Handler Value
postStoreOrderR = notImplemented
