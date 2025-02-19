namespace OpenAPI
open StoreApiHandlerParams
open System
open Microsoft.AspNetCore.Http


module StoreApiServiceInterface =
    
    //#region Service interface
    type IStoreApiService = 
      abstract member DeleteOrder : unit -> DeleteOrderResult
      abstract member GetInventory : unit -> GetInventoryResult
      abstract member GetOrderById : unit -> GetOrderByIdResult
      abstract member PlaceOrder : PlaceOrderBodyParams -> PlaceOrderResult
    //#endregion