namespace OpenAPI
open StoreApiHandlerParams
open System
open Giraffe
open Microsoft.AspNetCore.Http


module StoreApiServiceInterface =
    
    //#region Service interface
    type IStoreApiService = 
      abstract member DeleteOrder:HttpContext -> DeleteOrderArgs->DeleteOrderResult
      abstract member GetInventory:HttpContext ->GetInventoryResult
      abstract member GetOrderById:HttpContext -> GetOrderByIdArgs->GetOrderByIdResult
      abstract member PlaceOrder:HttpContext -> PlaceOrderArgs->PlaceOrderResult
    //#endregion