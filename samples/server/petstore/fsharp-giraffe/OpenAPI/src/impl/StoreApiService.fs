namespace OpenAPI
open System.Collections.Generic
open OpenAPI.Model.Order
open StoreApiHandlerParams
open StoreApiServiceInterface
open System.Collections.Generic
open System
open Giraffe

module StoreApiServiceImplementation =
    
    //#region Service implementation
    type StoreApiServiceImpl() = 
      interface IStoreApiService with
      
        member this.DeleteOrder ctx args =
          if true then 
            let content = "Invalid ID supplied" 
            DeleteOrderStatusCode400 { content = content }
          else
            let content = "Order not found" 
            DeleteOrderStatusCode404 { content = content }

        member this.GetInventory ctx  =
            let content = "successful operation" :> obj :?> IDictionary<string, int> // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetInventoryStatusCode200 { content = content }

        member this.GetOrderById ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Order // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetOrderByIdStatusCode200 { content = content }
          else if true then 
            let content = "Invalid ID supplied" 
            GetOrderByIdStatusCode400 { content = content }
          else
            let content = "Order not found" 
            GetOrderByIdStatusCode404 { content = content }

        member this.PlaceOrder ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Order // this cast is obviously wrong, and is only intended to allow generated project to compile   
            PlaceOrderStatusCode200 { content = content }
          else
            let content = "Invalid Order" 
            PlaceOrderStatusCode400 { content = content }

      //#endregion

    let StoreApiService = StoreApiServiceImpl() :> IStoreApiService