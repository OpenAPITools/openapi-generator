namespace OpenAPI
open System.Collections.Generic
open OpenAPI.Model.Order
open StoreApiHandlerParams
open StoreApiServiceInterface
open System.Collections.Generic
open System

module StoreApiServiceImplementation =
    
    //#region Service implementation
    type StoreApiServiceImpl() = 
      interface IStoreApiService with
      
        member this.DeleteOrder () =
          if true then 
            let content = "Invalid ID supplied" 
            DeleteOrderStatusCode400 { content = content }
          else
            let content = "Order not found" 
            DeleteOrderStatusCode404 { content = content }

        member this.GetInventory () =
            let content = "successful operation" :> obj :?> IDictionary<string, int> // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetInventoryDefaultStatusCode { content = content }

        member this.GetOrderById () =
          if true then 
            let content = "successful operation" :> obj :?> Order // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetOrderByIdDefaultStatusCode { content = content }
          else if true then 
            let content = "Invalid ID supplied" 
            GetOrderByIdStatusCode400 { content = content }
          else
            let content = "Order not found" 
            GetOrderByIdStatusCode404 { content = content }

        member this.PlaceOrder (parameters:PlaceOrderBodyParams) =
          if true then 
            let content = "successful operation" :> obj :?> Order // this cast is obviously wrong, and is only intended to allow generated project to compile   
            PlaceOrderDefaultStatusCode { content = content }
          else
            let content = "Invalid Order" 
            PlaceOrderStatusCode400 { content = content }

      //#endregion

    let StoreApiService = StoreApiServiceImpl() :> IStoreApiService