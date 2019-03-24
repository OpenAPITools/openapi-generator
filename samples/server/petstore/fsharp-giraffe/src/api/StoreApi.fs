namespace OpenAPI

open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive

module StoreApiHandler = 

    /// <summary>
    /// 
    /// </summary>

    //#region DeleteOrder
    /// <summary>
    /// Delete purchase order by ID
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type DeleteOrderPathParams = {
      orderId : string
    }
    //#endregion
  
    let DeleteOrder (args:DeleteOrderPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region GetInventory
    /// <summary>
    /// Returns pet inventories by status
    /// </summary>
  
    let GetInventory  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region GetOrderById
    /// <summary>
    /// Find purchase order by ID
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type GetOrderByIdPathParams = {
      orderId : int64
    }
    //#endregion
  
    let GetOrderById (args:GetOrderByIdPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region PlaceOrder
    /// <summary>
    /// Place an order for a pet
    /// </summary>

    //#region Body parameters
    [<CLIMutable>]
    type PlaceOrderBodyParams = {
      body : Order
    }
    //#endregion
  
    let PlaceOrder  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<PlaceOrderBodyParams>()
          return! null
        }
    //#endregion

