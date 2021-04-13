namespace OpenAPI

open System.Collections.Generic
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open StoreApiHandlerParams
open StoreApiServiceInterface
open StoreApiServiceImplementation
open System.Collections.Generic
open OpenAPI.Model.Order

module StoreApiHandler = 

    /// <summary>
    /// 
    /// </summary>

    //#region DeleteOrder
    /// <summary>
    /// Delete purchase order by ID
    /// </summary>

    let DeleteOrder (pathParams:DeleteOrderPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let serviceArgs = {    pathParams=pathParams;  } : DeleteOrderArgs
          let result = StoreApiService.DeleteOrder ctx serviceArgs
          return! (match result with 
                      | DeleteOrderStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | DeleteOrderStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region GetInventory
    /// <summary>
    /// Returns pet inventories by status
    /// </summary>

    let GetInventory  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let result = StoreApiService.GetInventory ctx 
          return! (match result with 
                      | GetInventoryStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
          ) next ctx
        }
    //#endregion

    //#region GetOrderById
    /// <summary>
    /// Find purchase order by ID
    /// </summary>

    let GetOrderById (pathParams:GetOrderByIdPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let serviceArgs = {    pathParams=pathParams;  } : GetOrderByIdArgs
          let result = StoreApiService.GetOrderById ctx serviceArgs
          return! (match result with 
                      | GetOrderByIdStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | GetOrderByIdStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | GetOrderByIdStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region PlaceOrder
    /// <summary>
    /// Place an order for a pet
    /// </summary>

    let PlaceOrder  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<PlaceOrderBodyParams>()
          let serviceArgs = {     bodyParams=bodyParams } : PlaceOrderArgs
          let result = StoreApiService.PlaceOrder ctx serviceArgs
          return! (match result with 
                      | PlaceOrderStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | PlaceOrderStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
          ) next ctx
        }
    //#endregion


    
