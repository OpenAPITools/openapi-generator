namespace OpenAPI

open StoreApiHandlerParams
open StoreApiServiceImplementation
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open Microsoft.Azure.WebJobs
open System.IO

module StoreApiHandlers =

    /// <summary>
    /// 
    /// </summary>

    //#region DeleteOrder
    /// <summary>
    /// Delete purchase order by ID
    /// </summary>
   [<FunctionName("DeleteOrder")>]
    let DeleteOrder
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "DELETE", Route = "/v2/store/order/{orderId}")>]
        req:HttpRequest ) =
      
      let result = StoreApiService.DeleteOrder ()
      match result with 
      | DeleteOrderStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | DeleteOrderStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 

    //#region GetInventory
    /// <summary>
    /// Returns pet inventories by status
    /// </summary>
   [<FunctionName("GetInventory")>]
    let GetInventory
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/store/inventory")>]
        req:HttpRequest ) =
      
      let result = StoreApiService.GetInventory ()
      match result with 
      | GetInventoryDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 

    //#region GetOrderById
    /// <summary>
    /// Find purchase order by ID
    /// </summary>
   [<FunctionName("GetOrderById")>]
    let GetOrderById
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/store/order/{orderId}")>]
        req:HttpRequest ) =
      
      let result = StoreApiService.GetOrderById ()
      match result with 
      | GetOrderByIdDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | GetOrderByIdStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | GetOrderByIdStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 

    //#region PlaceOrder
    /// <summary>
    /// Place an order for a pet
    /// </summary>
   [<FunctionName("PlaceOrder")>]
    let PlaceOrder
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/store/order")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = [] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<PlaceOrderBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = StoreApiService.PlaceOrder bodyParams
      match result with 
      | PlaceOrderDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | PlaceOrderStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 


      

