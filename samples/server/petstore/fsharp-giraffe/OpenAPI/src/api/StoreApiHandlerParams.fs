namespace OpenAPI

open System.Collections.Generic
open OpenAPI.Model.Order
open System.Collections.Generic
open System

module StoreApiHandlerParams = 

    //#region Path parameters
    [<CLIMutable>]
    type DeleteOrderPathParams = {
      orderId : string ;
    }
    //#endregion

    
    type DeleteOrderStatusCode400Response = {
      content:string;
      
    }
    
    type DeleteOrderStatusCode404Response = {
      content:string;
      
    }
    type DeleteOrderResult = DeleteOrderStatusCode400 of DeleteOrderStatusCode400Response|DeleteOrderStatusCode404 of DeleteOrderStatusCode404Response

    type DeleteOrderArgs = {
      pathParams:DeleteOrderPathParams;
    }

    
    type GetInventoryDefaultStatusCodeResponse = {
      content:IDictionary<string, int>;
      
    }
    type GetInventoryResult = GetInventoryDefaultStatusCode of GetInventoryDefaultStatusCodeResponse

    //#region Path parameters
    [<CLIMutable>]
    type GetOrderByIdPathParams = {
      orderId : int64 ;
    }
    //#endregion

    
    type GetOrderByIdDefaultStatusCodeResponse = {
      content:Order;
      
    }
    
    type GetOrderByIdStatusCode400Response = {
      content:string;
      
    }
    
    type GetOrderByIdStatusCode404Response = {
      content:string;
      
    }
    type GetOrderByIdResult = GetOrderByIdDefaultStatusCode of GetOrderByIdDefaultStatusCodeResponse|GetOrderByIdStatusCode400 of GetOrderByIdStatusCode400Response|GetOrderByIdStatusCode404 of GetOrderByIdStatusCode404Response

    type GetOrderByIdArgs = {
      pathParams:GetOrderByIdPathParams;
    }

    //#region Body parameters
    [<CLIMutable>]
    type PlaceOrderBodyParams = Order 
    //#endregion

    
    type PlaceOrderDefaultStatusCodeResponse = {
      content:Order;
      
    }
    
    type PlaceOrderStatusCode400Response = {
      content:string;
      
    }
    type PlaceOrderResult = PlaceOrderDefaultStatusCode of PlaceOrderDefaultStatusCodeResponse|PlaceOrderStatusCode400 of PlaceOrderStatusCode400Response

    type PlaceOrderArgs = {
      bodyParams:PlaceOrderBodyParams
    }
    