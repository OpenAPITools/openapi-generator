namespace OpenAPI

open UserApiHandlerParams
open UserApiServiceImplementation
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open Microsoft.Azure.WebJobs
open System.IO

module UserApiHandlers =

    /// <summary>
    /// 
    /// </summary>

    //#region CreateUser
    /// <summary>
    /// Create user
    /// </summary>
   [<FunctionName("CreateUser")>]
    let CreateUser
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/user")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = [] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<CreateUserBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = UserApiService.CreateUser bodyParams
      match result with 
      | CreateUserDefaultStatusCode resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(0)) 

    //#region CreateUsersWithArrayInput
    /// <summary>
    /// Creates list of users with given input array
    /// </summary>
   [<FunctionName("CreateUsersWithArrayInput")>]
    let CreateUsersWithArrayInput
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/user/createWithArray")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = [] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<CreateUsersWithArrayInputBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = UserApiService.CreateUsersWithArrayInput bodyParams
      match result with 
      | CreateUsersWithArrayInputDefaultStatusCode resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(0)) 

    //#region CreateUsersWithListInput
    /// <summary>
    /// Creates list of users with given input array
    /// </summary>
   [<FunctionName("CreateUsersWithListInput")>]
    let CreateUsersWithListInput
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/user/createWithList")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = [] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<CreateUsersWithListInputBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = UserApiService.CreateUsersWithListInput bodyParams
      match result with 
      | CreateUsersWithListInputDefaultStatusCode resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(0)) 

    //#region DeleteUser
    /// <summary>
    /// Delete user
    /// </summary>
   [<FunctionName("DeleteUser")>]
    let DeleteUser
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "DELETE", Route = "/v2/user/{username}")>]
        req:HttpRequest ) =
      
      let result = UserApiService.DeleteUser ()
      match result with 
      | DeleteUserStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | DeleteUserStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 

    //#region GetUserByName
    /// <summary>
    /// Get user by user name
    /// </summary>
   [<FunctionName("GetUserByName")>]
    let GetUserByName
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/user/{username}")>]
        req:HttpRequest ) =
      
      let result = UserApiService.GetUserByName ()
      match result with 
      | GetUserByNameDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | GetUserByNameStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | GetUserByNameStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 

    //#region LoginUser
    /// <summary>
    /// Logs user into the system
    /// </summary>
   [<FunctionName("LoginUser")>]
    let LoginUser
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/user/login")>]
        req:HttpRequest ) =
      
      let result = UserApiService.LoginUser ()
      match result with 
      | LoginUserDefaultStatusCode resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | LoginUserStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 

    //#region LogoutUser
    /// <summary>
    /// Logs out current logged in user session
    /// </summary>
   [<FunctionName("LogoutUser")>]
    let LogoutUser
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/user/logout")>]
        req:HttpRequest ) =
      
      let result = UserApiService.LogoutUser ()
      match result with 
      | LogoutUserDefaultStatusCode resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(0)) 

    //#region UpdateUser
    /// <summary>
    /// Updated user
    /// </summary>
   [<FunctionName("UpdateUser")>]
    let UpdateUser
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "PUT", Route = "/v2/user/{username}")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = [] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<UpdateUserBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = UserApiService.UpdateUser bodyParams
      match result with 
      | UpdateUserStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | UpdateUserStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 


      

