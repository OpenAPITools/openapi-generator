namespace OpenAPI

open System.Collections.Generic
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open UserApiHandlerParams
open UserApiServiceInterface
open UserApiServiceImplementation
open OpenAPI.Model.User

module UserApiHandler = 

    /// <summary>
    /// 
    /// </summary>

    //#region CreateUser
    /// <summary>
    /// Create user
    /// </summary>

    let CreateUser  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<CreateUserBodyParams>()
          let serviceArgs = {     bodyParams=bodyParams } : CreateUserArgs
          let result = UserApiService.CreateUser ctx serviceArgs
          return! (match result with 
                      | CreateUserDefaultStatusCode resolved ->
                            setStatusCode 0 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region CreateUsersWithArrayInput
    /// <summary>
    /// Creates list of users with given input array
    /// </summary>

    let CreateUsersWithArrayInput  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<CreateUsersWithArrayInputBodyParams>()
          let serviceArgs = {     bodyParams=bodyParams } : CreateUsersWithArrayInputArgs
          let result = UserApiService.CreateUsersWithArrayInput ctx serviceArgs
          return! (match result with 
                      | CreateUsersWithArrayInputDefaultStatusCode resolved ->
                            setStatusCode 0 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region CreateUsersWithListInput
    /// <summary>
    /// Creates list of users with given input array
    /// </summary>

    let CreateUsersWithListInput  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<CreateUsersWithListInputBodyParams>()
          let serviceArgs = {     bodyParams=bodyParams } : CreateUsersWithListInputArgs
          let result = UserApiService.CreateUsersWithListInput ctx serviceArgs
          return! (match result with 
                      | CreateUsersWithListInputDefaultStatusCode resolved ->
                            setStatusCode 0 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region DeleteUser
    /// <summary>
    /// Delete user
    /// </summary>

    let DeleteUser (pathParams:DeleteUserPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let serviceArgs = {    pathParams=pathParams;  } : DeleteUserArgs
          let result = UserApiService.DeleteUser ctx serviceArgs
          return! (match result with 
                      | DeleteUserStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | DeleteUserStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region GetUserByName
    /// <summary>
    /// Get user by user name
    /// </summary>

    let GetUserByName (pathParams:GetUserByNamePathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let serviceArgs = {    pathParams=pathParams;  } : GetUserByNameArgs
          let result = UserApiService.GetUserByName ctx serviceArgs
          return! (match result with 
                      | GetUserByNameStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | GetUserByNameStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | GetUserByNameStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region LoginUser
    /// <summary>
    /// Logs user into the system
    /// </summary>

    let LoginUser  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let queryParams = ctx.TryBindQueryString<LoginUserQueryParams>()
          let serviceArgs = {  queryParams=queryParams;    } : LoginUserArgs
          let result = UserApiService.LoginUser ctx serviceArgs
          return! (match result with 
                      | LoginUserStatusCode200 resolved ->
                            setStatusCode 200 >=> text resolved.content
                      | LoginUserStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region LogoutUser
    /// <summary>
    /// Logs out current logged in user session
    /// </summary>

    let LogoutUser  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let result = UserApiService.LogoutUser ctx 
          return! (match result with 
                      | LogoutUserDefaultStatusCode resolved ->
                            setStatusCode 0 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region UpdateUser
    /// <summary>
    /// Updated user
    /// </summary>

    let UpdateUser (pathParams:UpdateUserPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<UpdateUserBodyParams>()
          let serviceArgs = {    pathParams=pathParams; bodyParams=bodyParams } : UpdateUserArgs
          let result = UserApiService.UpdateUser ctx serviceArgs
          return! (match result with 
                      | UpdateUserStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | UpdateUserStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
          ) next ctx
        }
    //#endregion


    
