namespace OpenAPI

open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive

module UserApiHandler = 

    /// <summary>
    /// 
    /// </summary>

    //#region CreateUser
    /// <summary>
    /// Create user
    /// </summary>

    //#region Body parameters
    [<CLIMutable>]
    type CreateUserBodyParams = {
      body : User
    }
    //#endregion
  
    let CreateUser  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<CreateUserBodyParams>()
          return! null
        }
    //#endregion

    //#region CreateUsersWithArrayInput
    /// <summary>
    /// Creates list of users with given input array
    /// </summary>

    //#region Body parameters
    [<CLIMutable>]
    type CreateUsersWithArrayInputBodyParams = {
      body : User[]
    }
    //#endregion
  
    let CreateUsersWithArrayInput  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<CreateUsersWithArrayInputBodyParams>()
          return! null
        }
    //#endregion

    //#region CreateUsersWithListInput
    /// <summary>
    /// Creates list of users with given input array
    /// </summary>

    //#region Body parameters
    [<CLIMutable>]
    type CreateUsersWithListInputBodyParams = {
      body : User[]
    }
    //#endregion
  
    let CreateUsersWithListInput  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<CreateUsersWithListInputBodyParams>()
          return! null
        }
    //#endregion

    //#region DeleteUser
    /// <summary>
    /// Delete user
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type DeleteUserPathParams = {
      username : string
    }
    //#endregion
  
    let DeleteUser (args:DeleteUserPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region GetUserByName
    /// <summary>
    /// Get user by user name
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type GetUserByNamePathParams = {
      username : string
    }
    //#endregion
  
    let GetUserByName (args:GetUserByNamePathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region LoginUser
    /// <summary>
    /// Logs user into the system
    /// </summary>

    //#region Query parameters
    [<CLIMutable>]
    type LoginUserQueryParams = {
      username : string
    //#endregion

    //#region Query parameters
      password : string
    }
    //#endregion
  
    let LoginUser  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! queryParams = ctx.TryBindQueryString<LoginUserQueryParams>
          let! queryParams = ctx.TryBindQueryString<LoginUserQueryParams>
          return! null
        }
    //#endregion

    //#region LogoutUser
    /// <summary>
    /// Logs out current logged in user session
    /// </summary>
  
    let LogoutUser  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region UpdateUser
    /// <summary>
    /// Updated user
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type UpdateUserPathParams = {
      username : string
    }
    //#endregion

    //#region Body parameters
    [<CLIMutable>]
    type UpdateUserBodyParams = {
      body : User
    }
    //#endregion
  
    let UpdateUser (args:UpdateUserPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<UpdateUserBodyParams>()
          return! null
        }
    //#endregion

