namespace OpenAPI
open OpenAPI.Model.User
open UserApiHandlerParams
open UserApiServiceInterface
open System.Collections.Generic
open System
open Giraffe

module UserApiServiceImplementation =
    
    //#region Service implementation
    type UserApiServiceImpl() = 
      interface IUserApiService with
      
        member this.CreateUser ctx args =
            let content = "successful operation" 
            CreateUserDefaultStatusCode { content = content }

        member this.CreateUsersWithArrayInput ctx args =
            let content = "successful operation" 
            CreateUsersWithArrayInputDefaultStatusCode { content = content }

        member this.CreateUsersWithListInput ctx args =
            let content = "successful operation" 
            CreateUsersWithListInputDefaultStatusCode { content = content }

        member this.DeleteUser ctx args =
          if true then 
            let content = "Invalid username supplied" 
            DeleteUserStatusCode400 { content = content }
          else
            let content = "User not found" 
            DeleteUserStatusCode404 { content = content }

        member this.GetUserByName ctx args =
          if true then 
            let content = "successful operation" :> obj :?> User // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetUserByNameStatusCode200 { content = content }
          else if true then 
            let content = "Invalid username supplied" 
            GetUserByNameStatusCode400 { content = content }
          else
            let content = "User not found" 
            GetUserByNameStatusCode404 { content = content }

        member this.LoginUser ctx args =
          if true then 
            let content = "successful operation" :> obj :?> string // this cast is obviously wrong, and is only intended to allow generated project to compile   
            LoginUserStatusCode200 { content = content }
          else
            let content = "Invalid username/password supplied" 
            LoginUserStatusCode400 { content = content }

        member this.LogoutUser ctx  =
            let content = "successful operation" 
            LogoutUserDefaultStatusCode { content = content }

        member this.UpdateUser ctx args =
          if true then 
            let content = "Invalid user supplied" 
            UpdateUserStatusCode400 { content = content }
          else
            let content = "User not found" 
            UpdateUserStatusCode404 { content = content }

      //#endregion

    let UserApiService = UserApiServiceImpl() :> IUserApiService