namespace OpenAPI
open OpenAPI.Model.User
open UserApiHandlerParams
open UserApiServiceInterface
open System.Collections.Generic
open System

module UserApiServiceImplementation =
    
    //#region Service implementation
    type UserApiServiceImpl() = 
      interface IUserApiService with
      
        member this.CreateUser (parameters:CreateUserBodyParams) =
            let content = "successful operation" 
            CreateUserDefaultStatusCode { content = content }

        member this.CreateUsersWithArrayInput (parameters:CreateUsersWithArrayInputBodyParams) =
            let content = "successful operation" 
            CreateUsersWithArrayInputDefaultStatusCode { content = content }

        member this.CreateUsersWithListInput (parameters:CreateUsersWithListInputBodyParams) =
            let content = "successful operation" 
            CreateUsersWithListInputDefaultStatusCode { content = content }

        member this.DeleteUser () =
          if true then 
            let content = "Invalid username supplied" 
            DeleteUserStatusCode400 { content = content }
          else
            let content = "User not found" 
            DeleteUserStatusCode404 { content = content }

        member this.GetUserByName () =
          if true then 
            let content = "successful operation" :> obj :?> User // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetUserByNameDefaultStatusCode { content = content }
          else if true then 
            let content = "Invalid username supplied" 
            GetUserByNameStatusCode400 { content = content }
          else
            let content = "User not found" 
            GetUserByNameStatusCode404 { content = content }

        member this.LoginUser () =
          if true then 
            let content = "successful operation" :> obj :?> string // this cast is obviously wrong, and is only intended to allow generated project to compile   
            LoginUserDefaultStatusCode { content = content }
          else
            let content = "Invalid username/password supplied" 
            LoginUserStatusCode400 { content = content }

        member this.LogoutUser () =
            let content = "successful operation" 
            LogoutUserDefaultStatusCode { content = content }

        member this.UpdateUser (parameters:UpdateUserBodyParams) =
          if true then 
            let content = "Invalid user supplied" 
            UpdateUserStatusCode400 { content = content }
          else
            let content = "User not found" 
            UpdateUserStatusCode404 { content = content }

      //#endregion

    let UserApiService = UserApiServiceImpl() :> IUserApiService