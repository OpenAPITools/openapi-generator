namespace OpenAPI
open UserApiHandlerParams
open System
open Giraffe
open Microsoft.AspNetCore.Http


module UserApiServiceInterface =
    
    //#region Service interface
    type IUserApiService = 
      abstract member CreateUser:HttpContext -> CreateUserArgs->CreateUserResult
      abstract member CreateUsersWithArrayInput:HttpContext -> CreateUsersWithArrayInputArgs->CreateUsersWithArrayInputResult
      abstract member CreateUsersWithListInput:HttpContext -> CreateUsersWithListInputArgs->CreateUsersWithListInputResult
      abstract member DeleteUser:HttpContext -> DeleteUserArgs->DeleteUserResult
      abstract member GetUserByName:HttpContext -> GetUserByNameArgs->GetUserByNameResult
      abstract member LoginUser:HttpContext -> LoginUserArgs->LoginUserResult
      abstract member LogoutUser:HttpContext ->LogoutUserResult
      abstract member UpdateUser:HttpContext -> UpdateUserArgs->UpdateUserResult
    //#endregion