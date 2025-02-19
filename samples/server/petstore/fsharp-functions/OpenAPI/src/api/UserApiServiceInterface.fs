namespace OpenAPI
open UserApiHandlerParams
open System
open Microsoft.AspNetCore.Http


module UserApiServiceInterface =
    
    //#region Service interface
    type IUserApiService = 
      abstract member CreateUser : CreateUserBodyParams -> CreateUserResult
      abstract member CreateUsersWithArrayInput : CreateUsersWithArrayInputBodyParams -> CreateUsersWithArrayInputResult
      abstract member CreateUsersWithListInput : CreateUsersWithListInputBodyParams -> CreateUsersWithListInputResult
      abstract member DeleteUser : unit -> DeleteUserResult
      abstract member GetUserByName : unit -> GetUserByNameResult
      abstract member LoginUser : unit -> LoginUserResult
      abstract member LogoutUser : unit -> LogoutUserResult
      abstract member UpdateUser : UpdateUserBodyParams -> UpdateUserResult
    //#endregion