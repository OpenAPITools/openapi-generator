namespace OpenAPI

open System
open System.Net.Http
open System.Security.Claims
open System.Threading
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Diagnostics
open Giraffe.GiraffeViewEngine
open AspNet.Security.ApiKey.Providers

open PetApiHandlerParams
open StoreApiHandlerParams
open UserApiHandlerParams
open Giraffe

module App =

  // ---------------------------------
  // Error handler
  // ---------------------------------

  let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

  // ---------------------------------
  // Web app
  // ---------------------------------

  let HttpGet = GET
  let HttpPost = POST
  let HttpPut = PUT
  let HttpDelete = DELETE

  let authFailure : HttpHandler = 
    setStatusCode 401 >=> text "You must be authenticated to access this resource."

  let webApp =
    choose (CustomHandlers.handlers @ [
      HttpPost >=> route "/v2/pet" >=> requiresAuthentication authFailure >=>  PetApiHandler.AddPet;
      HttpDelete >=> routeBind<DeletePetPathParams> "/v2/pet/{petId}"  (fun x -> requiresAuthentication authFailure >=>  PetApiHandler.DeletePet x);
      HttpGet >=> route "/v2/pet/findByStatus" >=> requiresAuthentication authFailure >=>  PetApiHandler.FindPetsByStatus;
      HttpGet >=> route "/v2/pet/findByTags" >=> requiresAuthentication authFailure >=>  PetApiHandler.FindPetsByTags;
      HttpGet >=> routeBind<GetPetByIdPathParams> "/v2/pet/{petId}"  (fun x -> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  PetApiHandler.GetPetById x);
      HttpPut >=> route "/v2/pet" >=> requiresAuthentication authFailure >=>  PetApiHandler.UpdatePet;
      HttpPost >=> routeBind<UpdatePetWithFormPathParams> "/v2/pet/{petId}"  (fun x -> requiresAuthentication authFailure >=>  PetApiHandler.UpdatePetWithForm x);
      HttpPost >=> routeBind<UploadFilePathParams> "/v2/pet/{petId}/uploadImage"  (fun x -> requiresAuthentication authFailure >=>  PetApiHandler.UploadFile x);
      HttpDelete >=> routeBind<DeleteOrderPathParams> "/v2/store/order/{orderId}"  (fun x ->  StoreApiHandler.DeleteOrder x);
      HttpGet >=> route "/v2/store/inventory" >=> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  StoreApiHandler.GetInventory;
      HttpGet >=> routeBind<GetOrderByIdPathParams> "/v2/store/order/{orderId}"  (fun x ->  StoreApiHandler.GetOrderById x);
      HttpPost >=> route "/v2/store/order" >=>  StoreApiHandler.PlaceOrder;
      HttpPost >=> route "/v2/user" >=> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  UserApiHandler.CreateUser;
      HttpPost >=> route "/v2/user/createWithArray" >=> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  UserApiHandler.CreateUsersWithArrayInput;
      HttpPost >=> route "/v2/user/createWithList" >=> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  UserApiHandler.CreateUsersWithListInput;
      HttpDelete >=> routeBind<DeleteUserPathParams> "/v2/user/{username}"  (fun x -> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  UserApiHandler.DeleteUser x);
      HttpGet >=> routeBind<GetUserByNamePathParams> "/v2/user/{username}"  (fun x ->  UserApiHandler.GetUserByName x);
      HttpGet >=> route "/v2/user/login" >=>  UserApiHandler.LoginUser;
      HttpGet >=> route "/v2/user/logout" >=> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  UserApiHandler.LogoutUser;
      HttpPut >=> routeBind<UpdateUserPathParams> "/v2/user/{username}"  (fun x -> challenge ApiKeyDefaults.AuthenticationScheme >=> requiresAuthentication authFailure >=>  UserApiHandler.UpdateUser x);
      RequestErrors.notFound (text "Not Found") 
    ])
  // ---------------------------------
  // Main
  // ---------------------------------

  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
      .UseStaticFiles()
      .UseAuthentication()
      .UseResponseCaching() |> ignore
    CustomHandlers.configureApp app |> ignore
    app.UseGiraffe webApp |> ignore
    

  let configureServices (services : IServiceCollection) =
    services
          .AddResponseCaching()
          .AddGiraffe()
          |> AuthSchemes.configureServices      
          |> CustomHandlers.configureServices services
          |> ignore
    services.AddDataProtection() |> ignore

  let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                  .AddConsole()
                  .AddDebug() |> ignore

  [<EntryPoint>]
  let main _ =
    let builder = WebHost.CreateDefaultBuilder()
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> CustomHandlers.configureWebHost
    builder.Build()
            .Run()
    0