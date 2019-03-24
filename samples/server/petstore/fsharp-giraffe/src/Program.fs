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

  let authScheme = CookieAuthenticationDefaults.AuthenticationScheme

  let accessDenied = setStatusCode 401 >=> text "Access Denied"

  let mustBeUser = requiresAuthentication accessDenied

  let loginHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
          task {
          let issuer = "http://localhost:5000"
          let claims =
                  [
                  Claim(ClaimTypes.Name,      "John",  ClaimValueTypes.String, issuer)
                  Claim(ClaimTypes.Surname,   "Doe",   ClaimValueTypes.String, issuer)
                  Claim(ClaimTypes.Role,      "Admin", ClaimValueTypes.String, issuer)
                  ]
          let identity = ClaimsIdentity(claims, authScheme)
          let user     = ClaimsPrincipal(identity)

          do! ctx.SignInAsync(authScheme, user)

          return! text "Successfully logged in" next ctx
          }

  let userHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
          text ctx.User.Identity.Name next ctx

  let configuredHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
          let configuration = ctx.GetService<IConfiguration>()
          text configuration.["HelloMessage"] next ctx

  let fileUploadHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
          task {
          return!
                  (match ctx.Request.HasFormContentType with
                  | false -> RequestErrors.BAD_REQUEST "Bad request"
                  | true  ->
                  ctx.Request.Form.Files
                  |> Seq.fold (fun acc file -> sprintf "%s\n%s" acc file.FileName) ""
                  |> text) next ctx
          }

  let fileUploadHandler2 =
    fun (next : HttpFunc) (ctx : HttpContext) ->
          task {
          let formFeature = ctx.Features.Get<IFormFeature>()
          let! form = formFeature.ReadFormAsync CancellationToken.None
          return!
                  (form.Files
                  |> Seq.fold (fun acc file -> sprintf "%s\n%s" acc file.FileName) ""
                  |> text) next ctx
          }

  let cacheHandler1 : HttpHandler =
    publicResponseCaching 30 None
    >=> warbler (fun _ ->
          text (Guid.NewGuid().ToString()))

  let cacheHandler2 : HttpHandler =
    responseCaching
          (Public (TimeSpan.FromSeconds (float 30)))
          None
          (Some [| "key1"; "key2" |])
    >=> warbler (fun _ ->
          text (Guid.NewGuid().ToString()))

  let cacheHandler3 : HttpHandler =
    noResponseCaching >=> warbler (fun _ -> text (Guid.NewGuid().ToString()))

  let parsingErrorHandler err = RequestErrors.BAD_REQUEST err

  let dataTypeLookup = Map.empty.Add("string","%s").Add("int","%i").Add("float","%f").Add("int","%i").Add("long", "%f").Add("bool","%b")

  let HttpGet = HttpMethod.Get
  let HttpPost = HttpMethod.Post
  let HttpPut = HttpMethod.Put
  let HttpDelete = HttpMethod.Delete

  let handlers = [
      (HttpPost, routeBind "/pet" (fun () -> PetApiHandler.AddPet ));    
      (HttpDelete, routeBind "/pet/{petId}" (fun (args:UrlParams.DeletePet) -> PetApiHandler.DeletePet args));    
      (HttpGet, routeBind "/pet/findByStatus" (fun () -> PetApiHandler.FindPetsByStatus ));    
      (HttpGet, routeBind "/pet/findByTags" (fun () -> PetApiHandler.FindPetsByTags ));    
      (HttpGet, routeBind "/pet/{petId}" (fun (args:UrlParams.GetPetById) -> PetApiHandler.GetPetById args));    
      (HttpPut, routeBind "/pet" (fun () -> PetApiHandler.UpdatePet ));    
      (HttpPost, routeBind "/pet/{petId}" (fun (args:UrlParams.UpdatePetWithForm) -> PetApiHandler.UpdatePetWithForm args));    
      (HttpPost, routeBind "/pet/{petId}/uploadImage" (fun (args:UrlParams.UploadFile) -> PetApiHandler.UploadFile args));    
      (HttpDelete, routeBind "/store/order/{orderId}" (fun (args:UrlParams.DeleteOrder) -> StoreApiHandler.DeleteOrder args));    
      (HttpGet, routeBind "/store/inventory" (fun () -> StoreApiHandler.GetInventory ));    
      (HttpGet, routeBind "/store/order/{orderId}" (fun (args:UrlParams.GetOrderById) -> StoreApiHandler.GetOrderById args));    
      (HttpPost, routeBind "/store/order" (fun () -> StoreApiHandler.PlaceOrder ));    
      (HttpPost, routeBind "/user" (fun () -> UserApiHandler.CreateUser ));    
      (HttpPost, routeBind "/user/createWithArray" (fun () -> UserApiHandler.CreateUsersWithArrayInput ));    
      (HttpPost, routeBind "/user/createWithList" (fun () -> UserApiHandler.CreateUsersWithListInput ));    
      (HttpDelete, routeBind "/user/{username}" (fun (args:UrlParams.DeleteUser) -> UserApiHandler.DeleteUser args));    
      (HttpGet, routeBind "/user/{username}" (fun (args:UrlParams.GetUserByName) -> UserApiHandler.GetUserByName args));    
      (HttpGet, routeBind "/user/login" (fun () -> UserApiHandler.LoginUser ));    
      (HttpGet, routeBind "/user/logout" (fun () -> UserApiHandler.LogoutUser ));    
      (HttpPut, routeBind "/user/{username}" (fun (args:UrlParams.UpdateUser) -> UserApiHandler.UpdateUser args));    
  ]

  let get = handlers |> Seq.where (fun x -> fst(x) = HttpGet) |> Seq.map (fun x -> snd(x)) |> Seq.toList
  let post = handlers |> Seq.where (fun x -> fst(x) = HttpPost) |> Seq.map (fun x -> snd(x)) |> Seq.toList
  let put = handlers |> Seq.where (fun x -> fst(x) = HttpPut) |> Seq.map (fun x -> snd(x)) |> Seq.toList
  let delete = handlers |> Seq.where (fun x -> fst(x) = HttpDelete) |> Seq.map (fun x -> snd(x)) |> Seq.toList

  let webApp =
    choose [
          GET >=> choose get;
          POST >=> choose post;
          PUT >=> choose put;
          DELETE >=> choose delete;
          RequestErrors.notFound (text "Not Found") 
    ]

  // ---------------------------------
  // Main
  // ---------------------------------

  let cookieAuth (o : CookieAuthenticationOptions) =
    do
        o.Cookie.HttpOnly     <- true
        o.Cookie.SecurePolicy <- CookieSecurePolicy.SameAsRequest
        o.SlidingExpiration   <- true
        o.ExpireTimeSpan      <- TimeSpan.FromDays 7.0

  let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler)
      .UseStaticFiles()
      .UseAuthentication()
      .UseResponseCaching()
      .UseGiraffe webApp

  let configureServices (services : IServiceCollection) =
    services
          .AddResponseCaching()
          .AddGiraffe()
          .AddAuthentication(authScheme)
          .AddCookie(cookieAuth)   |> ignore
    services.AddDataProtection() |> ignore

  let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                  .AddConsole()
                  .AddDebug() |> ignore

  [<EntryPoint>]
  let main _ =
    WebHost.CreateDefaultBuilder()
          .Configure(Action<IApplicationBuilder> configureApp)
          .ConfigureServices(configureServices)
          .ConfigureLogging(configureLogging)
          .Build()
          .Run()
    0