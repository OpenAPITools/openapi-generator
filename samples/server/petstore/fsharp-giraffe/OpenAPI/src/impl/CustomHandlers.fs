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
open Giraffe.GiraffeViewEngine
open Microsoft.AspNetCore.Authentication.OAuth
open System.Threading.Tasks
open AspNet.Security.ApiKey.Providers.Events

module CustomHandlers = 

  let cookieAuth (o : CookieAuthenticationOptions) =
    do
        o.Cookie.HttpOnly     <- true
        o.Cookie.SecurePolicy <- CookieSecurePolicy.SameAsRequest
        o.SlidingExpiration   <- true
        o.ExpireTimeSpan      <- TimeSpan.FromDays 7.0

  
  let onCreatingTicket name (ctx:OAuthCreatingTicketContext) = 
    task {
        // implement post-authentication logic for oAuth handlers here
       ()
    } :> Task

  let validateApiKey key = 
    raise (NotImplementedException("API key validation must be implemented"))

  let setApiKeyEvents name (events:ApiKeyEvents) = 
    events.OnApiKeyValidated <- (fun ctx -> 
        task {
          // implement your validation/authentication logic for api key handlers here
          if validateApiKey ctx.ApiKey then  
          // to interact properly with Giraffe's handlers, you will need to manually set the identity 
          // let claims = ...
          // let identity = ClaimsIdentity(claims, ApiKeyDefaults.AuthenticationScheme)
          // ctx.HttpContext.User <- ClaimsPrincipal([|identity|])
            ctx.Success()
        } :> Task
    )
    events

  let setOAuthOptions name (options:OAuthOptions) scopes (settings:IConfiguration) = 
    options.ClientId <- settings.[name + "ClientId"]
    options.ClientSecret <- settings.[name + "ClientSecret"]
    for scope in scopes do
      options.Scope.Add scope

    options.Events.OnCreatingTicket <- Func<OAuthCreatingTicketContext,Tasks.Task>(onCreatingTicket name)
    match name with
    | "Google" ->
      ()  
    | "GitHub" ->
      ()
    | _ -> 
      ()
  
  let logout = signOut CookieAuthenticationDefaults.AuthenticationScheme >=> redirectTo false "/"

  let loginView =
    html [] [
        head [] [
            title [] [ str "Welcome" ]
        ]
        body [] [
            h1 [] [ str "Welcome" ]
            a [_href "/login-with-api_key"] [ str "Login with api_key" ]
            a [_href "/login-with-petstore_auth"] [ str "Login with petstore_auth" ]
        ]
    ]
  
  let redirectToLogin : HttpHandler = 
    htmlView loginView    
  
  let handlers : HttpHandler list = [
    // insert your handlers here
    GET >=> 
      choose [
        route "/login" >=> redirectToLogin
        route "/login-with-api_key" >=> challenge "api_key"
        route "/login-with-petstore_auth" >=> challenge "petstore_auth"
        route "/logout" >=> logout
      ]
  ]

  let configureWebHost (builder: IWebHostBuilder)  =
      // builder
      //  .UseContentRoot("content")
      //  .UseWebRoot("static")
      builder

  let configureApp (app : IApplicationBuilder) =
    app

  let configureServices (services:IServiceCollection) (authBuilder:AuthenticationBuilder) = 
    ()
