namespace OpenAPI

open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authentication.OAuth
open System
open Giraffe
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Extensions.Configuration
open AspNet.Security.ApiKey.Providers.Extensions
open AspNet.Security.ApiKey.Providers.Events


module AuthSchemes =

  let accessDenied : HttpHandler = setStatusCode 401 >=> text "Access Denied"

  let buildGoogle (builder:AuthenticationBuilder) name authorizationUrl scopes (settings:IConfiguration) = 
    builder.AddGoogle(fun googleOptions -> CustomHandlers.setOAuthOptions "Google" googleOptions scopes settings)

  let buildGitHub (builder:AuthenticationBuilder) name authorizationUrl scopes (settings:IConfiguration) = 
    builder.AddGitHub(fun githubOptions -> CustomHandlers.setOAuthOptions "GitHub" githubOptions scopes settings)

  let buildOAuth (builder:AuthenticationBuilder) (name:string) authorizationUrl scopes (settings:IConfiguration) = 
    builder.AddOAuth(name, (fun (options:OAuthOptions) -> 
      options.AuthorizationEndpoint <- authorizationUrl
      options.TokenEndpoint <- settings.[name + "TokenUrl"]
      options.CallbackPath <- PathString(settings.[name + "CallbackPath"])
      CustomHandlers.setOAuthOptions "" options scopes settings
  ))

  let OAuthBuilders = Map.empty.Add("Google", buildGoogle).Add("GitHub", buildGitHub)

  let checkEnvironment (settings:IConfiguration) name =
    if (isNull settings.[name + "ClientId"]) then
      raise (Exception(name + "ClientId is not set."))
    else if (isNull settings.[name + "ClientSecret"]) then
      raise (Exception((name + "ClientSecret is not set.")))

  let getOAuthBuilder settings name =  
    // check that "xxxClientId" and "xxxClientSecret" configuration variables have been set for all OAuth providers
    checkEnvironment settings name
    if OAuthBuilders.ContainsKey(name) then
      OAuthBuilders.[name]
    else
      buildOAuth

  let configureOAuth (settings:IConfiguration) services =
    (getOAuthBuilder settings "petstore_auth") services "petstore_auth" "http://petstore.swagger.io/api/oauth/dialog" ["write:pets";"read:pets";] settings

  let buildApiKeyAuth name (services:AuthenticationBuilder) =
    services.AddApiKey(fun options -> 
      options.Header <- name
      options.HeaderKey <- String.Empty
      let events = ApiKeyEvents()
      options.Events <- CustomHandlers.setApiKeyEvents name events
    )

  let configureApiKeyAuth (settings:IConfiguration) services =
    buildApiKeyAuth "api_key" services
    raise (NotImplementedException("API key security scheme outside of header has not yet been implemented"))


  let configureCookie (builder:AuthenticationBuilder) =
      builder.AddCookie(CustomHandlers.cookieAuth)

  let configureServices (services:IServiceCollection) = 
    let serviceProvider = services.BuildServiceProvider()
    let settings = serviceProvider.GetService<IConfiguration>()
    services.AddAuthentication(fun o -> o.DefaultScheme <- CookieAuthenticationDefaults.AuthenticationScheme)
    |> configureOAuth settings 
    |> configureApiKeyAuth settings
    |> configureCookie
    
  let (|||) v1 v2 = 
      match v1 with 
      | Some v -> v1
      | None -> v2

  // this can be replaced with ctx.GetCookieValue in Giraffe >= 3.6
  let getCookieValue (ctx:HttpContext) (key : string)  =
        match ctx.Request.Cookies.TryGetValue key with
        | true , cookie -> Some cookie
        | false, _ -> None

  