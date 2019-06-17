namespace OpenAPI.Tests

open System
open System.Net
open System.Net.Http
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open Xunit
open System.Text

module TestHelper = 
  // ---------------------------------
  // Test server/client setup
  // ---------------------------------

  let createHost() =
      WebHostBuilder()
          .UseContentRoot(Directory.GetCurrentDirectory())
          .Configure(Action<IApplicationBuilder> OpenAPI.App.configureApp)
          .ConfigureServices(Action<IServiceCollection> OpenAPI.App.configureServices)

  // ---------------------------------
  // Helper functions
  // ---------------------------------

  let HttpGet (client : HttpClient) (path : string) =
      client.GetAsync path
      |> Async.AwaitTask
      |> Async.RunSynchronously

  let HttpPost (client: HttpClient) (path : string) content  =
      client.PostAsync(path, content)
      |> Async.AwaitTask
      |> Async.RunSynchronously

  let HttpPut (client: HttpClient)  (path : string) content =
      client.PutAsync(path, content)
      |> Async.AwaitTask
      |> Async.RunSynchronously

  let HttpDelete (client: HttpClient)  (path : string) =
      client.DeleteAsync(path)
      |> Async.AwaitTask
      |> Async.RunSynchronously

  let createRequest (method : HttpMethod) (path : string) =
      let url = "http://127.0.0.1" + path
      new HttpRequestMessage(method, url)

  let addCookiesFromResponse (response : HttpResponseMessage)
                            (request  : HttpRequestMessage) =
      request.Headers.Add("Cookie", response.Headers.GetValues("Set-Cookie"))
      request

  let makeRequest (client : HttpClient) request =
      request
      |> client.SendAsync

  let isStatus (code : HttpStatusCode) (response : HttpResponseMessage) =
      Assert.Equal(code, response.StatusCode)
      response

  let isOfType (contentType : string) (response : HttpResponseMessage) =
      Assert.Equal(contentType, response.Content.Headers.ContentType.MediaType)
      response

  let readText (response : HttpResponseMessage) =
      response.Content.ReadAsStringAsync()
      |> Async.AwaitTask            
      |> Async.RunSynchronously

  let shouldEqual expected actual =
      Assert.Equal(expected, actual)

  let getConverter mediaType = 
    (fun (x:string) -> 
      match mediaType with
      | "application/x-www-form-urlencoded" -> raise (NotSupportedException()) // TODO - implement FormUrlEncodedContent
      | _ -> x |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent)