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
open TestHelper
open OpenAPI.StoreApiHandler
open OpenAPI.StoreApiHandlerParams

module StoreApiHandlerTestsHelper =

  ()

  ()

  ()


  let mutable PlaceOrderExamples = Map.empty
  let mutable PlaceOrderBody = ""

  PlaceOrderBody <- WebUtility.HtmlDecode "{
  &quot;petId&quot; : 6,
  &quot;quantity&quot; : 1,
  &quot;id&quot; : 0,
  &quot;shipDate&quot; : &quot;2000-01-23T04:56:07.000+00:00&quot;,
  &quot;complete&quot; : false,
  &quot;status&quot; : &quot;placed&quot;
}"
  PlaceOrderExamples <- PlaceOrderExamples.Add("application/json", PlaceOrderBody)

  let getPlaceOrderExample mediaType =
    PlaceOrderExamples.[mediaType]
      |> getConverter mediaType
