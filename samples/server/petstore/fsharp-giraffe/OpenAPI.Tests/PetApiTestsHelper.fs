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
open OpenAPI.PetApiHandler
open OpenAPI.PetApiHandlerParams

module PetApiHandlerTestsHelper =


  let mutable AddPetExamples = Map.empty
  let mutable AddPetBody = ""

  AddPetBody <- WebUtility.HtmlDecode "{
  &quot;photoUrls&quot; : [ &quot;photoUrls&quot;, &quot;photoUrls&quot; ],
  &quot;name&quot; : &quot;doggie&quot;,
  &quot;id&quot; : 0,
  &quot;category&quot; : {
    &quot;name&quot; : &quot;name&quot;,
    &quot;id&quot; : 6
  },
  &quot;tags&quot; : [ {
    &quot;name&quot; : &quot;name&quot;,
    &quot;id&quot; : 1
  }, {
    &quot;name&quot; : &quot;name&quot;,
    &quot;id&quot; : 1
  } ],
  &quot;status&quot; : &quot;available&quot;
}"
  AddPetExamples <- AddPetExamples.Add("application/json", AddPetBody)

  AddPetBody <- WebUtility.HtmlDecode "&lt;Pet&gt;
  &lt;id&gt;123456789&lt;/id&gt;
  &lt;name&gt;doggie&lt;/name&gt;
  &lt;photoUrls&gt;
    &lt;photoUrls&gt;aeiou&lt;/photoUrls&gt;
  &lt;/photoUrls&gt;
  &lt;tags&gt;
  &lt;/tags&gt;
  &lt;status&gt;aeiou&lt;/status&gt;
&lt;/Pet&gt;"
  AddPetExamples <- AddPetExamples.Add("application/xml", AddPetBody)

  let getAddPetExample mediaType =
    AddPetExamples.[mediaType]
      |> getConverter mediaType
  ()

  ()

  ()

  ()


  let mutable UpdatePetExamples = Map.empty
  let mutable UpdatePetBody = ""

  UpdatePetBody <- WebUtility.HtmlDecode "{
  &quot;photoUrls&quot; : [ &quot;photoUrls&quot;, &quot;photoUrls&quot; ],
  &quot;name&quot; : &quot;doggie&quot;,
  &quot;id&quot; : 0,
  &quot;category&quot; : {
    &quot;name&quot; : &quot;name&quot;,
    &quot;id&quot; : 6
  },
  &quot;tags&quot; : [ {
    &quot;name&quot; : &quot;name&quot;,
    &quot;id&quot; : 1
  }, {
    &quot;name&quot; : &quot;name&quot;,
    &quot;id&quot; : 1
  } ],
  &quot;status&quot; : &quot;available&quot;
}"
  UpdatePetExamples <- UpdatePetExamples.Add("application/json", UpdatePetBody)

  UpdatePetBody <- WebUtility.HtmlDecode "&lt;Pet&gt;
  &lt;id&gt;123456789&lt;/id&gt;
  &lt;name&gt;doggie&lt;/name&gt;
  &lt;photoUrls&gt;
    &lt;photoUrls&gt;aeiou&lt;/photoUrls&gt;
  &lt;/photoUrls&gt;
  &lt;tags&gt;
  &lt;/tags&gt;
  &lt;status&gt;aeiou&lt;/status&gt;
&lt;/Pet&gt;"
  UpdatePetExamples <- UpdatePetExamples.Add("application/xml", UpdatePetBody)

  let getUpdatePetExample mediaType =
    UpdatePetExamples.[mediaType]
      |> getConverter mediaType

  let mutable UpdatePetWithFormExamples = Map.empty
  let mutable UpdatePetWithFormBody = ""

  let getUpdatePetWithFormExample mediaType =
    UpdatePetWithFormExamples.[mediaType]
      |> getConverter mediaType

  let mutable UploadFileExamples = Map.empty
  let mutable UploadFileBody = ""

  let getUploadFileExample mediaType =
    UploadFileExamples.[mediaType]
      |> getConverter mediaType
