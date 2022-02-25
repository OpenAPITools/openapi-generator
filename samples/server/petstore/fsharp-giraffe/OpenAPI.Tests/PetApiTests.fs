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
open Newtonsoft
open TestHelper
open PetApiHandlerTestsHelper
open OpenAPI.PetApiHandler
open OpenAPI.PetApiHandlerParams
open OpenAPI.Model.ApiResponse
open OpenAPI.Model.Pet

module PetApiHandlerTests =

  // ---------------------------------
  // Tests
  // ---------------------------------

  [<Fact>]
  let ``AddPet - Add a new pet to the store returns 405 where Invalid input`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet"

      // use an example requestBody provided by the spec
      let examples = Map.empty.Add("application/json", getAddPetExample "application/json").Add("application/xml", getAddPetExample "application/xml")
      // or pass a body of type Pet
      let body = obj() :?> Pet |> Newtonsoft.Json.JsonConvert.SerializeObject |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent

      body
        |> HttpPost client path
        |> isStatus (enum<HttpStatusCode>(405))
        |> readText
        |> shouldEqual "TESTME"
      }

  [<Fact>]
  let ``DeletePet - Deletes a pet returns 400 where Invalid pet value`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/{petId}".Replace("petId", "ADDME")

      HttpDelete client path
        |> isStatus (enum<HttpStatusCode>(400))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``FindPetsByStatus - Finds Pets by status returns 200 where successful operation`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/findByStatus" + "?status=ADDME"

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(200))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``FindPetsByStatus - Finds Pets by status returns 400 where Invalid status value`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/findByStatus" + "?status=ADDME"

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(400))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``FindPetsByTags - Finds Pets by tags returns 200 where successful operation`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/findByTags" + "?tags=ADDME"

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(200))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``FindPetsByTags - Finds Pets by tags returns 400 where Invalid tag value`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/findByTags" + "?tags=ADDME"

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(400))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``GetPetById - Find pet by ID returns 200 where successful operation`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/{petId}".Replace("petId", "ADDME")

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(200))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``GetPetById - Find pet by ID returns 400 where Invalid ID supplied`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/{petId}".Replace("petId", "ADDME")

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(400))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``GetPetById - Find pet by ID returns 404 where Pet not found`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/{petId}".Replace("petId", "ADDME")

      HttpGet client path
        |> isStatus (enum<HttpStatusCode>(404))
        |> readText
        |> shouldEqual "TESTME"
        |> ignore
      }

  [<Fact>]
  let ``UpdatePet - Update an existing pet returns 400 where Invalid ID supplied`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet"

      // use an example requestBody provided by the spec
      let examples = Map.empty.Add("application/json", getUpdatePetExample "application/json").Add("application/xml", getUpdatePetExample "application/xml")
      // or pass a body of type Pet
      let body = obj() :?> Pet |> Newtonsoft.Json.JsonConvert.SerializeObject |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent

      body
        |> HttpPut client path
        |> isStatus (enum<HttpStatusCode>(400))
        |> readText
        |> shouldEqual "TESTME"
      }

  [<Fact>]
  let ``UpdatePet - Update an existing pet returns 404 where Pet not found`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet"

      // use an example requestBody provided by the spec
      let examples = Map.empty.Add("application/json", getUpdatePetExample "application/json").Add("application/xml", getUpdatePetExample "application/xml")
      // or pass a body of type Pet
      let body = obj() :?> Pet |> Newtonsoft.Json.JsonConvert.SerializeObject |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent

      body
        |> HttpPut client path
        |> isStatus (enum<HttpStatusCode>(404))
        |> readText
        |> shouldEqual "TESTME"
      }

  [<Fact>]
  let ``UpdatePet - Update an existing pet returns 405 where Validation exception`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet"

      // use an example requestBody provided by the spec
      let examples = Map.empty.Add("application/json", getUpdatePetExample "application/json").Add("application/xml", getUpdatePetExample "application/xml")
      // or pass a body of type Pet
      let body = obj() :?> Pet |> Newtonsoft.Json.JsonConvert.SerializeObject |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent

      body
        |> HttpPut client path
        |> isStatus (enum<HttpStatusCode>(405))
        |> readText
        |> shouldEqual "TESTME"
      }

  [<Fact>]
  let ``UpdatePetWithForm - Updates a pet in the store with form data returns 405 where Invalid input`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/{petId}".Replace("petId", "ADDME")

      // use an example requestBody provided by the spec
      let examples = Map.empty.Add("application/x-www-form-urlencoded", getUpdatePetWithFormExample "application/x-www-form-urlencoded")
      // or pass a formform
      let body = obj()  |> Newtonsoft.Json.JsonConvert.SerializeObject |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent

      body
        |> HttpPost client path
        |> isStatus (enum<HttpStatusCode>(405))
        |> readText
        |> shouldEqual "TESTME"
      }

  [<Fact>]
  let ``UploadFile - uploads an image returns 200 where successful operation`` () =
    task {
      use server = new TestServer(createHost())
      use client = server.CreateClient()

      // add your setup code here

      let path = "/v2/pet/{petId}/uploadImage".Replace("petId", "ADDME")

      // use an example requestBody provided by the spec
      let examples = Map.empty.Add("multipart/form-data", getUploadFileExample "multipart/form-data")
      // or pass a formform
      let body = obj()  |> Newtonsoft.Json.JsonConvert.SerializeObject |> Encoding.UTF8.GetBytes |> MemoryStream |> StreamContent

      body
        |> HttpPost client path
        |> isStatus (enum<HttpStatusCode>(200))
        |> readText
        |> shouldEqual "TESTME"
      }

