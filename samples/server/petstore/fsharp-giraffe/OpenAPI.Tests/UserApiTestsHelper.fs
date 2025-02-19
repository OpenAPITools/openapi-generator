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
open OpenAPI.UserApiHandler
open OpenAPI.UserApiHandlerParams

module UserApiHandlerTestsHelper =


  let mutable CreateUserExamples = Map.empty
  let mutable CreateUserBody = ""

  CreateUserBody <- WebUtility.HtmlDecode "{
  &quot;firstName&quot; : &quot;firstName&quot;,
  &quot;lastName&quot; : &quot;lastName&quot;,
  &quot;password&quot; : &quot;password&quot;,
  &quot;userStatus&quot; : 6,
  &quot;phone&quot; : &quot;phone&quot;,
  &quot;id&quot; : 0,
  &quot;email&quot; : &quot;email&quot;,
  &quot;username&quot; : &quot;username&quot;
}"
  CreateUserExamples <- CreateUserExamples.Add("application/json", CreateUserBody)

  let getCreateUserExample mediaType =
    CreateUserExamples.[mediaType]
      |> getConverter mediaType

  let mutable CreateUsersWithArrayInputExamples = Map.empty
  let mutable CreateUsersWithArrayInputBody = ""

  CreateUsersWithArrayInputBody <- WebUtility.HtmlDecode "{
  &quot;firstName&quot; : &quot;firstName&quot;,
  &quot;lastName&quot; : &quot;lastName&quot;,
  &quot;password&quot; : &quot;password&quot;,
  &quot;userStatus&quot; : 6,
  &quot;phone&quot; : &quot;phone&quot;,
  &quot;id&quot; : 0,
  &quot;email&quot; : &quot;email&quot;,
  &quot;username&quot; : &quot;username&quot;
}"
  CreateUsersWithArrayInputExamples <- CreateUsersWithArrayInputExamples.Add("application/json", CreateUsersWithArrayInputBody)

  let getCreateUsersWithArrayInputExample mediaType =
    CreateUsersWithArrayInputExamples.[mediaType]
      |> getConverter mediaType

  let mutable CreateUsersWithListInputExamples = Map.empty
  let mutable CreateUsersWithListInputBody = ""

  CreateUsersWithListInputBody <- WebUtility.HtmlDecode "{
  &quot;firstName&quot; : &quot;firstName&quot;,
  &quot;lastName&quot; : &quot;lastName&quot;,
  &quot;password&quot; : &quot;password&quot;,
  &quot;userStatus&quot; : 6,
  &quot;phone&quot; : &quot;phone&quot;,
  &quot;id&quot; : 0,
  &quot;email&quot; : &quot;email&quot;,
  &quot;username&quot; : &quot;username&quot;
}"
  CreateUsersWithListInputExamples <- CreateUsersWithListInputExamples.Add("application/json", CreateUsersWithListInputBody)

  let getCreateUsersWithListInputExample mediaType =
    CreateUsersWithListInputExamples.[mediaType]
      |> getConverter mediaType
  ()

  ()

  ()

  ()


  let mutable UpdateUserExamples = Map.empty
  let mutable UpdateUserBody = ""

  UpdateUserBody <- WebUtility.HtmlDecode "{
  &quot;firstName&quot; : &quot;firstName&quot;,
  &quot;lastName&quot; : &quot;lastName&quot;,
  &quot;password&quot; : &quot;password&quot;,
  &quot;userStatus&quot; : 6,
  &quot;phone&quot; : &quot;phone&quot;,
  &quot;id&quot; : 0,
  &quot;email&quot; : &quot;email&quot;,
  &quot;username&quot; : &quot;username&quot;
}"
  UpdateUserExamples <- UpdateUserExamples.Add("application/json", UpdateUserBody)

  let getUpdateUserExample mediaType =
    UpdateUserExamples.[mediaType]
      |> getConverter mediaType
