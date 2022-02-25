namespace OpenAPI.Model

open System
open System.Collections.Generic
open Newtonsoft.Json

module User = 

  //#region User

  [<CLIMutable>]
  type User = {
    [<JsonProperty(PropertyName = "id")>]
    Id : int64;
    [<JsonProperty(PropertyName = "username")>]
    Username : string;
    [<JsonProperty(PropertyName = "firstName")>]
    FirstName : string;
    [<JsonProperty(PropertyName = "lastName")>]
    LastName : string;
    [<JsonProperty(PropertyName = "email")>]
    Email : string;
    [<JsonProperty(PropertyName = "password")>]
    Password : string;
    [<JsonProperty(PropertyName = "phone")>]
    Phone : string;
    [<JsonProperty(PropertyName = "userStatus")>]
    UserStatus : int;
  }
  
  //#endregion
  