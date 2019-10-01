namespace OpenAPI.Model

open System
open System.Collections.Generic
open Newtonsoft.Json

module Category = 

  //#region Category

  [<CLIMutable>]
  type Category = {
    [<JsonProperty(PropertyName = "id")>]
    Id : int64;
    [<JsonProperty(PropertyName = "name")>]
    Name : string;
  }
  
  //#endregion
  