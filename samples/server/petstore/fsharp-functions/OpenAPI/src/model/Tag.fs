namespace OpenAPI.Model

open System
open System.Collections.Generic
open Newtonsoft.Json

module Tag = 

  //#region Tag

  [<CLIMutable>]
  type Tag = {
    [<JsonProperty(PropertyName = "id")>]
    Id : int64;
    [<JsonProperty(PropertyName = "name")>]
    Name : string;
  }
  
  //#endregion
  