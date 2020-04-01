namespace OpenAPI.Model

open System
open System.Collections.Generic
open Newtonsoft.Json

module ApiResponse = 

  //#region ApiResponse

  [<CLIMutable>]
  type ApiResponse = {
    [<JsonProperty(PropertyName = "code")>]
    Code : int;
    [<JsonProperty(PropertyName = "type")>]
    Type : string;
    [<JsonProperty(PropertyName = "message")>]
    Message : string;
  }
  
  //#endregion
  