namespace OpenAPI.Model

open System
open System.Collections.Generic
open Newtonsoft.Json
open OpenAPI.Model.Category
open OpenAPI.Model.Tag

module Pet = 

  //#region Pet

  [<CLIMutable>]
  type Pet = {
    [<JsonProperty(PropertyName = "id")>]
    Id : int64;
    [<JsonProperty(PropertyName = "category")>]
    Category : Category;
    [<JsonProperty(PropertyName = "name")>]
    Name : string;
    [<JsonProperty(PropertyName = "photoUrls")>]
    PhotoUrls : string[];
    [<JsonProperty(PropertyName = "tags")>]
    Tags : Tag[];
    [<JsonProperty(PropertyName = "status")>]
    Status : string;
  }
  
  //#endregion
  