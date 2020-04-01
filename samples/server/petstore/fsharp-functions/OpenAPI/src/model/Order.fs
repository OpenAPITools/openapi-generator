namespace OpenAPI.Model

open System
open System.Collections.Generic
open Newtonsoft.Json

module Order = 

  //#region Order

  [<CLIMutable>]
  type Order = {
    [<JsonProperty(PropertyName = "id")>]
    Id : int64;
    [<JsonProperty(PropertyName = "petId")>]
    PetId : int64;
    [<JsonProperty(PropertyName = "quantity")>]
    Quantity : int;
    [<JsonProperty(PropertyName = "shipDate")>]
    ShipDate : Nullable<DateTime>;
    [<JsonProperty(PropertyName = "status")>]
    Status : string;
    [<JsonProperty(PropertyName = "complete")>]
    Complete : bool;
  }
  
  //#endregion
  