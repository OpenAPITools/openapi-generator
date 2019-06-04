namespace OpenAPI.Model

open System
open System.Collections.Generic

module Order = 

  //#region Order

  //#region enums
  type StatusEnum = PlacedEnum of string  |  ApprovedEnum of string  |  DeliveredEnum of string  
  //#endregion

  type Order = {
    Id : int64;
    PetId : int64;
    Quantity : int;
    ShipDate : Nullable<DateTime>;
    Status : StatusEnum;
    Complete : bool;
  }
  //#endregion
  