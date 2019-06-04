namespace OpenAPI.Model

open System
open System.Collections.Generic
open OpenAPI.Model.Category
open OpenAPI.Model.Tag

module Pet = 

  //#region Pet

  //#region enums
  type StatusEnum = AvailableEnum of string  |  PendingEnum of string  |  SoldEnum of string  
  //#endregion

  type Pet = {
    Id : int64;
    Category : Category;
    Name : string;
    PhotoUrls : string[];
    Tags : Tag[];
    Status : StatusEnum;
  }
  //#endregion
  