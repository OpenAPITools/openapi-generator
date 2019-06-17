namespace OpenAPI.Model

open System
open System.Collections.Generic

module ApiResponse = 

  //#region ApiResponse


  type ApiResponse = {
    Code : int;
    Type : string;
    Message : string;
  }
  //#endregion
  