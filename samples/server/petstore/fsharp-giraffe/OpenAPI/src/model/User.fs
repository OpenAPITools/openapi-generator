namespace OpenAPI.Model

open System
open System.Collections.Generic

module User = 

  //#region User


  type User = {
    Id : int64;
    Username : string;
    FirstName : string;
    LastName : string;
    Email : string;
    Password : string;
    Phone : string;
    UserStatus : int;
  }
  //#endregion
  