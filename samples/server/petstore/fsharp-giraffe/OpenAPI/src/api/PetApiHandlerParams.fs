namespace OpenAPI

open OpenAPI.Model.ApiResponse
open OpenAPI.Model.Pet
open System.Collections.Generic
open System

module PetApiHandlerParams = 


    //#region Body parameters
    [<CLIMutable>]
    type AddPetBodyParams = Pet 
    //#endregion

    
    type AddPetStatusCode405Response = {
      content:string;
      
    }
    type AddPetResult = AddPetStatusCode405 of AddPetStatusCode405Response

    type AddPetArgs = {
      bodyParams:AddPetBodyParams
    }
    //#region Path parameters
    [<CLIMutable>]
    type DeletePetPathParams = {
      petId : int64 ;
    }
    //#endregion

    //#region Header parameters
    [<CLIMutable>]
    type DeletePetHeaderParams = {
      apiKey : string option;
    }
    //#endregion

    
    type DeletePetStatusCode400Response = {
      content:string;
      
    }
    type DeletePetResult = DeletePetStatusCode400 of DeletePetStatusCode400Response

    type DeletePetArgs = {
      headerParams:DeletePetHeaderParams;
      pathParams:DeletePetPathParams;
    }

    //#region Query parameters
    [<CLIMutable>]
    type FindPetsByStatusQueryParams = {
      status : string[] ;
      
    }
    //#endregion

    
    type FindPetsByStatusDefaultStatusCodeResponse = {
      content:Pet[];
      
    }
    
    type FindPetsByStatusStatusCode400Response = {
      content:string;
      
    }
    type FindPetsByStatusResult = FindPetsByStatusDefaultStatusCode of FindPetsByStatusDefaultStatusCodeResponse|FindPetsByStatusStatusCode400 of FindPetsByStatusStatusCode400Response

    type FindPetsByStatusArgs = {
      queryParams:Result<FindPetsByStatusQueryParams,string>;
    }

    //#region Query parameters
    [<CLIMutable>]
    type FindPetsByTagsQueryParams = {
      tags : string[] ;
      

      maxCount : int option;
      
    }
    //#endregion

    
    type FindPetsByTagsDefaultStatusCodeResponse = {
      content:Pet[];
      
    }
    
    type FindPetsByTagsStatusCode400Response = {
      content:string;
      
    }
    type FindPetsByTagsResult = FindPetsByTagsDefaultStatusCode of FindPetsByTagsDefaultStatusCodeResponse|FindPetsByTagsStatusCode400 of FindPetsByTagsStatusCode400Response

    type FindPetsByTagsArgs = {
      queryParams:Result<FindPetsByTagsQueryParams,string>;
    }
    //#region Path parameters
    [<CLIMutable>]
    type GetPetByIdPathParams = {
      petId : int64 ;
    }
    //#endregion

    
    type GetPetByIdDefaultStatusCodeResponse = {
      content:Pet;
      
    }
    
    type GetPetByIdStatusCode400Response = {
      content:string;
      
    }
    
    type GetPetByIdStatusCode404Response = {
      content:string;
      
    }
    type GetPetByIdResult = GetPetByIdDefaultStatusCode of GetPetByIdDefaultStatusCodeResponse|GetPetByIdStatusCode400 of GetPetByIdStatusCode400Response|GetPetByIdStatusCode404 of GetPetByIdStatusCode404Response

    type GetPetByIdArgs = {
      pathParams:GetPetByIdPathParams;
    }

    //#region Body parameters
    [<CLIMutable>]
    type UpdatePetBodyParams = Pet 
    //#endregion

    
    type UpdatePetStatusCode400Response = {
      content:string;
      
    }
    
    type UpdatePetStatusCode404Response = {
      content:string;
      
    }
    
    type UpdatePetStatusCode405Response = {
      content:string;
      
    }
    type UpdatePetResult = UpdatePetStatusCode400 of UpdatePetStatusCode400Response|UpdatePetStatusCode404 of UpdatePetStatusCode404Response|UpdatePetStatusCode405 of UpdatePetStatusCode405Response

    type UpdatePetArgs = {
      bodyParams:UpdatePetBodyParams
    }
    //#region Path parameters
    [<CLIMutable>]
    type UpdatePetWithFormPathParams = {
      petId : int64 ;
    }
    //#endregion

    //#region Form parameters
    [<CLIMutable>]
    type UpdatePetWithFormFormParams = {
      name : string option;
    //#endregion

    //#region Form parameters
      status : string option;
    }
    //#endregion

    
    type UpdatePetWithFormStatusCode405Response = {
      content:string;
      
    }
    type UpdatePetWithFormResult = UpdatePetWithFormStatusCode405 of UpdatePetWithFormStatusCode405Response

    type UpdatePetWithFormArgs = {
      pathParams:UpdatePetWithFormPathParams;
      formParams:Result<UpdatePetWithFormFormParams,string>
    }
    //#region Path parameters
    [<CLIMutable>]
    type UploadFilePathParams = {
      petId : int64 ;
    }
    //#endregion

    //#region Form parameters
    [<CLIMutable>]
    type UploadFileFormParams = {
      additionalMetadata : string option;
    //#endregion

    //#region Form parameters
      file : System.IO.Stream option;
    }
    //#endregion

    
    type UploadFileDefaultStatusCodeResponse = {
      content:ApiResponse;
      
    }
    type UploadFileResult = UploadFileDefaultStatusCode of UploadFileDefaultStatusCodeResponse

    type UploadFileArgs = {
      pathParams:UploadFilePathParams;
      formParams:Result<UploadFileFormParams,string>
    }
    