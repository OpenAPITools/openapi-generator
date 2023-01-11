# .ResponseMeta

## Properties

|Name | Type | Description | Notes|
|------------ | ------------- | ------------- | -------------|
|**code** | **string** | Code returned by the function | [default to ResponseMetaCodeEnum.Ok]|
|**detail** | **string** | Explanation of what went wrong | [optional] [default to &quot;&quot;]|
|**exception** | **string** | Message of the exception that will help developer to debug this problem if needed | [optional] [default to &quot;&quot;]|
|**type** | **string** | Type of error | [optional] [default to &quot;&quot;]|
|**errorCode** | [**ErrorCode**](ErrorCode.md) |  | [optional] [default to ErrorCode.VolumeRangeAtLowestValue]|
|**errors** | **Array&lt;Error&gt;** | An array of all the specific error encountered during the request | [optional] [default to List&lt;ErrorRecord&gt;()]|


## Enum: ResponseMetaCodeEnum


* `Ok` (value: `'Ok'`)

* `GenericException` (value: `'Generic_Exception'`)

* `FieldErrorException` (value: `'Field_Error_Exception'`)

* `ImageValidationException` (value: `'Image_Validation_Exception'`)

* `InvalidContainerCreationWithNoDefaultAssetException` (value: `'Invalid_Container_Creation_With_No_Default_Asset_Exception'`)

* `InvalidOverrideModeException` (value: `'Invalid_Override_Mode_Exception'`)

* `InvalidTagException` (value: `'Invalid_Tag_Exception'`)

* `ItemUseException` (value: `'Item_Use_Exception'`)

* `MissingPlatformForSoftwareException` (value: `'Missing_Platform_For_Software_Exception'`)

* `MissingSoftwareForPlatformException` (value: `'Missing_Software_For_Platform_Exception'`)

* `PlatformNotSupportedException` (value: `'Platform_Not_Supported_Exception'`)

* `RefreshDataException` (value: `'Refresh_Data_Exception'`)

* `RoleAssignmentException` (value: `'Role_Assignment_Exception'`)

* `TaskAlreadyRunningException` (value: `'Task_Already_Running_Exception'`)

* `LoggedOutException` (value: `'Logged_Out_Exception'`)

* `AuthorizationException` (value: `'Authorization_Exception'`)

* `UnauthorizedActionForCurrentUserException` (value: `'Unauthorized_Action_For_Current_User_Exception'`)

* `UserAlreadyExistsButIsNotAuthenticatedException` (value: `'User_Already_Exists_But_Is_Not_Authenticated_Exception'`)

* `UserAlreadyHasActiveOrClosedGalaxieApiProductException` (value: `'User_Already_Has_Active_Or_Closed_Galaxie_Api_Product_Exception'`)

* `UserAlreadyHasMultipleGalaxieApiProductsException` (value: `'User_Already_Has_Multiple_Galaxie_Api_Products_Exception'`)

* `RecurlyApiException` (value: `'Recurly_Api_Exception'`)

* `RecurlyTransactionErrorException` (value: `'Recurly_Transaction_Error_Exception'`)

* `GalaxieApiException` (value: `'Galaxie_Api_Exception'`)




