# \DefaultApi

All URIs are relative to *http://localhost:8080*

Method | HTTP request | Description
------------- | ------------- | -------------
[**upload_multiple_fields**](DefaultApi.md#upload_multiple_fields) | **POST** /upload/multiple-fields | Upload with multiple form fields
[**upload_optional_file**](DefaultApi.md#upload_optional_file) | **POST** /upload/optional | Upload an optional file
[**upload_single_file**](DefaultApi.md#upload_single_file) | **POST** /upload/single | Upload a single file (required parameter)



## upload_multiple_fields

> models::UploadResponse upload_multiple_fields(primary_file, title, tags, thumbnail)
Upload with multiple form fields

Tests async multipart with multiple files and text fields

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**primary_file** | **std::path::PathBuf** | Primary file (required) | [required] |
**title** | Option<**String**> | Upload title |  |
**tags** | Option<[**Vec<String>**](String.md)> | Tags for the upload |  |
**thumbnail** | Option<**std::path::PathBuf**> | Optional thumbnail file |  |

### Return type

[**models::UploadResponse**](UploadResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## upload_optional_file

> models::UploadResponse upload_optional_file(metadata, file)
Upload an optional file

Tests async multipart file upload with optional file parameter

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**metadata** | Option<**String**> | Optional metadata string |  |
**file** | Option<**std::path::PathBuf**> | Optional file to upload |  |

### Return type

[**models::UploadResponse**](UploadResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## upload_single_file

> models::UploadResponse upload_single_file(description, file)
Upload a single file (required parameter)

Tests async multipart file upload with required file parameter

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**description** | **String** | File description metadata | [required] |
**file** | **std::path::PathBuf** | File to upload | [required] |

### Return type

[**models::UploadResponse**](UploadResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

