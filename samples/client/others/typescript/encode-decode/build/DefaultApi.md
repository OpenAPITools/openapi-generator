# .DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testDecodeArrayOfArraysGet**](DefaultApi.md#testDecodeArrayOfArraysGet) | **GET** /test/decode/array-of-arrays | 
[**testDecodeArrayOfGet**](DefaultApi.md#testDecodeArrayOfGet) | **GET** /test/decode/array-of | 
[**testDecodeArrayOfMapsOfObjectsGet**](DefaultApi.md#testDecodeArrayOfMapsOfObjectsGet) | **GET** /test/decode/array-of/maps-of/objects | 
[**testDecodeArrayOfNullableGet**](DefaultApi.md#testDecodeArrayOfNullableGet) | **GET** /test/decode/array-of/nullable | 
[**testDecodeArrayOfNullableObjectsGet**](DefaultApi.md#testDecodeArrayOfNullableObjectsGet) | **GET** /test/decode/array-of/nullable-objects | 
[**testDecodeCompositeObjectsGet**](DefaultApi.md#testDecodeCompositeObjectsGet) | **GET** /test/decode/composite-objects | 
[**testDecodeMapOfMapsOfObjectsGet**](DefaultApi.md#testDecodeMapOfMapsOfObjectsGet) | **GET** /test/decode/map-of/maps-of/objects | 
[**testDecodeMapOfObjectsGet**](DefaultApi.md#testDecodeMapOfObjectsGet) | **GET** /test/decode/map-of/objects | 
[**testDecodeMapOfPrimitiveGet**](DefaultApi.md#testDecodeMapOfPrimitiveGet) | **GET** /test/decode/map-of/primitive | 
[**testDecodeNullableArrayGet**](DefaultApi.md#testDecodeNullableArrayGet) | **GET** /test/decode/nullable-array | 
[**testDecodeNullableGet**](DefaultApi.md#testDecodeNullableGet) | **GET** /test/decode/nullable | 
[**testDecodeObjectGet**](DefaultApi.md#testDecodeObjectGet) | **GET** /test/decode/object | 
[**testDecodePrimitiveBooleanGet**](DefaultApi.md#testDecodePrimitiveBooleanGet) | **GET** /test/decode/primitive/boolean | 
[**testDecodePrimitiveIntegerGet**](DefaultApi.md#testDecodePrimitiveIntegerGet) | **GET** /test/decode/primitive/integer | 
[**testDecodePrimitiveNumberGet**](DefaultApi.md#testDecodePrimitiveNumberGet) | **GET** /test/decode/primitive/number | 
[**testDecodePrimitiveStringGet**](DefaultApi.md#testDecodePrimitiveStringGet) | **GET** /test/decode/primitive/string | 
[**testEncodeArrayOfArraysPost**](DefaultApi.md#testEncodeArrayOfArraysPost) | **POST** /test/encode/array-of-arrays | 
[**testEncodeArrayOfMapsOfObjectsPost**](DefaultApi.md#testEncodeArrayOfMapsOfObjectsPost) | **POST** /test/encode/array-of/maps-of/objects | 
[**testEncodeArrayOfNullableObjectsPost**](DefaultApi.md#testEncodeArrayOfNullableObjectsPost) | **POST** /test/encode/array-of/nullable-objects | 
[**testEncodeArrayOfNullablePost**](DefaultApi.md#testEncodeArrayOfNullablePost) | **POST** /test/encode/array-of/nullable | 
[**testEncodeArrayOfPost**](DefaultApi.md#testEncodeArrayOfPost) | **POST** /test/encode/array-of | 
[**testEncodeCompositeObjectsPost**](DefaultApi.md#testEncodeCompositeObjectsPost) | **POST** /test/encode/composite-objects | 
[**testEncodeMapOfMapsOfObjectsPost**](DefaultApi.md#testEncodeMapOfMapsOfObjectsPost) | **POST** /test/encode/map-of/maps-of/objects | 
[**testEncodeMapOfObjectsPost**](DefaultApi.md#testEncodeMapOfObjectsPost) | **POST** /test/encode/map-of/objects | 
[**testEncodeMapOfPrimitivePost**](DefaultApi.md#testEncodeMapOfPrimitivePost) | **POST** /test/encode/map-of/primitive | 
[**testEncodeNullableArrayPost**](DefaultApi.md#testEncodeNullableArrayPost) | **POST** /test/encode/nullable-array | 
[**testEncodeNullablePost**](DefaultApi.md#testEncodeNullablePost) | **POST** /test/encode/nullable | 
[**testEncodeObjectPost**](DefaultApi.md#testEncodeObjectPost) | **POST** /test/encode/object | 
[**testEncodePrimitiveBooleanPost**](DefaultApi.md#testEncodePrimitiveBooleanPost) | **POST** /test/encode/primitive/boolean | 
[**testEncodePrimitiveIntegerPost**](DefaultApi.md#testEncodePrimitiveIntegerPost) | **POST** /test/encode/primitive/integer | 
[**testEncodePrimitiveNumberPost**](DefaultApi.md#testEncodePrimitiveNumberPost) | **POST** /test/encode/primitive/number | 
[**testEncodePrimitiveStringPost**](DefaultApi.md#testEncodePrimitiveStringPost) | **POST** /test/encode/primitive/string | 


# **testDecodeArrayOfArraysGet**
> Array<Array<string>> testDecodeArrayOfArraysGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeArrayOfArraysGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**Array<Array<string>>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an array of arrays |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeArrayOfGet**
> Array<string> testDecodeArrayOfGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeArrayOfGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**Array<string>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an array of primitive types |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeArrayOfMapsOfObjectsGet**
> Array<{ [key: string]: ComplexObject; }> testDecodeArrayOfMapsOfObjectsGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeArrayOfMapsOfObjectsGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**Array<{ [key: string]: ComplexObject; }>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an array of maps of complex objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeArrayOfNullableGet**
> Array<string | null> testDecodeArrayOfNullableGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeArrayOfNullableGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**Array<string | null>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an array of nullable types |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeArrayOfNullableObjectsGet**
> Array<ComplexObject> testDecodeArrayOfNullableObjectsGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeArrayOfNullableObjectsGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**Array<ComplexObject>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an array of nullable objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeCompositeObjectsGet**
> CompositeObject testDecodeCompositeObjectsGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeCompositeObjectsGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**CompositeObject**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a composite object |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeMapOfMapsOfObjectsGet**
> { [key: string]: { [key: string]: ComplexObject; }; } testDecodeMapOfMapsOfObjectsGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeMapOfMapsOfObjectsGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**{ [key: string]: { [key: string]: ComplexObject; }; }**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a map of maps of objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeMapOfObjectsGet**
> { [key: string]: ComplexObject | null; } testDecodeMapOfObjectsGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeMapOfObjectsGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**{ [key: string]: ComplexObject | null; }**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a map of objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeMapOfPrimitiveGet**
> { [key: string]: string; } testDecodeMapOfPrimitiveGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeMapOfPrimitiveGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**{ [key: string]: string; }**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a map of primitive types |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeNullableArrayGet**
> Array<string> testDecodeNullableArrayGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeNullableArrayGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**Array<string>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a nullable array |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeNullableGet**
> string testDecodeNullableGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeNullableGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a nullable type |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodeObjectGet**
> ComplexObject testDecodeObjectGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodeObjectGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**ComplexObject**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an object |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodePrimitiveBooleanGet**
> boolean testDecodePrimitiveBooleanGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodePrimitiveBooleanGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a boolean |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodePrimitiveIntegerGet**
> number testDecodePrimitiveIntegerGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodePrimitiveIntegerGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**number**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes an integer |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodePrimitiveNumberGet**
> number testDecodePrimitiveNumberGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodePrimitiveNumberGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**number**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a number |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testDecodePrimitiveStringGet**
> string testDecodePrimitiveStringGet()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:any = {};

apiInstance.testDecodePrimitiveStringGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters
This endpoint does not need any parameter.


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Decodes a string |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeArrayOfArraysPost**
> void testEncodeArrayOfArraysPost(requestBody)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeArrayOfArraysPostRequest = {
  // Array<Array<string>>
  requestBody: [
    [
      "string_example",
    ],
  ],
};

apiInstance.testEncodeArrayOfArraysPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **Array<Array<string>>**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an array of arrays |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeArrayOfMapsOfObjectsPost**
> void testEncodeArrayOfMapsOfObjectsPost(complexObject)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeArrayOfMapsOfObjectsPostRequest = {
  // Array<{ [key: string]: ComplexObject; }>
  complexObject: [
    {
      "key": {
        requiredProperty: "requiredProperty_example",
        requiredNullableProperty: "requiredNullableProperty_example",
        optionalProperty: "optionalProperty_example",
        optionalNullableProperty: "optionalNullableProperty_example",
      },
    },
  ],
};

apiInstance.testEncodeArrayOfMapsOfObjectsPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **complexObject** | **Array<{ [key: string]: ComplexObject; }>**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an array of maps of complex objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeArrayOfNullableObjectsPost**
> void testEncodeArrayOfNullableObjectsPost(complexObject)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeArrayOfNullableObjectsPostRequest = {
  // Array<ComplexObject>
  complexObject: [
    {
      requiredProperty: "requiredProperty_example",
      requiredNullableProperty: "requiredNullableProperty_example",
      optionalProperty: "optionalProperty_example",
      optionalNullableProperty: "optionalNullableProperty_example",
    },
  ],
};

apiInstance.testEncodeArrayOfNullableObjectsPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **complexObject** | **Array<ComplexObject>**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an array of nullable objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeArrayOfNullablePost**
> void testEncodeArrayOfNullablePost(requestBody)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeArrayOfNullablePostRequest = {
  // Array<string | null>
  requestBody: [
    "requestBody_example",
  ],
};

apiInstance.testEncodeArrayOfNullablePost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **Array<string | null>**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an array of nullable types |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeArrayOfPost**
> void testEncodeArrayOfPost(requestBody)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeArrayOfPostRequest = {
  // Array<string>
  requestBody: [
    "requestBody_example",
  ],
};

apiInstance.testEncodeArrayOfPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **Array<string>**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an array of primitive types |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeCompositeObjectsPost**
> void testEncodeCompositeObjectsPost(compositeObject)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeCompositeObjectsPostRequest = {
  // CompositeObject
  compositeObject: {
    optionalNullableInnerObject: {
      requiredProperty: "requiredProperty_example",
      requiredNullableProperty: "requiredNullableProperty_example",
      optionalProperty: "optionalProperty_example",
      optionalNullableProperty: "optionalNullableProperty_example",
    },
  },
};

apiInstance.testEncodeCompositeObjectsPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **compositeObject** | **CompositeObject**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a composite object |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeMapOfMapsOfObjectsPost**
> void testEncodeMapOfMapsOfObjectsPost(requestBody)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeMapOfMapsOfObjectsPostRequest = {
  // { [key: string]: { [key: string]: ComplexObject; }; }
  requestBody: {
    "key": 
      key: {
        requiredProperty: "requiredProperty_example",
        requiredNullableProperty: "requiredNullableProperty_example",
        optionalProperty: "optionalProperty_example",
        optionalNullableProperty: "optionalNullableProperty_example",
      },
    ,
  },
};

apiInstance.testEncodeMapOfMapsOfObjectsPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **{ [key: string]: { [key: string]: ComplexObject; }; }**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a map of maps of objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeMapOfObjectsPost**
> void testEncodeMapOfObjectsPost(requestBody)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeMapOfObjectsPostRequest = {
  // { [key: string]: ComplexObject | null; }
  requestBody: {
    "key": {
      requiredProperty: "requiredProperty_example",
      requiredNullableProperty: "requiredNullableProperty_example",
      optionalProperty: "optionalProperty_example",
      optionalNullableProperty: "optionalNullableProperty_example",
    },
  },
};

apiInstance.testEncodeMapOfObjectsPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **{ [key: string]: ComplexObject | null; }**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a map of objects |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeMapOfPrimitivePost**
> void testEncodeMapOfPrimitivePost(requestBody)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeMapOfPrimitivePostRequest = {
  // { [key: string]: string; }
  requestBody: {
    "key": "key_example",
  },
};

apiInstance.testEncodeMapOfPrimitivePost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **{ [key: string]: string; }**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a map of primitive types |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeNullableArrayPost**
> void testEncodeNullableArrayPost()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeNullableArrayPostRequest = {
  // Array<string> (optional)
  requestBody: [
    "requestBody_example",
  ],
};

apiInstance.testEncodeNullableArrayPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **Array<string>**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a nullable array |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeNullablePost**
> void testEncodeNullablePost()


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeNullablePostRequest = {
  // string (optional)
  body: "body_example",
};

apiInstance.testEncodeNullablePost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a nullable type |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodeObjectPost**
> void testEncodeObjectPost(complexObject)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodeObjectPostRequest = {
  // ComplexObject
  complexObject: {
    requiredProperty: "requiredProperty_example",
    requiredNullableProperty: "requiredNullableProperty_example",
    optionalProperty: "optionalProperty_example",
    optionalNullableProperty: "optionalNullableProperty_example",
  },
};

apiInstance.testEncodeObjectPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **complexObject** | **ComplexObject**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an object |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodePrimitiveBooleanPost**
> void testEncodePrimitiveBooleanPost(body)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodePrimitiveBooleanPostRequest = {
  // boolean
  body: true,
};

apiInstance.testEncodePrimitiveBooleanPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **boolean**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a boolean |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodePrimitiveIntegerPost**
> void testEncodePrimitiveIntegerPost(body)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodePrimitiveIntegerPostRequest = {
  // number
  body: 1,
};

apiInstance.testEncodePrimitiveIntegerPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **number**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes an integer |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodePrimitiveNumberPost**
> void testEncodePrimitiveNumberPost(body)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodePrimitiveNumberPostRequest = {
  // number
  body: 3.14,
};

apiInstance.testEncodePrimitiveNumberPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **number**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a number |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEncodePrimitiveStringPost**
> void testEncodePrimitiveStringPost(body)


### Example


```typescript
import {  } from '';
import * as fs from 'fs';

const configuration = .createConfiguration();
const apiInstance = new .DefaultApi(configuration);

let body:.DefaultApiTestEncodePrimitiveStringPostRequest = {
  // string
  body: "body_example",
};

apiInstance.testEncodePrimitiveStringPost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Encodes a string |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


