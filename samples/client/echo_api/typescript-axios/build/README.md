## @openapitools/typescript-axios-echo-api@1.0.0

This generator creates TypeScript/JavaScript client that utilizes [axios](https://github.com/axios/axios). The generated Node module can be used in the following environments:

Environment
* Node.js
* Webpack
* Browserify

Language level
* ES5 - you must have a Promises/A+ library installed
* ES6

Module system
* CommonJS
* ES6 module system

It can be used in both TypeScript and JavaScript. In TypeScript, the definition will be automatically resolved via `package.json`. ([Reference](https://www.typescriptlang.org/docs/handbook/declaration-files/consumption.html))

### Building

To build and compile the typescript sources to javascript use:
```
npm install
npm run build
```

### Publishing

First build the package then run `npm publish`

### Consuming

navigate to the folder of your consuming project and run one of the following commands.

_published:_

```
npm install @openapitools/typescript-axios-echo-api@1.0.0 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```

### Documentation for API Endpoints

All URIs are relative to *http://localhost:3000*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AuthApi* | [**testAuthHttpBasic**](docs/AuthApi.md#testauthhttpbasic) | **POST** /auth/http/basic | To test HTTP basic authentication
*AuthApi* | [**testAuthHttpBearer**](docs/AuthApi.md#testauthhttpbearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication
*BodyApi* | [**testBinaryGif**](docs/BodyApi.md#testbinarygif) | **POST** /binary/gif | Test binary (gif) response body
*BodyApi* | [**testBodyApplicationOctetstreamBinary**](docs/BodyApi.md#testbodyapplicationoctetstreambinary) | **POST** /body/application/octetstream/binary | Test body parameter(s)
*BodyApi* | [**testBodyMultipartFormdataArrayOfBinary**](docs/BodyApi.md#testbodymultipartformdataarrayofbinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime
*BodyApi* | [**testBodyMultipartFormdataSingleBinary**](docs/BodyApi.md#testbodymultipartformdatasinglebinary) | **POST** /body/application/octetstream/single_binary | Test single binary in multipart mime
*BodyApi* | [**testEchoBodyAllOfPet**](docs/BodyApi.md#testechobodyallofpet) | **POST** /echo/body/allOf/Pet | Test body parameter(s)
*BodyApi* | [**testEchoBodyFreeFormObjectResponseString**](docs/BodyApi.md#testechobodyfreeformobjectresponsestring) | **POST** /echo/body/FreeFormObject/response_string | Test free form object
*BodyApi* | [**testEchoBodyPet**](docs/BodyApi.md#testechobodypet) | **POST** /echo/body/Pet | Test body parameter(s)
*BodyApi* | [**testEchoBodyPetResponseString**](docs/BodyApi.md#testechobodypetresponsestring) | **POST** /echo/body/Pet/response_string | Test empty response body
*BodyApi* | [**testEchoBodyStringEnum**](docs/BodyApi.md#testechobodystringenum) | **POST** /echo/body/string_enum | Test string enum response body
*BodyApi* | [**testEchoBodyTagResponseString**](docs/BodyApi.md#testechobodytagresponsestring) | **POST** /echo/body/Tag/response_string | Test empty json (request body)
*FormApi* | [**testFormIntegerBooleanString**](docs/FormApi.md#testformintegerbooleanstring) | **POST** /form/integer/boolean/string | Test form parameter(s)
*FormApi* | [**testFormObjectMultipart**](docs/FormApi.md#testformobjectmultipart) | **POST** /form/object/multipart | Test form parameter(s) for multipart schema
*FormApi* | [**testFormOneof**](docs/FormApi.md#testformoneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema
*HeaderApi* | [**testHeaderIntegerBooleanStringEnums**](docs/HeaderApi.md#testheaderintegerbooleanstringenums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s)
*PathApi* | [**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](docs/PathApi.md#testspathstringpathstringintegerpathintegerenumnonrefstringpathenumrefstringpath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)
*QueryApi* | [**testEnumRefString**](docs/QueryApi.md#testenumrefstring) | **GET** /query/enum_ref_string | Test query parameter(s)
*QueryApi* | [**testQueryDatetimeDateString**](docs/QueryApi.md#testquerydatetimedatestring) | **GET** /query/datetime/date/string | Test query parameter(s)
*QueryApi* | [**testQueryIntegerBooleanString**](docs/QueryApi.md#testqueryintegerbooleanstring) | **GET** /query/integer/boolean/string | Test query parameter(s)
*QueryApi* | [**testQueryStyleDeepObjectExplodeTrueObject**](docs/QueryApi.md#testquerystyledeepobjectexplodetrueobject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)
*QueryApi* | [**testQueryStyleDeepObjectExplodeTrueObjectAllOf**](docs/QueryApi.md#testquerystyledeepobjectexplodetrueobjectallof) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeFalseArrayInteger**](docs/QueryApi.md#testquerystyleformexplodefalsearrayinteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeFalseArrayString**](docs/QueryApi.md#testquerystyleformexplodefalsearraystring) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeTrueArrayString**](docs/QueryApi.md#testquerystyleformexplodetruearraystring) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeTrueObject**](docs/QueryApi.md#testquerystyleformexplodetrueobject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeTrueObjectAllOf**](docs/QueryApi.md#testquerystyleformexplodetrueobjectallof) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)


### Documentation For Models

 - [Bird](docs/Bird.md)
 - [Category](docs/Category.md)
 - [DataQuery](docs/DataQuery.md)
 - [DefaultValue](docs/DefaultValue.md)
 - [NumberPropertiesOnly](docs/NumberPropertiesOnly.md)
 - [Pet](docs/Pet.md)
 - [Query](docs/Query.md)
 - [StringEnumRef](docs/StringEnumRef.md)
 - [Tag](docs/Tag.md)
 - [TestFormObjectMultipartRequestMarker](docs/TestFormObjectMultipartRequestMarker.md)
 - [TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter](docs/TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.md)
 - [TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter](docs/TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter.md)


<a id="documentation-for-authorization"></a>
## Documentation For Authorization


Authentication schemes defined for the API:
<a id="http_auth"></a>
### http_auth

- **Type**: HTTP basic authentication

<a id="http_bearer_auth"></a>
### http_bearer_auth

- **Type**: Bearer authentication

