## @openapitools/typescript-axios-with-aws-iam@1.0.0

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
npm install @openapitools/typescript-axios-with-aws-iam@1.0.0 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```

### Documentation for API Endpoints

All URIs are relative to *https://abc123.execute-api.us-east-1.amazonaws.com/prod*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**createPet**](docs/DefaultApi.md#createpet) | **POST** /pet | Create a new pet
*DefaultApi* | [**getInventory**](docs/DefaultApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*DefaultApi* | [**getPetById**](docs/DefaultApi.md#getpetbyid) | **GET** /pet | Find pet by ID


### Documentation For Models

 - [Pet](docs/Pet.md)


<a id="documentation-for-authorization"></a>
## Documentation For Authorization


Authentication schemes defined for the API:
<a id="iam"></a>
### iam

- **Type**: API key
- **API key parameter name**: Authorization
- **Location**: HTTP header

