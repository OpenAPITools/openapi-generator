## any-of-union@1.0.0

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
npm install any-of-union@1.0.0 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```

### Documentation for API Endpoints

All URIs are relative to *http://localhost*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**getSettings**](docs/DefaultApi.md#getsettings) | **GET** /settings | 
*DefaultApi* | [**listItems**](docs/DefaultApi.md#listitems) | **GET** /items | 


### Documentation For Models

 - [ActionError](docs/ActionError.md)
 - [ActionErrorErrorsInner](docs/ActionErrorErrorsInner.md)
 - [AllOfWithAnyOf](docs/AllOfWithAnyOf.md)
 - [Cat](docs/Cat.md)
 - [Detailed](docs/Detailed.md)
 - [DictionaryWithAnyOf](docs/DictionaryWithAnyOf.md)
 - [Dog](docs/Dog.md)
 - [GetSettings200Response](docs/GetSettings200Response.md)
 - [GetSettings200ResponseListUnion](docs/GetSettings200ResponseListUnion.md)
 - [GetSettings200ResponseMeta](docs/GetSettings200ResponseMeta.md)
 - [GetSettings200ResponseOverridesValue](docs/GetSettings200ResponseOverridesValue.md)
 - [Holder](docs/Holder.md)
 - [Known](docs/Known.md)
 - [Labelled](docs/Labelled.md)
 - [ListItems200Response](docs/ListItems200Response.md)
 - [ListItems200ResponseItemsInner](docs/ListItems200ResponseItemsInner.md)
 - [ListItems422Response](docs/ListItems422Response.md)
 - [Message](docs/Message.md)
 - [MixedValue](docs/MixedValue.md)
 - [OneOfWithAnyOf](docs/OneOfWithAnyOf.md)
 - [Pet](docs/Pet.md)
 - [RequiredWithAnyOf](docs/RequiredWithAnyOf.md)
 - [Summary](docs/Summary.md)


<a id="documentation-for-authorization"></a>
## Documentation For Authorization

Endpoints do not require authorization.

