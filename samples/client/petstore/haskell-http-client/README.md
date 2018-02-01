## Swagger Auto-Generated [http-client](https://www.stackage.org/lts-10.0/package/http-client-0.5.7.1) Bindings to `Swagger Petstore` 

The library in `lib` provides auto-generated-from-Swagger [http-client](https://www.stackage.org/lts-10.0/package/http-client-0.5.7.1) bindings to the Swagger Petstore API.

Targeted swagger version: 2.0

OpenAPI-Specification: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. To build the package, and generate the documentation (recommended):
```
stack haddock
```
which will generate docs for this lib in the `docs` folder.

To generate the docs in the normal location (to enable hyperlinks to external libs), remove 
```
build:
  haddock-arguments:
    haddock-args:
    - "--odir=./docs"
```
from the stack.yaml file and run `stack haddock` again.

3. To run unit tests:
```
stack test
```

## Swagger-Codegen

The code generator that produced this library, and which explains how
to obtain and use the swagger-codegen cli tool lives at

https://github.com/swagger-api/swagger-codegen

The _language_ argument (`--lang`) passed to the cli tool used should be 

```
haskell-http-client
```

### Unsupported Swagger Features

* Model Inheritance

This is beta software; other cases may not be supported.

### Codegen "additional properties" parameters

These options allow some customization of the code generation process.

**haskell-http-client additional properties:**

| OPTION                          | DESCRIPTION                                                                                                                   | DEFAULT  | ACTUAL                                |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- | -------- | ------------------------------------- |
| allowFromJsonNulls              | allow JSON Null during model decoding from JSON                                                                               | true     | true              |
| allowNonUniqueOperationIds      | allow *different* API modules to contain the same operationId. Each API must be imported qualified                            | false    | false    |
| allowToJsonNulls                | allow emitting JSON Null during model encoding to JSON                                                                        | false    | false                |
| baseModule                      | Set the base module namespace                                                                                                 |          | SwaggerPetstore                      |
| cabalPackage                    | Set the cabal package name, which consists of one or more alphanumeric words separated by hyphens                             |          | swagger-petstore                    |
| cabalVersion                    | Set the cabal version number, consisting of a sequence of one or more integers separated by dots                              | 0.1.0.0  | 0.1.0.0                    |
| configType                      | Set the name of the type used for configuration                                                                               |          | SwaggerPetstoreConfig                      |
| dateFormat                      | format string used to parse/render a date                                                                                     | %Y-%m-%d | %Y-%m-%d                      |
| dateTimeFormat                  | format string used to parse/render a datetime. (Defaults to [formatISO8601Millis][1] when not provided)                       |          |                   |
| generateEnums                   | Generate specific datatypes for swagger enums                                                                                 | true     | true                   |
| generateFormUrlEncodedInstances | Generate FromForm/ToForm instances for models used by x-www-form-urlencoded operations (model fields must be primitive types) | true     | true |
| generateLenses                  | Generate Lens optics for Models                                                                                               | true     | true                  |
| generateModelConstructors       | Generate smart constructors (only supply required fields) for models                                                          | true     | true       |
| inlineMimeTypes                 | Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option                           | true     | true                 |
| modelDeriving                   | Additional classes to include in the deriving() clause of Models                                                              |          |                    |
| requestType                     | Set the name of the type used to generate requests                                                                            |          | SwaggerPetstoreRequest                     |
| strictFields                    | Add strictness annotations to all model fields                                                                                | true     | true                  |
| useMonadLogger                  | Use the monad-logger package to provide logging (if instead false, use the katip logging package)                             | false    | false                |

[1]: https://www.stackage.org/haddock/lts-9.0/iso8601-time-0.1.4/Data-Time-ISO8601.html#v:formatISO8601Millis

An example setting _strictFields_ and _dateTimeFormat_:

```
java -jar swagger-codegen-cli.jar generate -i petstore.yaml -l haskell-http-client -o output/haskell-http-client -DstrictFields=true -DdateTimeFormat="%Y-%m-%dT%H:%M:%S%Q%z"
```

View the full list of Codegen "config option" parameters with the command:

```
java -jar swagger-codegen-cli.jar config-help -l haskell-http-client
```

## Usage Notes

### Example SwaggerPetstore Haddock documentation 

An example of the generated haddock documentation targeting the server http://petstore.swagger.io/ (SwaggerPetstore) can be found [here][2]

[2]: https://hackage.haskell.org/package/swagger-petstore

### Example SwaggerPetstore App

An example application using the auto-generated haskell-http-client bindings for the server http://petstore.swagger.io/ can be found [here][3]

[3]: https://github.com/swagger-api/swagger-codegen/tree/master/samples/client/petstore/haskell-http-client/example-app

This library is intended to be imported qualified.

### Modules

| MODULE              | NOTES                                               |
| ------------------- | --------------------------------------------------- |
| SwaggerPetstore.Client    | use the "dispatch" functions to send requests       |
| SwaggerPetstore.Core      | core funcions, config and request types             |
| SwaggerPetstore.API       | construct api requests                              |
| SwaggerPetstore.Model     | describes api models                                |
| SwaggerPetstore.MimeTypes | encoding/decoding MIME types (content-types/accept) |
| SwaggerPetstore.ModelLens | lenses for model fields                             |
| SwaggerPetstore.Logging   | logging functions and utils                         |


### MimeTypes

This library adds type safety around what swagger specifies as
Produces and Consumes for each Operation (e.g. the list of MIME types an
Operation can Produce (using 'accept' headers) and Consume (using 'content-type' headers).

For example, if there is an Operation named _addFoo_, there will be a
data type generated named _AddFoo_ (note the capitalization), which
describes additional constraints and actions on the _addFoo_ operation
via its typeclass instances. These typeclass instances can be viewed
in GHCi or via the Haddocks.

* required parameters are included as function arguments to _addFoo_
* optional non-body parameters are included by using  `applyOptionalParam`
* optional body parameters are set by using  `setBodyParam`

Example code generated for pretend _addFoo_ operation: 

```haskell
data AddFoo 	
instance Consumes AddFoo MimeJSON
instance Produces AddFoo MimeJSON
instance Produces AddFoo MimeXML
instance HasBodyParam AddFoo FooModel
instance HasOptionalParam AddFoo FooName
instance HasOptionalParam AddFoo FooId
```

this would indicate that:

* the _addFoo_ operation can consume JSON
* the _addFoo_ operation produces JSON or XML, depending on the argument passed to the dispatch function
* the _addFoo_ operation can set it's body param of _FooModel_ via `setBodyParam`
* the _addFoo_ operation can set 2 different optional parameters via `applyOptionalParam`

If the swagger spec doesn't declare it can accept or produce a certain
MIME type for a given Operation, you should either add a Produces or
Consumes instance for the desired MIME types (assuming the server
supports it), use `dispatchLbsUnsafe` or modify the swagger spec and
run the generator again.

New MIME type instances can be added via MimeType/MimeRender/MimeUnrender

Only JSON instances are generated by default, and in some case
x-www-form-urlencoded instances (FromFrom, ToForm) will also be
generated if the model fields are primitive types, and there are
Operations using x-www-form-urlencoded which use those models.

### Authentication

A haskell data type will be generated for each swagger authentication type.

If for example the AuthMethod `AuthOAuthFoo` is generated for OAuth operations, then
`addAuthMethod` should be used to add the AuthMethod config.

When a request is dispatched, if a matching auth method is found in
the config, it will be applied to the request.

### Example

```haskell
mgr <- newManager defaultManagerSettings
config0 <- withStdoutLogging =<< newConfig 
let config = config0
    `addAuthMethod` AuthOAuthFoo "secret-key"

let addFooRequest = 
  addFoo 
    (ContentType MimeJSON) 
    (Accept MimeXML) 
    (ParamBar paramBar)
    (ParamQux paramQux)
    modelBaz
  `applyOptionalParam` FooId 1
  `applyOptionalParam` FooName "name"
  `setHeader` [("qux_header","xxyy")]
addFooResult <- dispatchMime mgr config addFooRequest
```

See the example app and the haddocks for details.
