## OpenAPI Auto-Generated [http-client](https://www.stackage.org/lts-10.0/package/http-client-0.5.7.1) Bindings to `OpenAPI Petstore`

The library in `lib` provides auto-generated-from-OpenAPI [http-client](https://www.stackage.org/lts-10.0/package/http-client-0.5.7.1) bindings to the OpenAPI Petstore API.

OpenApi Version: 3.0.1

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

## OpenAPI-Generator

The code generator that produced this library, and which explains how
to obtain and use the openapi-generator cli tool lives at

https://openapi-generator.tech

The _generator-name_ argument (`--generator-name`) passed to the cli tool used should be

```
haskell-http-client
```

### Unsupported OpenAPI Features

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
| baseModule                      | Set the base module namespace                                                                                                 |          | OpenAPIPetstore                      |
| cabalPackage                    | Set the cabal package name, which consists of one or more alphanumeric words separated by hyphens                             |          | openapi-petstore                    |
| cabalVersion                    | Set the cabal version number, consisting of a sequence of one or more integers separated by dots                              | 0.1.0.0  | 0.1.0.0                    |
| customTestInstanceModule        | test module used to provide typeclass instances for types not known by the generator                                          |          |         |
| configType                      | Set the name of the type used for configuration                                                                               |          | OpenAPIPetstoreConfig                      |
| dateFormat                      | format string used to parse/render a date                                                                                     | %Y-%m-%d | %Y-%m-%d                      |
| dateTimeFormat                  | format string used to parse/render a datetime. (Defaults to [formatISO8601Millis][1] when not provided)                       |          |                   |
| dateTimeParseFormat             | overrides the format string used to parse a datetime                                                                          |          |              |
| generateEnums                   | Generate specific datatypes for OpenAPI enums                                                                                 | true     | true                   |
| generateFormUrlEncodedInstances | Generate FromForm/ToForm instances for models used by x-www-form-urlencoded operations (model fields must be primitive types) | true     | true |
| generateLenses                  | Generate Lens optics for Models                                                                                               | true     | true                  |
| generateModelConstructors       | Generate smart constructors (only supply required fields) for models                                                          | true     | true       |
| inlineMimeTypes                 | Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option                           | true     | true                 |
| modelDeriving                   | Additional classes to include in the deriving() clause of Models                                                              |          |                    |
| requestType                     | Set the name of the type used to generate requests                                                                            |          | OpenAPIPetstoreRequest                     |
| strictFields                    | Add strictness annotations to all model fields                                                                                | true     | true                  |
| useKatip                        | Sets the default value for the UseKatip cabal flag. If true, the katip package provides logging instead of monad-logger       | true     | true                      |

[1]: https://www.stackage.org/haddock/lts-9.0/iso8601-time-0.1.4/Data-Time-ISO8601.html#v:formatISO8601Millis

An example setting _dateTimeFormat_ and _strictFields_:

```
java -jar openapi-generator-cli.jar generate -i petstore.yaml -g haskell-http-client -o output/haskell-http-client --additional-properties=dateTimeFormat="%Y-%m-%dT%H:%M:%S%Q%z" --additional-properties=strictFields=false 
```

View the full list of Codegen "config option" parameters with the command:

```
java -jar openapi-generator-cli.jar config-help -g haskell-http-client
```

## Usage Notes

### Example Petstore Haddock documentation

An example of the generated haddock documentation targeting the server http://petstore.swagger.io/ (Petstore) can be found [here][2]

[2]: https://hackage.haskell.org/package/swagger-petstore

### Example Petstore App

An example application using the auto-generated haskell-http-client bindings for the server http://petstore.swagger.io/ can be found [here][3]

[3]: https://github.com/openapitools/openapi-generator/tree/master/samples/client/petstore/haskell-http-client/example-app

This library is intended to be imported qualified.

### Modules

| MODULE              | NOTES                                               |
| ------------------- | --------------------------------------------------- |
| OpenAPIPetstore.Client    | use the "dispatch" functions to send requests       |
| OpenAPIPetstore.Core      | core funcions, config and request types             |
| OpenAPIPetstore.API       | construct api requests                              |
| OpenAPIPetstore.Model     | describes api models                                |
| OpenAPIPetstore.MimeTypes | encoding/decoding MIME types (content-types/accept) |
| OpenAPIPetstore.ModelLens | lenses for model fields                             |
| OpenAPIPetstore.Logging   | logging functions and utils                         |


### MimeTypes

This library adds type safety around what OpenAPI specifies as
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

If the OpenAPI spec doesn't declare it can accept or produce a certain
MIME type for a given Operation, you should either add a Produces or
Consumes instance for the desired MIME types (assuming the server
supports it), use `dispatchLbsUnsafe` or modify the OpenAPI spec and
run the generator again.

New MIME type instances can be added via MimeType/MimeRender/MimeUnrender

Only JSON instances are generated by default, and in some case
x-www-form-urlencoded instances (FromFrom, ToForm) will also be
generated if the model fields are primitive types, and there are
Operations using x-www-form-urlencoded which use those models.

### Authentication

A haskell data type will be generated for each OpenAPI authentication type.

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
