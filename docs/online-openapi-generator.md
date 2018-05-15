## Online OpenAPI generator

One can also generate API client or server using the online openapi-generator.

Here are the steps to run it locally:
```
mvn clean install
cd modules/openapi-generator-online
mvn spring-boot:run
```

For example, to generate Ruby API client, simply send the following HTTP request using curl:
```sh
curl -X POST -H "content-type:application/json" -d '{"openAPIUrl":"https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml"}' http://localhost:8080/api/gen/clients/ruby
```
Then you will receieve a JSON response with the URL to download the zipped code.

To customize the SDK, you can `POST` to `http://localhost:8080/gen/clients/{language}` with the following HTTP body:
```json
{
  "options": {},
  "openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml"
}
```
in which the `options` for a language can be obtained by submitting a `GET` request to `http://locahost:8080/api/gen/clients/{language}`:

For example, `curl http://localhost:8080/api/gen/clients/python` returns
```json
  "packageName":{
    "opt":"packageName",
    "description":"python package name (convention: snake_case).",
    "type":"string",
    "default":"openapi_client"
  },
  "packageVersion":{
    "opt":"packageVersion",
    "description":"python package version.",
    "type":"string",
    "default":"1.0.0"
  },
  "sortParamsByRequiredFlag":{
    "opt":"sortParamsByRequiredFlag",
    "description":"Sort method arguments to place required parameters before optional parameters.",
    "type":"boolean",
    "default":"true"
  }

{}
```
To set package name to `pet_store`, the HTTP body of the request is as follows:
```json
{
  "options": {
    "packageName": "pet_store"
  },
  "openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml"
}
```
and here is the curl command:
```sh
curl -H "Content-type: application/json" -X POST -d '{"options": {"packageName": "pet_store"},"openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml"}' http://localhost:8080/api/gen/clients/python
```

Instead of using `openAPIUrl` with an URL to the OpenAPI spec, one can include the spec in the JSON payload with `spec`, e.g.
```json
{
  "options": {},
  "spec": {
    "swagger": "2.0",
    "info": {
      "version": "1.0.0",
      "title": "Test API"
    },
    ...
  }
}
```
