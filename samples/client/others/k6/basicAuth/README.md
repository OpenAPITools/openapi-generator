# Generated k6 script

The `script.js` file contains most of the Swagger/OpenAPI specification and you can customize it to your needs.

Global header variables are defined at the top of the file, like `api_key`. Each path in the specification is converted into a [group](https://docs.k6.io/docs/tags-and-groups) in k6 and each group contains all the request methods related to that path. Path and query parameters are extracted from the specification and put at the start of the group. The URL is constructed from the base URL plus path and query.

If the Swagger/OpenAPI specification used as the input spec contains examples at parameter level, those will be extracted and utilized as parameter values. The `handleParamValue` custom Mustache lambda registered for use in the K6 `script.mustache` template handles the conditional checks, formatting, and outputting of parameter values. If a given parameter has value specified – either in `example` or `examples` field, defined at the parameter level – that value will be used. For list (`examples`), entire list will be output in the generated script and the first element from that list will be assigned as parameter value. If a given parameter does not have an example defined, a placeholder value with `TODO_EDIT_THE_` prefix will be generated for that parameter, and you will have to assign a value before you can run the script. In other words, you can now generate K6 test scripts which are ready to run, provided the Swagger/OpenAPI specification used as the input spec contains examples for all of the path/query parameters; see `modules/openapi-generator/src/test/resources/3_0/examples.yaml` for an example of such specification, and https://swagger.io/docs/specification/adding-examples/ for more information about adding examples.

k6 specific parameters are in the [`params`](https://docs.k6.io/docs/params-k6http) object, and `body` contains the [request](https://docs.k6.io/docs/http-requests) body which is in the form of `identifier: type`, which the `type` should be substituted by a proper value. Then goes the request and the check.

[Check](https://docs.k6.io/docs/checks) are like asserts but differ in that they don't halt execution, instead they just store the result of the check, pass or fail, and let the script execution continue.

Each request is always followed by a 0.1 second [sleep](https://docs.k6.io/docs/sleep-t-1) to prevent the script execution from flooding the system with too many requests simultaneously.

Note that the default iteration count and VU count is 1. So each request in each group will be executed once. For more information, see the [k6 options](https://docs.k6.io/docs/options).
