# openapi-petstore-tests-integration

This contains integration tests for the haskell http-client openapi-petstore api client library.

This module is not auto-generated.

The integration tests require a openapi petstore server running at
`http://0.0.0.0/v2`, or the value of the `HOST` environment variable.

The api client library bindings are expected to live in the parent folder


### Petstore Server

The petstore server can be obtained at:

https://github.com/wing328/swagger-samples/tree/docker/java/java-jersey-jaxrs

Follow the instructions in the readme to install and run the petstore
server (the docker branch is used here, but docker is not required)

### Usage

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Start the petstore server (described above)
3. To run the integration tests: 
```
stack --install-ghc test
```
4. After stack installs ghc on the first run, `--install-ghc` can be omitted

### Optional Environment Variables

* `HOST` - the root url of the petstore server
* `http_proxy` - the address of the http proxy 

Example: 

```
HOST=http://0.0.0.0/v2  http_proxy=http://0.0.0.0:8080 stack --install-ghc test
```


### Running with Maven

If using Maven, after ensuring the haskell `stack` tool is installed
(run `stack --version` to verify installation), an example command to
run the integration tests with maven in this directory is:

```
mvn -q verify -Pintegration-test
```

Adjust `pom.xml` as necessary to set environment variables.
