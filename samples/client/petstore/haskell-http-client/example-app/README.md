# swagger-petstore-app

This contains an example application which uses the auto-generated
swagger-petstore API Client: `haskell-http-client`

This module is not auto-generated.

The application requires a swagger petstore server running at
`http://0.0.0.0/v2`, or the value of the `HOST` environment variable.

To compile this application, the api client library bindings generated for swagger-petstore are expected to live in the parent folder.

### Petstore Server

The petstore server can be obtained at:

https://github.com/wing328/swagger-samples/tree/docker/java/java-jersey-jaxrs

Follow the instructions in the readme to install and run the petstore
server (the docker branch is used here, but docker is not required)

### Usage

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Start the petstore server (described above)
3. To run the application: 
```
stack --install-ghc exec swagger-petstore-app
```
4. After stack installs ghc on the first run, `--install-ghc` can be omitted

### Optional Environment Variables

* `HOST` - the root url of the petstore server
* `http_proxy` - the address of the http proxy 

Example: 

```
HOST=http://0.0.0.0/v2  http_proxy=http://0.0.0.0:8080 stack --install-ghc exec swagger-petstore-app
```

### Source Documentation

The application code lives in `Main.hs`, which is commented with additional implementation notes
