---
id: online
title: Online
---

## Hosted

We offer online services, publicly and free of charge:

- latest stable version: http://api.openapi-generator.tech
- latest master: http://api-latest-master.openapi-generator.tech (updated with latest master every hour)

> **Hosting Sponsor**   
> [![Linode Logo](https://www.linode.com/media/images/logos/standard/light/linode-logo_standard_light_small.png)](https://www.linode.com/)

These services are beta and do not have any guarantee on service level

## Docker Image

The openapi-generator-online Docker image can act as a self-hosted web application and API for generating code. This container can be incorporated into a CI pipeline, and requires at least two HTTP requests and some docker orchestration to access generated code.

Example usage:

```bash
# Start container at port 8888 and save the container id
CID=$(docker run -d -p 8888:8080 openapitools/openapi-generator-online)

# allow for startup
sleep 10

# Get the IP of the running container (optional)
GEN_IP=$(docker inspect --format '{{.NetworkSettings.IPAddress}}'  ${CID})

# Execute an HTTP request to generate a Ruby client
curl -X POST --header 'Content-Type: application/json' \
  --header 'Accept: application/json' \
  -d '{"openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"}' \
  'http://localhost:8888/api/gen/clients/ruby'

# Example output:
# {"code":"c2d483.3.4672-40e9-91df-b9ffd18d22b8","link":"http://localhost:8888/api/gen/download/c2d483.3.4672-40e9-91df-b9ffd18d22b8"}

# Download the generated zip file  (using "code" provided from your output) 
wget http://localhost:8888/api/gen/download/c2d483.3.4672-40e9-91df-b9ffd18d22b8

# Unzip the file
unzip c2d483.3.4672-40e9-91df-b9ffd18d22b8

# Shutdown the openapi generator image
docker stop ${CID} && docker rm ${CID}
```

## Local/Self-hosting

If you prefer to run the service locally, here are the steps:

```bash
mvn clean install
cd modules/openapi-generator-online
mvn spring-boot:run
```

> The online openapi-generator can be run via [Docker](#docker-image) as well.

For example, to generate Ruby API client, simply send the following HTTP request using curl:

```bash
curl -X POST -H "content-type:application/json" -d '{"openAPIUrl":"https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"}' \
    http://localhost:8080/api/gen/clients/ruby
```
Then you will receive a JSON response with the URL to download the zipped code.

To customize the SDK, you can `POST` to `http://localhost:8080/gen/clients/{generator}` with the following HTTP body:

```json
{
  "options": {},
  "openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
}
```

Here, the `options` for a language can be obtained by submitting a `GET` request to `http://locahost:8080/api/gen/clients/{generator}`:

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
  "openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
}
```

and here is the curl command:
```bash
curl -H "Content-type: application/json" \
    -X POST \
    -d '{"options": {"packageName": "pet_store"},"openAPIUrl": "https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/3_0/petstore.yaml"}' \
    http://localhost:8080/api/gen/clients/python
```

Instead of using `openAPIUrl` with an URL to the OpenAPI spec, one can include the spec in the JSON payload with `spec`:

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
