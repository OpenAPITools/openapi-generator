# Org.OpenAPITools - ASP.NET Core 3.0 Server

This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.

## Run

Linux/OS X:

```
sh build.sh
```

Windows:

```
build.bat
```
## Run in Docker

```
cd src/Org.OpenAPITools
docker build -t org.openapitools .
docker run -p 5000:8080 org.openapitools
```
