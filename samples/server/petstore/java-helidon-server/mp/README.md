# Helidon Server with OpenAPI

## Build and run

With JDK11+
```bash
mvn package
java -jar target/petstore-helidon-server-mp.jar
```

## Exercise the application

```
curl -X PATCH http://petstore.swagger.io:80/v2
curl -X GET http://petstore.swagger.io:80/v2
curl -X GET http://petstore.swagger.io:80/v2/BigDecimalMap
curl -X GET http://petstore.swagger.io:80/v2/health
curl -X GET http://petstore.swagger.io:80/v2/http-signature-test
curl -X POST http://petstore.swagger.io:80/v2/outer/boolean
curl -X POST http://petstore.swagger.io:80/v2/outer/composite
curl -X POST http://petstore.swagger.io:80/v2/outer/number
curl -X POST http://petstore.swagger.io:80/v2/outer/string
curl -X POST http://petstore.swagger.io:80/v2/property/enum-int
curl -X PUT http://petstore.swagger.io:80/v2/body-with-binary
curl -X PUT http://petstore.swagger.io:80/v2/body-with-file-schema
curl -X PUT http://petstore.swagger.io:80/v2/body-with-query-params
curl -X PATCH http://petstore.swagger.io:80/v2
curl -X POST http://petstore.swagger.io:80/v2
curl -X GET http://petstore.swagger.io:80/v2
curl -X DELETE http://petstore.swagger.io:80/v2
curl -X POST http://petstore.swagger.io:80/v2/inline-additionalProperties
curl -X POST http://petstore.swagger.io:80/v2/inline-freeform-additionalProperties
curl -X GET http://petstore.swagger.io:80/v2/jsonFormData
curl -X POST http://petstore.swagger.io:80/v2/nullable
curl -X PUT http://petstore.swagger.io:80/v2/test-query-parameters
curl -X PATCH http://petstore.swagger.io:80/v2
curl -X POST http://petstore.swagger.io:80/v2/pet
curl -X DELETE http://petstore.swagger.io:80/v2/pet/{petId}
curl -X GET http://petstore.swagger.io:80/v2/pet/findByStatus
curl -X GET http://petstore.swagger.io:80/v2/pet/findByTags
curl -X GET http://petstore.swagger.io:80/v2/pet/{petId}
curl -X PUT http://petstore.swagger.io:80/v2/pet
curl -X POST http://petstore.swagger.io:80/v2/pet/{petId}
curl -X POST http://petstore.swagger.io:80/v2/pet/{petId}/uploadImage
curl -X POST http://petstore.swagger.io:80/v2/fake/{petId}/uploadImageWithRequiredFile
curl -X DELETE http://petstore.swagger.io:80/v2/order/{order_id}
curl -X GET http://petstore.swagger.io:80/v2/inventory
curl -X GET http://petstore.swagger.io:80/v2/order/{order_id}
curl -X POST http://petstore.swagger.io:80/v2/order
curl -X POST http://petstore.swagger.io:80/v2
curl -X POST http://petstore.swagger.io:80/v2/createWithArray
curl -X POST http://petstore.swagger.io:80/v2/createWithList
curl -X DELETE http://petstore.swagger.io:80/v2/{username}
curl -X GET http://petstore.swagger.io:80/v2/{username}
curl -X GET http://petstore.swagger.io:80/v2/login
curl -X GET http://petstore.swagger.io:80/v2/logout
curl -X PUT http://petstore.swagger.io:80/v2/{username}

```

## Try health and metrics

```
curl -s -X GET http://petstore.swagger.io:80/v2/health
{"outcome":"UP",...
. . .

# Prometheus Format
curl -s -X GET http://petstore.swagger.io:80/v2/metrics
# TYPE base:gc_g1_young_generation_count gauge
. . .

# JSON Format
curl -H 'Accept: application/json' -X GET http://petstore.swagger.io:80/v2/metrics
{"base":...
. . .
```