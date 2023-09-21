# Helidon SE Server with OpenAPI

## Build and run

With JDK11+
```bash
mvn package
java -jar target/petstore-helidon-server-se.jar
```

## Exercise the application

```
curl -X PATCH http://petstore.swagger.io:80/v2/another-fake/dummy
curl -X GET http://petstore.swagger.io:80/v2/foo
curl -X GET http://petstore.swagger.io:80/v2/fake/BigDecimalMap
curl -X GET http://petstore.swagger.io:80/v2/fake/health
curl -X GET http://petstore.swagger.io:80/v2/fake/http-signature-test
curl -X POST http://petstore.swagger.io:80/v2/fake/outer/boolean
curl -X POST http://petstore.swagger.io:80/v2/fake/outer/composite
curl -X POST http://petstore.swagger.io:80/v2/fake/outer/number
curl -X POST http://petstore.swagger.io:80/v2/fake/outer/string
curl -X POST http://petstore.swagger.io:80/v2/fake/property/enum-int
curl -X PUT http://petstore.swagger.io:80/v2/fake/body-with-binary
curl -X PUT http://petstore.swagger.io:80/v2/fake/body-with-file-schema
curl -X PUT http://petstore.swagger.io:80/v2/fake/body-with-query-params
curl -X PATCH http://petstore.swagger.io:80/v2/fake
curl -X POST http://petstore.swagger.io:80/v2/fake
curl -X GET http://petstore.swagger.io:80/v2/fake
curl -X DELETE http://petstore.swagger.io:80/v2/fake
curl -X POST http://petstore.swagger.io:80/v2/fake/inline-additionalProperties
curl -X GET http://petstore.swagger.io:80/v2/fake/jsonFormData
curl -X POST http://petstore.swagger.io:80/v2/fake/nullable
curl -X PUT http://petstore.swagger.io:80/v2/fake/test-query-parameters
curl -X PATCH http://petstore.swagger.io:80/v2/fake_classname_test
curl -X POST http://petstore.swagger.io:80/v2/pet
curl -X DELETE http://petstore.swagger.io:80/v2/pet/{petId}
curl -X GET http://petstore.swagger.io:80/v2/pet/findByStatus
curl -X GET http://petstore.swagger.io:80/v2/pet/findByTags
curl -X GET http://petstore.swagger.io:80/v2/pet/{petId}
curl -X PUT http://petstore.swagger.io:80/v2/pet
curl -X POST http://petstore.swagger.io:80/v2/pet/{petId}
curl -X POST http://petstore.swagger.io:80/v2/pet/{petId}/uploadImage
curl -X POST http://petstore.swagger.io:80/v2/fake/{petId}/uploadImageWithRequiredFile
curl -X DELETE http://petstore.swagger.io:80/v2/store/order/{order_id}
curl -X GET http://petstore.swagger.io:80/v2/store/inventory
curl -X GET http://petstore.swagger.io:80/v2/store/order/{order_id}
curl -X POST http://petstore.swagger.io:80/v2/store/order
curl -X POST http://petstore.swagger.io:80/v2/user
curl -X POST http://petstore.swagger.io:80/v2/user/createWithArray
curl -X POST http://petstore.swagger.io:80/v2/user/createWithList
curl -X DELETE http://petstore.swagger.io:80/v2/user/{username}
curl -X GET http://petstore.swagger.io:80/v2/user/{username}
curl -X GET http://petstore.swagger.io:80/v2/user/login
curl -X GET http://petstore.swagger.io:80/v2/user/logout
curl -X PUT http://petstore.swagger.io:80/v2/user/{username}

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