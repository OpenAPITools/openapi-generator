package org.openapitools.client

import io.kotlintest.shouldBe
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.BodyApi
import org.openapitools.client.apis.QueryApi
import org.openapitools.client.models.Category
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Tag

class PetApiTest : ShouldSpec() {
    init {
        val petId:Long = 10006
        val queryApi = QueryApi("http://localhost:3000")
        val bodyApi = BodyApi("http://localhost:3000")

        should("return a pet") {
            val pet : Pet = Pet(
                id = petId,
                name = "kotlin client test",
                photoUrls = listOf("http://test_kotlin_unit_test.com"),
                category = Category(petId, "test kotlin category"),
                tags = listOf(Tag(petId, "test kotlin tag"))
            )

            /* comment out the following as for some reasons http request body contains
            a line "cc" before the JSON payload, e.g.

            Echo mode switched on for this request
--> POST /echo/body/Pet HTTP/1.1
--> Connection: Upgrade, HTTP2-Settings
--> Host: localhost:3000
--> HTTP2-Settings: AAEAAEAAAAIAAAABAAMAAABkAAQBAAAAAAUAAEAA
--> Transfer-encoding: chunked
--> Upgrade: h2c
--> User-Agent: Java-http-client/17.0.10
--> Accept: application/json
--> Content-Type: application/json
-->
-->
Found the content-type: application/json
Found the blank line before the response body
<--
[socket#8] event: data
--> cc
--> {"name":"kotlin client test","photoUrls":["http://test_kotlin_unit_test.com"],"id":10006,"category":{"id":10006,"name":"test kotlin category"},"tags":[{"id":10006,"name":"test kotlin tag"}],"status":null}
-->
<-- cc
<-- {"name":"kotlin client test","photoUrls":["http://test_kotlin_unit_test.com"],"id":10006,"category":{"id":10006,"name":"test kotlin category"},"tags":[{"id":10006,"name":"test kotlin tag"}],"status":null}
<--
[socket#8] event: data

            val result : Pet = bodyApi.testEchoBodyPet(pet)

            result.id shouldBe (petId)
            result.name shouldBe ("kotlin client test")
            result.photoUrls[0] shouldBe ("http://test_kotlin_unit_test.com")
            result.category!!.id shouldBe (petId)
            result.category!!.name shouldBe ("test kotlin category")
            result.tags!![0].id shouldBe (petId)
            result.tags!![0].name shouldBe ("test kotlin tag")

             */
        }

        should("test echo server response parser") {
            val response: String = queryApi.testQueryStyleFormExplodeTrueArrayString()
            val result = EchoServerResponseParser(response)
            result.path shouldBe ("/query/style_form/explode_true/array_string")
            result.method shouldBe ("GET")
            result.body shouldBe ("")
        }
    }
}