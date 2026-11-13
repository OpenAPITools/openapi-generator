package org.openapitools.integration

import io.kotlintest.shouldBe
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.infrastructure.ApiClient
import org.openapitools.client.models.Category
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Tag
import tools.jackson.databind.json.JsonMapper
import tools.jackson.module.kotlin.KotlinModule
import tools.jackson.module.kotlin.readValue

class JsonSerializationTest : ShouldSpec() {
    // Same mapper as the client: ktor-serialization-jackson3 adds the Kotlin module, then applies ApiClient.JSON_DEFAULT.
    private val mapper: JsonMapper = JsonMapper.builder()
        .addModule(KotlinModule.Builder().build())
        .apply(ApiClient.JSON_DEFAULT)
        .build()

    init {
        should("serialize a pet with Jackson 3") {
            val pet = Pet(
                name = "doggie",
                photoUrls = listOf("https://example.com/doggie.png"),
                id = 10,
                category = Category(id = 1, name = "Dogs"),
                tags = listOf(Tag(id = 2, name = "friendly")),
                status = Pet.Status.available,
            )

            val tree = mapper.readTree(mapper.writeValueAsString(pet))

            tree.get("name").asString() shouldBe "doggie"
            tree.get("category").get("name").asString() shouldBe "Dogs"
            tree.get("tags").get(0).get("name").asString() shouldBe "friendly"
            tree.get("status").asString() shouldBe "available"
        }

        should("deserialize a pet with Jackson 3") {
            val json = """
                {
                  "id": 10,
                  "name": "doggie",
                  "photoUrls": ["https://example.com/doggie.png"],
                  "category": {"id": 1, "name": "Dogs"},
                  "tags": [{"id": 2, "name": "friendly"}],
                  "status": "sold"
                }
            """.trimIndent()

            val pet = mapper.readValue<Pet>(json)

            pet shouldBe Pet(
                name = "doggie",
                photoUrls = listOf("https://example.com/doggie.png"),
                id = 10,
                category = Category(id = 1, name = "Dogs"),
                tags = listOf(Tag(id = 2, name = "friendly")),
                status = Pet.Status.sold,
            )
        }

        should("round-trip a pet with Jackson 3") {
            val pet = Pet(name = "doggie", photoUrls = emptyList(), status = Pet.Status.pending)

            mapper.readValue<Pet>(mapper.writeValueAsString(pet)) shouldBe pet
        }
    }
}
