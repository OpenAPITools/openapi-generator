package org.openapitools.client.infrastructure

import io.kotlintest.shouldNotThrow
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.BirdApi
import org.openapitools.client.models.Bird
import java.io.File
import java.math.BigDecimal
import java.util.UUID

class MultipartJsonSerializationTest : ShouldSpec({
    should("serialize Bird model in multipart without SerializationException") {
        val bird = Bird(
            id = UUID.randomUUID(),
            featherType = "fluffy",
            optionalProperty = BigDecimal("42.0")
        )
        
        // Create a test file
        val testFile = File.createTempFile("test", ".txt")
        testFile.writeText("test content")
        testFile.deleteOnExit()
        
        val birdApi = BirdApi("http://example.org")
        
        // This should NOT throw SerializationException
        // The generated code creates a PartConfig with a serializer lambda that captures the Bird type
        // Before this fix, kotlinx.serialization would fail with "Cannot serialize Any?"
        shouldNotThrow<kotlinx.serialization.SerializationException> {
            // We build the RequestConfig which creates PartConfig with the serializer
            birdApi.uploadBirdWithMetadataRequestConfig(metadata = bird, file = testFile)
        }
    }
})
