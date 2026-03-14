package org.openapitools.client

import io.kotlintest.matchers.string.shouldContain
import io.kotlintest.matchers.string.shouldNotContain
import io.kotlintest.specs.ShouldSpec
import org.junit.jupiter.api.Assertions
import org.openapitools.client.apis.BodyApi
import org.openapitools.client.models.FileMetadata
import java.io.File
import java.io.FileWriter

class MultipartJsonTest : ShouldSpec({
    val bodyApi = BodyApi()

    should("serialize JSON part with proper JSON format, not toString()") {
        val metadata = FileMetadata(
            id = 12345L,
            name = "test-file",
            tags = listOf("tag1", "tag2")
        )
        val testFile = createTestFile()
        
        val result = bodyApi.testBodyMultipartFormdataWithJsonPart(metadata, testFile)
        val parsedResult = EchoServerResponseParser(result)

        // The metadata part should contain proper JSON serialization
        parsedResult.body shouldContain """"id":12345"""
        parsedResult.body shouldContain """"name":"test-file""""
        parsedResult.body shouldContain """"tags":["tag1","tag2"]"""
        
        // Should NOT contain Kotlin's toString() format like "FileMetadata(id=12345, name=test-file)"
        parsedResult.body shouldNotContain "FileMetadata("
        
        // Should have proper Content-Type for metadata part
        parsedResult.body shouldContain """Content-Type: application/json"""
        
        // File part should be present
        parsedResult.body shouldContain """Content-Disposition: form-data; name="file"; filename="test.txt""""
        parsedResult.body shouldContain """testing multipart with json"""
    }

    should("not throw IllegalArgumentException for Content-Type in headers") {
        // This test verifies that Content-Type is properly filtered from headers
        // and passed to OkHttp's asRequestBody/toRequestBody methods instead
        val metadata = FileMetadata(
            id = 999L,
            name = "another-test"
        )
        val testFile = createTestFile()
        
        // Should not throw: java.lang.IllegalArgumentException: Unexpected header: Content-Type
        val result = bodyApi.testBodyMultipartFormdataWithJsonPart(metadata, testFile)
        
        // If we get here without exception, the fix is working
        Assertions.assertNotNull(result)
    }
})

private fun createTestFile(): File {
    val myFile = File("test.txt")
    if (!myFile.exists()) {
        Assertions.assertTrue(myFile.createNewFile())
    }
    val fw = FileWriter(myFile)
    fw.write("testing multipart with json")
    fw.close()
    myFile.deleteOnExit()
    return myFile
}
