package org.openapitools.client

import io.kotlintest.matchers.string.shouldContain
import io.kotlintest.specs.ShouldSpec
import org.junit.jupiter.api.Assertions
import org.openapitools.client.apis.BodyApi
import java.io.File
import java.io.FileWriter

class CustomTests : ShouldSpec({
    val bodyApi = BodyApi()

    should("send a single file as null") {
        val result = bodyApi.testBodyMultipartFormdataSingleBinary(null)
        val parsedResult = EchoServerResponseParser(result)

        parsedResult.body shouldContain """
            Content-Disposition: form-data; name="my-file"
            Content-Length: 0
        """.trimIndent()
    }

    should("send a single file") {
        val result = bodyApi.testBodyMultipartFormdataSingleBinary(createTestFile())
        val parsedResult = EchoServerResponseParser(result)

        parsedResult.body shouldContain """
            Content-Disposition: form-data; name="my-file"; filename="test.txt"
            Content-Type: text/plain
            Content-Length: 12
            
            testing only
        """.trimIndent()
    }

    should("send several files") {
        val result = bodyApi.testBodyMultipartFormdataArrayOfBinary(listOf(createTestFile(), createOtherTestFile()))
        val parsedResult = EchoServerResponseParser(result)

        parsedResult.body shouldContain """
            Content-Disposition: form-data; name="files"; filename="test.txt"
            Content-Type: text/plain
            Content-Length: 12
            
            testing only
        """.trimIndent()

        parsedResult.body shouldContain """
            Content-Disposition: form-data; name="files"; filename="otherTestFile.txt"
            Content-Type: text/plain
            Content-Length: 17

            Another test file
        """.trimIndent()
    }
})

private fun createTestFile(): File {
    val myFile = File("test.txt")
    if (!myFile.exists()) {
        Assertions.assertTrue(myFile.createNewFile())
    }
    val fw = FileWriter(myFile)
    fw.write("testing only")
    fw.close()
    myFile.deleteOnExit()
    return myFile
}

private fun createOtherTestFile(): File {
    val myFile = File("otherTestFile.txt")
    if (!myFile.exists()) {
        Assertions.assertTrue(myFile.createNewFile())
    }
    val fw = FileWriter(myFile)
    fw.write("Another test file")
    fw.close()
    myFile.deleteOnExit()
    return myFile
}