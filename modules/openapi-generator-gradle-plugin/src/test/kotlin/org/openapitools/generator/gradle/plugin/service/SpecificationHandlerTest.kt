package org.openapitools.generator.gradle.plugin.service

import org.gradle.api.GradleException
import org.gradle.api.internal.provider.DefaultProperty
import org.testng.annotations.DataProvider
import org.testng.annotations.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class SpecificationHandlerTest {

    @Test
    fun `check for result if only inputSpec set`() {
        val inputSpec = DefaultProperty(String::class.java)

        val filePath = "src/main/spec.yaml"
        inputSpec.set(filePath)

        val remoteSpec = DefaultProperty(String::class.java)

        val spec = SpecificationHandler.getInputSpec(inputSpec, remoteSpec)
        assertEquals(filePath, spec)
    }

    @Test
    fun `check for result if only remoteSpec set`() {
        val inputSpec = DefaultProperty(String::class.java)

        val remoteSpec = DefaultProperty(String::class.java)
        val fileLocation = "http://some-location/files/spec.yaml"
        remoteSpec.set(fileLocation)

        val spec = SpecificationHandler.getInputSpec(inputSpec, remoteSpec)
        assertEquals(fileLocation, spec)
    }

    @Test
    fun `check for result if both set`() {
        val inputSpec = DefaultProperty(String::class.java)
        val filePath = "src/main/spec.yaml"
        inputSpec.set(filePath)

        val remoteSpec = DefaultProperty(String::class.java)
        val fileLocation = "http://some-location/files/spec.yaml"
        remoteSpec.set(fileLocation)

        val spec = SpecificationHandler.getInputSpec(inputSpec, remoteSpec)
        assertEquals(filePath, spec)
    }

    @Test
    fun `check for result if both non set`() {
        val inputSpec = DefaultProperty(String::class.java)
        val remoteSpec = DefaultProperty(String::class.java)

        assertFailsWith<GradleException> { SpecificationHandler.getInputSpec(inputSpec, remoteSpec) }
    }

    @Test
    fun `check for result if both set null`() {
        val inputSpec = DefaultProperty(String::class.java)
        inputSpec.set(null)
        val remoteSpec = DefaultProperty(String::class.java)
        inputSpec.set(null)

        assertFailsWith<GradleException> { SpecificationHandler.getInputSpec(inputSpec, remoteSpec) }
    }

    @DataProvider(name = "validUrls")
    fun validUrls(): MutableIterator<Array<String>> {
        val testData: ArrayList<Array<String>> = arrayListOf()
        testData.add(arrayOf("http://www.asd.ss"))
        testData.add(arrayOf("https://petstore.swagger.io/v2/swagger.json"))
        testData.add(arrayOf("http://255.255.255.255"))
        testData.add(arrayOf("http://www.example.com/products?id=1&page=2"))
        testData.add(arrayOf("http://www.example.com:443"))
        testData.add(arrayOf("http://invalid.com/as%20ss"))
        return testData.iterator()
    }

    @Test(dataProvider = "validUrls")
    fun `check for result if remoteSpec is valid URL`(url: String) {
        val inputSpec = DefaultProperty(String::class.java)

        val remoteSpec = DefaultProperty(String::class.java)
        remoteSpec.set(url)

        val spec = SpecificationHandler.getInputSpec(inputSpec, remoteSpec)

        assertEquals(url, spec)
    }

    @DataProvider(name = "badUrls")
    fun badUrls(): MutableIterator<Array<String>> {
        val testData: ArrayList<Array<String>> = arrayListOf()
        testData.add(arrayOf("www.asd.ss"))
        testData.add(arrayOf("c://appData/test.txt"))
        testData.add(arrayOf("C:/Users/rguluev/AppData/Local/Temp/test_projects/gradle-customer-search/build/telekom-customer-search.openapi.yaml"))
        testData.add(arrayOf("C:\\WINDOWS\\system32\\file.txt"))
        testData.add(arrayOf("255.255.255.255"))
        testData.add(arrayOf("src/main/java/test.txt"))
        testData.add(arrayOf("folder"))
        testData.add(arrayOf("dir/subDir"))
        return testData.iterator()
    }

    @Test(dataProvider = "badUrls")
    fun `check for result if remoteSpec is invalid URL`(url: String) {
        val inputSpec = DefaultProperty(String::class.java)

        val remoteSpec = DefaultProperty(String::class.java)
        remoteSpec.set(url)

        assertFailsWith<GradleException> { SpecificationHandler.getInputSpec(inputSpec, remoteSpec) }
    }
}
