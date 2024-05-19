@file:Suppress(
    "RemoveRedundantQualifierName",
    "UnusedImport",
    "unused",
)

package org.openapitools.apis

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import org.openapitools.models.*

/**
 * WireMock stub request builder.
 */
open class BodyApiStubs(private val objectMapper: ObjectMapper) {

    /**
     * Construct a stub for the operation testBinaryGif.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestBinaryGifStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testBinaryGif(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBinaryGifStubBuilder =
        TestBinaryGifStubBuilder(objectMapper, post(urlPathTemplate("/binary/gif"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testBodyApplicationOctetstreamBinary.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestBodyApplicationOctetstreamBinaryStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testBodyApplicationOctetstreamBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyApplicationOctetstreamBinaryStubBuilder =
        TestBodyApplicationOctetstreamBinaryStubBuilder(objectMapper, post(urlPathTemplate("/body/application/octetstream/binary"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testBodyMultipartFormdataArrayOfBinary.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestBodyMultipartFormdataArrayOfBinaryStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testBodyMultipartFormdataArrayOfBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyMultipartFormdataArrayOfBinaryStubBuilder =
        TestBodyMultipartFormdataArrayOfBinaryStubBuilder(objectMapper, post(urlPathTemplate("/body/application/octetstream/array_of_binary"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testBodyMultipartFormdataSingleBinary.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestBodyMultipartFormdataSingleBinaryStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testBodyMultipartFormdataSingleBinary(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestBodyMultipartFormdataSingleBinaryStubBuilder =
        TestBodyMultipartFormdataSingleBinaryStubBuilder(objectMapper, post(urlPathTemplate("/body/application/octetstream/single_binary"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testEchoBodyFreeFormObjectResponseString.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestEchoBodyFreeFormObjectResponseStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testEchoBodyFreeFormObjectResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyFreeFormObjectResponseStringStubBuilder =
        TestEchoBodyFreeFormObjectResponseStringStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/FreeFormObject/response_string"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testEchoBodyPet.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestEchoBodyPetStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testEchoBodyPet(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyPetStubBuilder =
        TestEchoBodyPetStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/Pet"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testEchoBodyPetResponseString.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestEchoBodyPetResponseStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testEchoBodyPetResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyPetResponseStringStubBuilder =
        TestEchoBodyPetResponseStringStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/Pet/response_string"))
            .configurer()
        )

    /**
     * Construct a stub for the operation testEchoBodyTagResponseString.
     *
     * @param configurer configurer for the [MappingBuilder], allowing for arbitrary changes.
     * @return A [TestEchoBodyTagResponseStringStubBuilder] to configure the response, and the final [MappingBuilder].
     */
    fun testEchoBodyTagResponseString(configurer: MappingBuilder.() -> MappingBuilder = { this }): TestEchoBodyTagResponseStringStubBuilder =
        TestEchoBodyTagResponseStringStubBuilder(objectMapper, post(urlPathTemplate("/echo/body/Tag/response_string"))
            .configurer()
        )
}
