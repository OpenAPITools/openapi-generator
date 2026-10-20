package org.openapitools.api

import com.fasterxml.jackson.databind.ObjectMapper
import org.junit.jupiter.api.Test
import org.openapitools.jackson.nullable.JsonNullable
import org.openapitools.model.NullableModel
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.http.MediaType
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.post
import org.assertj.core.api.Assertions.assertThat

/**
 * Verifies the runtime behaviour of the openApiNullable annotations generated onto [NullableApi]:
 *
 * - **`@field:JsonSetter(nulls = Nulls.FAIL)`** on `optionalNonNullable` — sending an explicit JSON
 *   null for a non-nullable optional field is rejected with HTTP 400 (Jackson fires before the method
 *   is called; Spring maps `HttpMessageNotReadableException` to 400).
 * - **`JsonNullable<T>`** on `optionalNullable` — Jackson correctly distinguishes three states:
 *   - **absent** (`JsonNullable.undefined()`) when the key is omitted from the JSON body
 *   - **present-with-null** (`JsonNullable.of(null)`) when `"optionalNullable": null` is sent
 *   - **present-with-value** (`JsonNullable.of("...")`) when a non-null value is sent
 *
 * HTTP 200 responses confirm both that the request was accepted *and* that [NullableApiImpl]'s
 * internal assertions about the deserialized state passed.
 * HTTP 400 responses confirm that Jackson (or bean validation) rejected the invalid input.
 */
@SpringBootTest
@AutoConfigureMockMvc
class NullableApiValidationTest {

    @Autowired
    lateinit var mockMvc: MockMvc

    @Autowired
    lateinit var objectMapper: ObjectMapper

    // ── checkRequiredOnly — positive cases ───────────────────────────────────
    // Endpoint: POST /nullable/check-required-only
    // Impl asserts: optionalNonNullable == null && !optionalNullable.isPresent

    @Test
    fun `checkRequiredOnly - only required fields returns 200 and impl assertions pass`() {
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_REQUIRED_ONLY}") {
            contentType = MediaType.APPLICATION_JSON
            content = """{"requiredNonNullable":"foo","requiredNullable":"bar"}"""
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `checkRequiredOnly - requiredNullable sent as explicit null is accepted`() {
        // requiredNullable is String? — explicit null is allowed
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_REQUIRED_ONLY}") {
            contentType = MediaType.APPLICATION_JSON
            content = """{"requiredNonNullable":"foo","requiredNullable":null}"""
        }.andExpect { status { isOk() } }
    }

    // ── checkRequiredOnly — null-rejection case ───────────────────────────────
    // @field:JsonSetter(nulls = Nulls.FAIL) on optionalNonNullable blocks explicit null in JSON

    @Test
    fun `checkRequiredOnly - optionalNonNullable sent as explicit null returns 400`() {
        // Jackson fires Nulls.FAIL before the method body is reached → HttpMessageNotReadableException → 400
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_REQUIRED_ONLY}") {
            contentType = MediaType.APPLICATION_JSON
            content = """{"requiredNonNullable":"foo","requiredNullable":"bar","optionalNonNullable":null}"""
        }.andExpect { status { isBadRequest() } }
    }

    // ── missing required field → 400 ─────────────────────────────────────────
    // Jackson module-kotlin enforces Kotlin non-nullable types → throws on missing required field

    @Test
    fun `checkRequiredOnly - missing requiredNonNullable returns 400`() {
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_REQUIRED_ONLY}") {
            contentType = MediaType.APPLICATION_JSON
            content = """{"requiredNullable":"bar"}"""
        }.andExpect { status { isBadRequest() } }
    }

    // ── checkOptionalNullableNull — present-with-null state ──────────────────
    // Endpoint: POST /nullable/check-optional-nullable-null
    // Impl asserts: optionalNullable.isPresent && optionalNullable.get() == null

    @Test
    fun `checkOptionalNullableNull - optionalNullable as explicit null returns 200 and impl assertions pass`() {
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_OPTIONAL_NULLABLE_NULL}") {
            contentType = MediaType.APPLICATION_JSON
            content = """{"requiredNonNullable":"foo","requiredNullable":"bar","optionalNullable":null}"""
        }.andExpect { status { isOk() } }
    }

    // ── checkAllPresent — present-with-value state ───────────────────────────
    // Endpoint: POST /nullable/check-all-present
    // Impl asserts: optionalNonNullable == "opt-non-null" && optionalNullable.isPresent && get() == "opt-nullable"

    @Test
    fun `checkAllPresent - all four fields with values returns 200 and impl assertions pass`() {
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_ALL_PRESENT}") {
            contentType = MediaType.APPLICATION_JSON
            content = """
                {
                  "requiredNonNullable": "req-non-null",
                  "requiredNullable":    "req-nullable",
                  "optionalNonNullable": "opt-non-null",
                  "optionalNullable":    "opt-nullable"
                }
            """.trimIndent()
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `checkAllPresent - optionalNonNullable sent as explicit null returns 400`() {
        // Even on a different endpoint, Nulls.FAIL fires before the method body
        mockMvc.post("${NullableApi.BASE_PATH}${NullableApi.PATH_CHECK_ALL_PRESENT}") {
            contentType = MediaType.APPLICATION_JSON
            content = """
                {
                  "requiredNonNullable": "req-non-null",
                  "requiredNullable":    "req-nullable",
                  "optionalNonNullable": null,
                  "optionalNullable":    "opt-nullable"
                }
            """.trimIndent()
        }.andExpect { status { isBadRequest() } }
    }

    // ── @JsonInclude(NON_ABSENT) serialization behaviour ─────────────────────
    // These tests verify that the @field:JsonInclude(JsonInclude.Include.NON_ABSENT)
    // annotation on NullableModel.optionalNullable controls serialization correctly:
    //   - JsonNullable.undefined()  → key must be ABSENT from the JSON output
    //   - JsonNullable.of(null)     → key must be PRESENT with a null value
    //   - JsonNullable.of("value")  → key must be PRESENT with the given value

    @Test
    fun `serialization - optionalNullable undefined is absent from JSON output`() {
        val model = NullableModel(
            requiredNonNullable = "req",
            requiredNullable = "req-null",
            optionalNullable = JsonNullable.undefined()
        )
        val json = objectMapper.writeValueAsString(model)
        assertThat(json).doesNotContain("optionalNullable")
    }

    @Test
    fun `serialization - optionalNullable of(null) is present as null in JSON output`() {
        val model = NullableModel(
            requiredNonNullable = "req",
            requiredNullable = "req-null",
            optionalNullable = JsonNullable.of(null)
        )
        val json = objectMapper.writeValueAsString(model)
        assertThat(json).contains("\"optionalNullable\":null")
    }

    @Test
    fun `serialization - optionalNullable of(value) is present with value in JSON output`() {
        val model = NullableModel(
            requiredNonNullable = "req",
            requiredNullable = "req-null",
            optionalNullable = JsonNullable.of("hello")
        )
        val json = objectMapper.writeValueAsString(model)
        assertThat(json).contains("\"optionalNullable\":\"hello\"")
    }
}
