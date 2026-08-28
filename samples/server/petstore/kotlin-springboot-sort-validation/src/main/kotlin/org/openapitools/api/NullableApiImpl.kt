package org.openapitools.api

import org.openapitools.jackson.nullable.JsonNullable
import org.openapitools.model.NullableModel
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.RestController

/**
 * Sample implementation of [NullableApi] demonstrating that the generated
 * openApiNullable annotations work correctly at runtime.
 *
 * Each method receives a deserialized [NullableModel] and asserts the expected
 * state of its fields. When Jackson correctly applies `@field:JsonSetter(nulls = Nulls.FAIL)`
 * and `JsonNullable<T>`, the assertions pass and HTTP 200 is returned. If the
 * deserialized state is wrong, the assertion throws [IllegalStateException] and
 * the request fails with HTTP 500, which causes any calling test to fail.
 *
 * The null-rejection case (`optionalNonNullable = null` in JSON) never reaches
 * a method body — Jackson's `Nulls.FAIL` throws during deserialization, and
 * Spring maps the resulting [org.springframework.http.converter.HttpMessageNotReadableException]
 * to HTTP 400 before the method is called.
 */
@RestController
class NullableApiImpl : NullableApi {

    /**
     * Endpoint for verifying the "required fields only" scenario.
     *
     * The JSON body contains only the two required fields; both optional fields are absent.
     * Expected state:
     *  - [NullableModel.optionalNonNullable] → `null`  (absent key → default null)
     *  - [NullableModel.optionalNullable]    → `JsonNullable.undefined()` (absent key → undefined)
     */
    override fun checkRequiredOnly(nullableModel: NullableModel): ResponseEntity<Unit> {
        check(nullableModel.optionalNonNullable == null) {
            "optionalNonNullable: expected null (absent from JSON), got ${nullableModel.optionalNonNullable}"
        }
        check(!nullableModel.optionalNullable.isPresent) {
            "optionalNullable: expected JsonNullable.undefined() (absent from JSON), got ${nullableModel.optionalNullable}"
        }
        return ResponseEntity(HttpStatus.OK)
    }

    /**
     * Endpoint for verifying the "optional nullable sent as explicit null" scenario.
     *
     * The JSON body contains the required fields plus `"optionalNullable": null`.
     * Expected state:
     *  - [NullableModel.optionalNullable] → `JsonNullable.of(null)`
     *    (key present with null value → isPresent = true, get() = null)
     */
    override fun checkOptionalNullableNull(nullableModel: NullableModel): ResponseEntity<Unit> {
        check(nullableModel.optionalNullable.isPresent) {
            "optionalNullable: expected JsonNullable present (explicit null in JSON), got ${nullableModel.optionalNullable}"
        }
        check(nullableModel.optionalNullable.get() == null) {
            "optionalNullable: expected null inner value, got ${nullableModel.optionalNullable.get()}"
        }
        return ResponseEntity(HttpStatus.OK)
    }

    /**
     * Endpoint for verifying the "all four fields present" scenario.
     *
     * The JSON body contains all four fields with non-null string values.
     * Expected state:
     *  - [NullableModel.optionalNonNullable] → `"opt-non-null"` (plain string)
     *  - [NullableModel.optionalNullable]    → `JsonNullable.of("opt-nullable")` (isPresent, non-null value)
     */
    override fun checkAllPresent(nullableModel: NullableModel): ResponseEntity<Unit> {
        check(nullableModel.optionalNonNullable == "opt-non-null") {
            "optionalNonNullable: expected 'opt-non-null', got ${nullableModel.optionalNonNullable}"
        }
        check(nullableModel.optionalNullable.isPresent) {
            "optionalNullable: expected JsonNullable present, got ${nullableModel.optionalNullable}"
        }
        check(nullableModel.optionalNullable.get() == "opt-nullable") {
            "optionalNullable: expected 'opt-nullable', got ${nullableModel.optionalNullable.get()}"
        }
        return ResponseEntity(HttpStatus.OK)
    }
}
