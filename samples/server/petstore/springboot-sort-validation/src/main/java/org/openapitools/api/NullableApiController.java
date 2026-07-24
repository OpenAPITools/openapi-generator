package org.openapitools.api;

import org.openapitools.model.NullableModel;
import org.openapitools.model.OptionalNullableOnlyModel;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;

/**
 * Sample implementation of {@link NullableApi} demonstrating that the generated
 * nullable annotations work correctly at runtime.
 *
 * Each method receives a deserialized {@link NullableModel} and asserts the expected
 * state of its fields. When Jackson correctly applies {@code @JsonSetter(nulls = Nulls.FAIL)}
 * and {@code JsonNullable<T>}, the assertions pass and HTTP 200 is returned. If the
 * deserialized state is wrong, the assertion throws {@link IllegalStateException} and
 * the request fails with HTTP 500, causing any calling test to fail with a clear message.
 */
@RestController
public class NullableApiController implements NullableApi {

    /**
     * POST with only required fields — asserts optional fields are absent/undefined.
     *
     * The JSON body contains only the two required fields; both optional fields are absent.
     * Expected state:
     * <ul>
     *   <li>{@code optionalNonNullable} → {@code null} (absent key → default null)</li>
     *   <li>{@code optionalNullable} → {@code JsonNullable.undefined()} (absent key → undefined)</li>
     * </ul>
     */
    @Override
    public ResponseEntity<Void> checkRequiredOnly(@Valid NullableModel nullableModel) {
        if (nullableModel.getOptionalNonNullable() != null) {
            throw new IllegalStateException(
                    "optionalNonNullable: expected null (absent from JSON), got "
                            + nullableModel.getOptionalNonNullable());
        }
        if (nullableModel.getOptionalNullable().isPresent()) {
            throw new IllegalStateException(
                    "optionalNullable: expected JsonNullable.undefined() (absent from JSON), got "
                            + nullableModel.getOptionalNullable());
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * POST with optionalNullable set to null — asserts JsonNullable is present-with-null.
     *
     * The JSON body contains the required fields plus {@code "optionalNullable": null}.
     * Expected state:
     * <ul>
     *   <li>{@code optionalNullable} → {@code JsonNullable.of(null)}
     *       (key present with null value → isPresent = true, get() = null)</li>
     * </ul>
     */
    @Override
    public ResponseEntity<Void> checkOptionalNullableNull(@Valid NullableModel nullableModel) {
        if (!nullableModel.getOptionalNullable().isPresent()) {
            throw new IllegalStateException(
                    "optionalNullable: expected JsonNullable present (explicit null in JSON), got "
                            + nullableModel.getOptionalNullable());
        }
        if (nullableModel.getOptionalNullable().get() != null) {
            throw new IllegalStateException(
                    "optionalNullable: expected null inner value, got "
                            + nullableModel.getOptionalNullable().get());
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * POST with all 4 fields present — asserts each field has the expected value.
     *
     * The JSON body contains all four fields with non-null string values.
     * Expected state:
     * <ul>
     *   <li>{@code optionalNonNullable} → {@code "opt-non-null"}</li>
     *   <li>{@code optionalNullable} → {@code JsonNullable.of("opt-nullable")} (isPresent, non-null value)</li>
     * </ul>
     */
    @Override
    public ResponseEntity<Void> checkAllPresent(@Valid NullableModel nullableModel) {
        if (!"opt-non-null".equals(nullableModel.getOptionalNonNullable())) {
            throw new IllegalStateException(
                    "optionalNonNullable: expected 'opt-non-null', got "
                            + nullableModel.getOptionalNonNullable());
        }
        if (!nullableModel.getOptionalNullable().isPresent()) {
            throw new IllegalStateException(
                    "optionalNullable: expected JsonNullable present, got "
                            + nullableModel.getOptionalNullable());
        }
        if (!"opt-nullable".equals(nullableModel.getOptionalNullable().get())) {
            throw new IllegalStateException(
                    "optionalNullable: expected 'opt-nullable', got "
                            + nullableModel.getOptionalNullable().get());
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * POST a model whose only property is optional+nullable.
     *
     * This endpoint exists purely to exercise code generation for a model whose sole
     * property is both optional and nullable. Its presence ensures the generated
     * {@link OptionalNullableOnlyModel} compiles (in particular that the
     * {@code com.fasterxml.jackson.annotation.JsonInclude} import is emitted).
     */
    @Override
    public ResponseEntity<Void> postOptionalNullableOnly(@Valid OptionalNullableOnlyModel optionalNullableOnlyModel) {
        return new ResponseEntity<>(HttpStatus.OK);
    }
}

