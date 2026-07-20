package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.jackson.nullable.JsonNullable
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid

/**
 * 
 * @param optionalNullable 
 */
data class OptionalNullableOnlyModel(

    @field:JsonInclude(JsonInclude.Include.NON_ABSENT)
    @param:JsonProperty("optionalNullable")
    @get:JsonProperty("optionalNullable") val optionalNullable: JsonNullable<java.util.UUID> = JsonNullable.undefined()
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

