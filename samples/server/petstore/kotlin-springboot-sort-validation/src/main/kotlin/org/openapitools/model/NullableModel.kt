package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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
 * @param requiredNonNullable 
 * @param requiredNullable 
 * @param optionalNonNullable 
 * @param optionalNullable 
 */
data class NullableModel(

    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("requiredNonNullable")
    @get:JsonProperty("requiredNonNullable", required = true) val requiredNonNullable: kotlin.String,

    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("requiredNullable")
    @get:JsonProperty("requiredNullable", required = true) val requiredNullable: kotlin.String?,

    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.FAIL)
    @param:JsonProperty("optionalNonNullable")
    @get:JsonProperty("optionalNonNullable") val optionalNonNullable: kotlin.String? = null,

    @param:JsonProperty("optionalNullable")
    @get:JsonProperty("optionalNullable") val optionalNullable: JsonNullable<kotlin.String> = JsonNullable.undefined()
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

