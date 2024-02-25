package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * 
 * @param myNumber 
 * @param myString 
 * @param myBoolean 
 */
data class OuterComposite(

    @Schema(example = "null", description = "")
    @get:JsonProperty("my_number") val myNumber: java.math.BigDecimal? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("my_string") val myString: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("my_boolean") val myBoolean: kotlin.Boolean? = null
) {

}

