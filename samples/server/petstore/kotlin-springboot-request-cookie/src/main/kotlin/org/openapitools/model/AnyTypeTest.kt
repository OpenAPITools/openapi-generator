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
 * @param anyTypeProperty 
 * @param arrayProp test array in 3.1 spec
 */
data class AnyTypeTest(

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("any_type_property") val anyTypeProperty: kotlin.Any? = null,

    @Schema(example = "null", description = "test array in 3.1 spec")
    @get:JsonProperty("array_prop") val arrayProp: kotlin.collections.List<kotlin.String>? = null
) {

}

