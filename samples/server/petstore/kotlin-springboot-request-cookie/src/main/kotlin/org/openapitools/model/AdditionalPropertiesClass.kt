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
 * @param mapProperty 
 * @param mapOfMapProperty 
 */
data class AdditionalPropertiesClass(

    @Schema(example = "null", description = "")
    @get:JsonProperty("map_property") val mapProperty: kotlin.collections.Map<kotlin.String, kotlin.String>? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("map_of_map_property") val mapOfMapProperty: kotlin.collections.Map<kotlin.String, kotlin.collections.Map<kotlin.String, kotlin.String>>? = null
) {

}

