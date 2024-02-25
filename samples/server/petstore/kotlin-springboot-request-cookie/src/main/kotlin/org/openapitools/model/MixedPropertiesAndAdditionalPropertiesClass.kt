package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.Animal
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
 * @param uuid 
 * @param dateTime 
 * @param map 
 */
data class MixedPropertiesAndAdditionalPropertiesClass(

    @Schema(example = "null", description = "")
    @get:JsonProperty("uuid") val uuid: java.util.UUID? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("dateTime") val dateTime: java.time.OffsetDateTime? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("map") val map: kotlin.collections.Map<kotlin.String, Animal>? = null
) {

}

