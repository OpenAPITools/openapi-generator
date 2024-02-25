package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.DeprecatedObject
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
 * @param id 
 * @param deprecatedRef 
 * @param bars 
 */
data class ObjectWithDeprecatedFields(

    @Schema(example = "null", description = "")
    @get:JsonProperty("uuid") val uuid: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @Deprecated(message = "")
    @get:JsonProperty("id") val id: java.math.BigDecimal? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @Deprecated(message = "")
    @get:JsonProperty("deprecatedRef") val deprecatedRef: DeprecatedObject? = null,

    @Schema(example = "null", description = "")
    @Deprecated(message = "")
    @get:JsonProperty("bars") val bars: kotlin.collections.List<kotlin.String>? = null
) {

}

