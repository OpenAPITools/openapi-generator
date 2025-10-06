package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * 
 * @param bepa 
 * @param cepa 
 * @param depa 
 * @param epa 
 * @param fepa 
 * @param gepa 
 */
data class Apa(

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("bepa", required = true) val bepa: java.math.BigDecimal = java.math.BigDecimal("0"),

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("cepa", required = true) val cepa: java.math.BigDecimal = java.math.BigDecimal("6.28318"),

    @Schema(example = "null", description = "")
    @get:JsonProperty("depa") val depa: java.math.BigDecimal? = java.math.BigDecimal("71"),

    @Schema(example = "null", description = "")
    @get:JsonProperty("epa") val epa: java.math.BigDecimal? = java.math.BigDecimal("-71"),

    @Schema(example = "null", description = "")
    @Deprecated(message = "")
    @get:JsonProperty("fepa") val fepa: java.math.BigDecimal? = java.math.BigDecimal("100"),

    @Schema(example = "null", description = "")
    @get:JsonProperty("gepa") val gepa: java.math.BigDecimal? = null
) {

}

