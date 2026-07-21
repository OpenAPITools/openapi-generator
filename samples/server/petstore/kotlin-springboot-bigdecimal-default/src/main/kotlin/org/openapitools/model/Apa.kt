package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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

    @Schema(required = true, description = "")
    @param:JsonProperty("bepa")
    @get:JsonProperty("bepa", required = true) val bepa: java.math.BigDecimal = java.math.BigDecimal("0"),

    @Schema(required = true, description = "")
    @param:JsonProperty("cepa")
    @get:JsonProperty("cepa", required = true) val cepa: java.math.BigDecimal = java.math.BigDecimal("6.28318"),

    @Schema(description = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("depa")
    @get:JsonProperty("depa") val depa: java.math.BigDecimal? = java.math.BigDecimal("71"),

    @Schema(description = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("epa")
    @get:JsonProperty("epa") val epa: java.math.BigDecimal? = java.math.BigDecimal("-71"),

    @Schema(description = "")
    @Deprecated(message = "")
    @param:JsonProperty("fepa")
    @get:JsonProperty("fepa") val fepa: java.math.BigDecimal? = java.math.BigDecimal("100"),

    @Schema(description = "")
    @param:JsonProperty("gepa")
    @get:JsonProperty("gepa") val gepa: java.math.BigDecimal? = null
) {

}

