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
 * @param smallCamel 
 * @param capitalCamel 
 * @param smallSnake 
 * @param capitalSnake 
 * @param scAETHFlowPoints 
 * @param ATT_NAME Name of the pet 
 */
data class Capitalization(

    @Schema(example = "null", description = "")
    @get:JsonProperty("smallCamel") val smallCamel: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("CapitalCamel") val capitalCamel: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("small_Snake") val smallSnake: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("Capital_Snake") val capitalSnake: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("SCA_ETH_Flow_Points") val scAETHFlowPoints: kotlin.String? = null,

    @Schema(example = "null", description = "Name of the pet ")
    @get:JsonProperty("ATT_NAME") val ATT_NAME: kotlin.String? = null
) {

}

