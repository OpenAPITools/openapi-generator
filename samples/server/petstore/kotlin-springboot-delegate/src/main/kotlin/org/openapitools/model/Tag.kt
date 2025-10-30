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
 * A tag for a pet
 * @param id 
 * @param name 
 */
data class Tag(

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("id", required = false)
    val id: kotlin.Long? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("name", required = false)
    val name: kotlin.String? = null
) {

}

