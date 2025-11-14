package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import java.io.Serializable
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.annotations.ApiModelProperty

/**
 * A category for a pet
 * @param id 
 * @param name 
 */
data class Category(

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("id", required = false)
    val id: kotlin.Long? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("name", required = false)
    val name: kotlin.String? = null
) : Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

