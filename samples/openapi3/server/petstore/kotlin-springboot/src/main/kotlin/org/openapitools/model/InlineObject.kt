package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size

/**
 * 
 * @param name Updated name of the pet
 * @param status Updated status of the pet
 */
data class InlineObject(

    @JsonProperty("name") val name: kotlin.String? = null,

    @JsonProperty("status") val status: kotlin.String? = null
) {

}

