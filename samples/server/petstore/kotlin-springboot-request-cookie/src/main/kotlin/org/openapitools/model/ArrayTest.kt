package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import org.openapitools.model.ReadOnlyFirst
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
 * @param arrayOfString 
 * @param arrayArrayOfInteger 
 * @param arrayArrayOfModel 
 */
data class ArrayTest(

    @get:Size(min=0,max=3) 
    @Schema(example = "null", description = "")
    @get:JsonProperty("array_of_string") val arrayOfString: kotlin.collections.List<kotlin.String>? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("array_array_of_integer") val arrayArrayOfInteger: kotlin.collections.List<kotlin.collections.List<kotlin.Long>>? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("array_array_of_model") val arrayArrayOfModel: kotlin.collections.List<kotlin.collections.List<ReadOnlyFirst>>? = null
) {

}

