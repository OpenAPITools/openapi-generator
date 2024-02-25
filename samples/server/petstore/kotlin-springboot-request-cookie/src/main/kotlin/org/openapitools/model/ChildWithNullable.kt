package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.ParentWithNullable
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
 * @param otherProperty 
 */
data class ChildWithNullable(

    @Schema(example = "null", description = "")
    @get:JsonProperty("otherProperty") val otherProperty: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("type") override val type: ChildWithNullable.Type? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("nullableProperty") override val nullableProperty: kotlin.String? = null
) : ParentWithNullable{

}

