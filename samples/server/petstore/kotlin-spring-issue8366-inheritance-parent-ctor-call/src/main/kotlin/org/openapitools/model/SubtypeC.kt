package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.DiscriminatingType
import org.openapitools.model.ParentSchema
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
 * @param subtypeAproperty 
 */
data class SubtypeC(

    @Schema(example = "null", description = "")
    @get:JsonProperty("subtypeAproperty") val subtypeAproperty: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("id") override val id: kotlin.String? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("type") override val type: DiscriminatingType? = null
) : ParentSchema {

}

