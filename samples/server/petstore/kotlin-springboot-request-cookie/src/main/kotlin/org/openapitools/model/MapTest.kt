package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
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
 * @param mapMapOfString 
 * @param mapOfEnumString 
 * @param directMap 
 * @param indirectMap 
 */
data class MapTest(

    @Schema(example = "null", description = "")
    @get:JsonProperty("map_map_of_string") val mapMapOfString: kotlin.collections.Map<kotlin.String, kotlin.collections.Map<kotlin.String, kotlin.String>>? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("map_of_enum_string") val mapOfEnumString: MapTest.MapOfEnumString? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("direct_map") val directMap: kotlin.collections.Map<kotlin.String, kotlin.Boolean>? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("indirect_map") val indirectMap: kotlin.collections.Map<kotlin.String, kotlin.Boolean>? = null
) {

    /**
    * 
    * Values: uPPER,lower
    */
    enum class MapOfEnumString(val value: kotlin.String) {

        @JsonProperty("UPPER") uPPER("UPPER"),
        @JsonProperty("lower") lower("lower")
    }

}

