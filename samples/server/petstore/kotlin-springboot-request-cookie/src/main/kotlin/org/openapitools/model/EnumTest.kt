package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.OuterEnum
import org.openapitools.model.OuterEnumDefaultValue
import org.openapitools.model.OuterEnumInteger
import org.openapitools.model.OuterEnumIntegerDefaultValue
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
 * @param enumStringRequired 
 * @param enumString 
 * @param enumInteger 
 * @param enumNumber 
 * @param outerEnum 
 * @param outerEnumInteger 
 * @param outerEnumDefaultValue 
 * @param outerEnumIntegerDefaultValue 
 */
data class EnumTest(

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("enum_string_required", required = true) val enumStringRequired: EnumTest.EnumStringRequired,

    @Schema(example = "null", description = "")
    @get:JsonProperty("enum_string") val enumString: EnumTest.EnumString? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("enum_integer") val enumInteger: EnumTest.EnumInteger? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("enum_number") val enumNumber: EnumTest.EnumNumber? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("outerEnum") val outerEnum: OuterEnum? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("outerEnumInteger") val outerEnumInteger: OuterEnumInteger? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("outerEnumDefaultValue") val outerEnumDefaultValue: OuterEnumDefaultValue? = OuterEnumDefaultValue.placed,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("outerEnumIntegerDefaultValue") val outerEnumIntegerDefaultValue: OuterEnumIntegerDefaultValue? = OuterEnumIntegerDefaultValue._0
) {

    /**
    * 
    * Values: uPPER,lower,eMPTY
    */
    enum class EnumStringRequired(val value: kotlin.String) {

        @JsonProperty("UPPER") uPPER("UPPER"),
        @JsonProperty("lower") lower("lower"),
        @JsonProperty("") eMPTY("")
    }

    /**
    * 
    * Values: uPPER,lower,eMPTY
    */
    enum class EnumString(val value: kotlin.String) {

        @JsonProperty("UPPER") uPPER("UPPER"),
        @JsonProperty("lower") lower("lower"),
        @JsonProperty("") eMPTY("")
    }

    /**
    * 
    * Values: _1,minus1
    */
    enum class EnumInteger(val value: kotlin.Int) {

        @JsonProperty(1) _1(1),
        @JsonProperty(-1) minus1(-1)
    }

    /**
    * 
    * Values: _1period1,minus1Period2
    */
    enum class EnumNumber(val value: kotlin.Double) {

        @JsonProperty(1.1) _1period1(1.1),
        @JsonProperty(-1.2) minus1Period2(-1.2)
    }

}

