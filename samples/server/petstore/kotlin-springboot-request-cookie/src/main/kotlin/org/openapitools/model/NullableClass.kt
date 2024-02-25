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
 * @param integerProp 
 * @param numberProp 
 * @param booleanProp 
 * @param stringProp 
 * @param dateProp 
 * @param datetimeProp 
 * @param arrayNullableProp 
 * @param arrayAndItemsNullableProp 
 * @param arrayItemsNullable 
 * @param objectNullableProp 
 * @param objectAndItemsNullableProp 
 * @param objectItemsNullable 
 */
data class NullableClass(

    @Schema(example = "null", description = "")
    @get:JsonProperty("integer_prop") val integerProp: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("number_prop") val numberProp: java.math.BigDecimal? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("boolean_prop") val booleanProp: kotlin.Boolean? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("string_prop") val stringProp: kotlin.String? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("date_prop") val dateProp: java.time.LocalDate? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("datetime_prop") val datetimeProp: java.time.OffsetDateTime? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("array_nullable_prop") val arrayNullableProp: kotlin.collections.List<kotlin.Any>? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("array_and_items_nullable_prop") val arrayAndItemsNullableProp: kotlin.collections.List<kotlin.Any>? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("array_items_nullable") val arrayItemsNullable: kotlin.collections.List<kotlin.Any>? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("object_nullable_prop") val objectNullableProp: kotlin.collections.Map<kotlin.String, kotlin.Any>? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("object_and_items_nullable_prop") val objectAndItemsNullableProp: kotlin.collections.Map<kotlin.String, kotlin.Any>? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("object_items_nullable") val objectItemsNullable: kotlin.collections.Map<kotlin.String, kotlin.Any>? = null
) : kotlin.collections.HashMap<String, kotlin.Any>{

}

