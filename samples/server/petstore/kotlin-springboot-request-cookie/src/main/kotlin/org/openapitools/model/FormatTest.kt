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
 * @param number 
 * @param byte 
 * @param date 
 * @param password 
 * @param integer 
 * @param int32 
 * @param int64 
 * @param float 
 * @param double 
 * @param decimal 
 * @param string 
 * @param binary 
 * @param dateTime 
 * @param uuid 
 * @param patternWithDigits A string that is a 10 digit number. Can have leading zeros.
 * @param patternWithDigitsAndDelimiter A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
 */
data class FormatTest(

    @get:DecimalMin("32.1")
    @get:DecimalMax("543.2")
    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("number", required = true) val number: java.math.BigDecimal,

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("byte", required = true) val byte: kotlin.ByteArray,

    @field:Valid
    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("date", required = true) val date: java.time.LocalDate,

    @get:Size(min=10,max=64)
    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("password", required = true) val password: kotlin.String,

    @get:Min(10)
    @get:Max(100)
    @Schema(example = "null", description = "")
    @get:JsonProperty("integer") val integer: kotlin.Int? = null,

    @get:Min(20)
    @get:Max(200)
    @Schema(example = "null", description = "")
    @get:JsonProperty("int32") val int32: kotlin.Int? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("int64") val int64: kotlin.Long? = null,

    @get:DecimalMin("54.3")
    @get:DecimalMax("987.6")
    @Schema(example = "null", description = "")
    @get:JsonProperty("float") val float: kotlin.Float? = null,

    @get:DecimalMin("67.8")
    @get:DecimalMax("123.4")
    @Schema(example = "null", description = "")
    @get:JsonProperty("double") val double: kotlin.Double? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("decimal") val decimal: java.math.BigDecimal? = null,

    @get:Pattern(regexp="/[a-z]/i")
    @Schema(example = "null", description = "")
    @get:JsonProperty("string") val string: kotlin.String? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("binary") val binary: org.springframework.core.io.Resource? = null,

    @Schema(example = "null", description = "")
    @get:JsonProperty("dateTime") val dateTime: java.time.OffsetDateTime? = null,

    @Schema(example = "72f98069-206d-4f12-9f12-3d1e525a8e84", description = "")
    @get:JsonProperty("uuid") val uuid: java.util.UUID? = null,

    @get:Pattern(regexp="^\\d{10}$")
    @Schema(example = "null", description = "A string that is a 10 digit number. Can have leading zeros.")
    @get:JsonProperty("pattern_with_digits") val patternWithDigits: kotlin.String? = null,

    @get:Pattern(regexp="/^image_\\d{1,3}$/i")
    @Schema(example = "null", description = "A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.")
    @get:JsonProperty("pattern_with_digits_and_delimiter") val patternWithDigitsAndDelimiter: kotlin.String? = null
) {

}

