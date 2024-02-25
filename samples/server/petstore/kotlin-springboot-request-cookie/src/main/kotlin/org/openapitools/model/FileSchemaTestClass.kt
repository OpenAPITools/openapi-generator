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
 * @param file 
 * @param files 
 */
data class FileSchemaTestClass(

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("file") val file: java.io.File? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("files") val files: kotlin.collections.List<java.io.File>? = null
) {

}

