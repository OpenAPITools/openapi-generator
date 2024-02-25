package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.Category
import org.openapitools.model.Tag
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
 * @param name 
 * @param photoUrls 
 * @param id 
 * @param category 
 * @param tags 
 * @param status pet status in the store
 */
data class Pet(

    @Schema(example = "doggie", required = true, description = "")
    @get:JsonProperty("name", required = true) val name: kotlin.String,

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("photoUrls", required = true) val photoUrls: kotlin.collections.Set<kotlin.String>,

    @Schema(example = "null", description = "")
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("category") val category: Category? = null,

    @field:Valid
    @Schema(example = "null", description = "")
    @get:JsonProperty("tags") val tags: kotlin.collections.List<Tag>? = null,

    @Schema(example = "null", description = "pet status in the store")
    @get:JsonProperty("status") val status: Pet.Status? = null
) {

    /**
    * pet status in the store
    * Values: available,pending,sold
    */
    enum class Status(val value: kotlin.String) {

        @JsonProperty("available") available("available"),
        @JsonProperty("pending") pending("pending"),
        @JsonProperty("sold") sold("sold")
    }

}

