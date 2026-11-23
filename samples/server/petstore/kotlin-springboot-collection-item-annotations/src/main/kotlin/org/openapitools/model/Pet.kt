package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
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

/**
 * A pet for sale in the pet store
 * @param name 
 * @param photoUrls 
 * @param id 
 * @param category 
 * @param tags 
 * @param ratings 
 * @param weights 
 * @param nicknames 
 * @param labels 
 * @param status pet status in the store
 */
data class Pet(

    @param:JsonProperty("name", required = true)
    @get:JsonProperty("name", required = true) val name: kotlin.String,

    @param:JsonProperty("photoUrls", required = true)
    @get:JsonProperty("photoUrls", required = true) val photoUrls: kotlin.collections.List<@jakarta.validation.constraints.NotBlank @Size(min=1,max=255) kotlin.String>,

    @param:JsonProperty("id")
    @get:JsonProperty("id") val id: kotlin.Long? = null,

    @field:Valid
    @param:JsonProperty("category")
    @get:JsonProperty("category") val category: Category? = null,

    @field:Valid
    @param:JsonProperty("tags")
    @get:JsonProperty("tags") val tags: kotlin.collections.List<@Valid Tag>? = null,

    @param:JsonProperty("ratings")
    @get:JsonProperty("ratings") val ratings: kotlin.collections.List<@Min(value=1) @Max(value=5) kotlin.Int>? = null,

    @field:Valid
    @param:JsonProperty("weights")
    @get:JsonProperty("weights") val weights: kotlin.collections.Set<@DecimalMin(value="0", inclusive=false) java.math.BigDecimal>? = null,

    @param:JsonProperty("nicknames")
    @get:JsonProperty("nicknames") val nicknames: kotlin.collections.List<kotlin.collections.List<@Size(max=10) kotlin.String?>>? = null,

    @param:JsonProperty("labels")
    @get:JsonProperty("labels") val labels: kotlin.collections.List<Pet.Labels>? = null,

    @Deprecated(message = "")
    @param:JsonProperty("status")
    @get:JsonProperty("status") val status: Pet.Status? = null
) {

    /**
    * 
    * Values: cute,fluffy
    */
    enum class Labels(@get:JsonValue val value: kotlin.String) {

        cute("cute"),
        fluffy("fluffy");
    
        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.String): Labels {
                return values().firstOrNull{ it.value == value }
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Labels'")
            }
        }
    }

    /**
    * pet status in the store
    * Values: available,pending,sold
    */
    enum class Status(@get:JsonValue val value: kotlin.String) {

        available("available"),
        pending("pending"),
        sold("sold");
    
        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.String): Status {
                return values().firstOrNull{ it.value == value }
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Status'")
            }
        }
    }

}

