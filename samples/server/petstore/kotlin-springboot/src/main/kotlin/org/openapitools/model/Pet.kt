package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.Category
import org.openapitools.model.Tag
import javax.validation.Valid
import javax.validation.constraints.*

/**
 * A pet for sale in the pet store
 * @param id 
 * @param category 
 * @param name 
 * @param photoUrls 
 * @param tags 
 * @param status pet status in the store
 */
data class Pet (
    @JsonProperty("name") val name: kotlin.String,
    @JsonProperty("photoUrls") val photoUrls: kotlin.Array<kotlin.String>,
    @JsonProperty("id") val id: kotlin.Long? = null,
    @JsonProperty("category") val category: Category? = null,
    @JsonProperty("tags") val tags: kotlin.Array<Tag>? = null,
    /* pet status in the store */
    @JsonProperty("status") val status: Pet.Status? = null
) {

    /**
    * pet status in the store
    * Values: available,pending,sold
    */
    enum class Status(val value: kotlin.String) {
    
        @JsonProperty("available") available("available"),
    
        @JsonProperty("pending") pending("pending"),
    
        @JsonProperty("sold") sold("sold");
    
    }

}

