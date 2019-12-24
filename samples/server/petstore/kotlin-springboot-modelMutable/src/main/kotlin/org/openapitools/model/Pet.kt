package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.Category
import org.openapitools.model.Tag
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import io.swagger.annotations.ApiModelProperty

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

        @get:NotNull 
        @ApiModelProperty(example = "doggie", required = true, value = "")
        @JsonProperty("name") var name: kotlin.String,

        @get:NotNull 
        @ApiModelProperty(example = "null", required = true, value = "")
        @JsonProperty("photoUrls") var photoUrls: kotlin.collections.List<kotlin.String>,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("id") var id: kotlin.Long? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("category") var category: Category? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("tags") var tags: kotlin.collections.List<Tag>? = null,

        @ApiModelProperty(example = "null", value = "pet status in the store")
        @JsonProperty("status") var status: Pet.Status? = null
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

