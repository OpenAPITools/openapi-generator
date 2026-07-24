package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.annotation.Nulls
import org.openapitools.model.Category
import org.openapitools.model.Color
import org.openapitools.model.Pet
import org.openapitools.model.Tag
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.annotations.ApiModelProperty

/**
 * 
 * @param bark 
 * @param breed 
 * @param likesFetch Whether the dog enjoys fetching
 * @param name 
 * @param photoUrls 
 * @param petType 
 * @param id 
 * @param category 
 * @param tags 
 * @param color 
 */
data class Dog(

    @ApiModelProperty(required = true, value = "")
    @param:JsonProperty("bark")
    @get:JsonProperty("bark", required = true) val bark: kotlin.Boolean,

    @ApiModelProperty(required = true, value = "")
    @param:JsonProperty("breed")
    @get:JsonProperty("breed", required = true) val breed: Dog.Breed,

    @ApiModelProperty(required = true, value = "Whether the dog enjoys fetching")
    @param:JsonProperty("likesFetch")
    @get:JsonProperty("likesFetch", required = true) override val likesFetch: kotlin.Boolean,

    @ApiModelProperty(required = true, value = "")
    @param:JsonProperty("name")
    @get:JsonProperty("name", required = true) override val name: kotlin.String,

    @ApiModelProperty(required = true, value = "")
    @param:JsonProperty("photoUrls")
    @get:JsonProperty("photoUrls", required = true) override val photoUrls: kotlin.collections.List<kotlin.String>,

    @ApiModelProperty(required = true, value = "")
    @param:JsonProperty("petType")
    @get:JsonProperty("petType", required = true) override val petType: kotlin.String,

    @ApiModelProperty(value = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("id")
    @get:JsonProperty("id") override val id: kotlin.Long? = null,

    @field:Valid
    @ApiModelProperty(value = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("category")
    @get:JsonProperty("category") override val category: Category? = null,

    @field:Valid
    @ApiModelProperty(value = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("tags")
    @get:JsonProperty("tags") override val tags: kotlin.collections.List<Tag>? = null,

    @field:Valid
    @ApiModelProperty(value = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("color")
    @get:JsonProperty("color") override val color: Color? = null
) : Pet, com.some.pack.Fetchable, java.io.Serializable {

    /**
    * 
    * Values: Dingo,Husky,Retriever,Shepherd
    */
    enum class Breed(@get:JsonValue val value: kotlin.String) {
    
        Dingo("Dingo"),
        Husky("Husky"),
        Retriever("Retriever"),
        Shepherd("Shepherd");
        
        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.String): Breed {
                return values().firstOrNull{ it.value == value }
                                ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Breed'")
            }
        }
    }

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

