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
 * @param name 
 * @param photoUrls 
 * @param petType 
 * @param hunts 
 * @param age 
 * @param id 
 * @param category 
 * @param tags 
 * @param color 
 */
data class Cat(

    @ApiModelProperty(required = true, value = "")
    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("name")
    @get:JsonProperty("name", required = true) override val name: kotlin.String,

    @ApiModelProperty(required = true, value = "")
    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("photoUrls")
    @get:JsonProperty("photoUrls", required = true) override val photoUrls: kotlin.collections.List<kotlin.String>,

    @ApiModelProperty(required = true, value = "")
    @field:JsonInclude(JsonInclude.Include.ALWAYS)
    @param:JsonProperty("petType")
    @get:JsonProperty("petType", required = true) override val petType: kotlin.String,

    @ApiModelProperty(value = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("hunts")
    @get:JsonProperty("hunts") val hunts: kotlin.Boolean? = null,

    @ApiModelProperty(value = "")
    @field:JsonInclude(JsonInclude.Include.NON_NULL)
    @field:JsonSetter(nulls = Nulls.SKIP)
    @param:JsonProperty("age")
    @get:JsonProperty("age") val age: kotlin.Int? = null,

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
) : Pet, java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

