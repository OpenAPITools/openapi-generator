package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
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
 * @param hunts 
 * @param age 
 */
data class Cat(

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("name", required = true) override val name: kotlin.String,

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("photoUrls", required = true) override val photoUrls: kotlin.collections.List<kotlin.String>,

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("petType", required = true) override val petType: kotlin.String,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("hunts") val hunts: kotlin.Boolean? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("age") val age: kotlin.Int? = null,

    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("id") override val id: kotlin.Long? = null,

    @field:Valid
    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("category") override val category: Category? = null,

    @field:Valid
    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("tags") override val tags: kotlin.collections.List<Tag>? = null,

    @field:Valid
    @ApiModelProperty(example = "null", value = "")
    @get:JsonProperty("color") override val color: Color? = null
) : Pet, java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

