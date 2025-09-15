package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.Category
import org.openapitools.model.Color
import org.openapitools.model.Pet
import org.openapitools.model.Tag
import java.io.Serializable
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid

/**
 * 
 * @param hunts 
 * @param age 
 */
data class Cat(

    @get:JsonProperty("name", required = true) override val name: kotlin.String,

    @get:JsonProperty("photoUrls", required = true) override val photoUrls: kotlin.collections.List<kotlin.String>,

    @get:JsonProperty("petType", required = true) override val petType: kotlin.String,

    @get:JsonProperty("hunts") val hunts: kotlin.Boolean? = null,

    @get:JsonProperty("age") val age: kotlin.Int? = null,

    @get:JsonProperty("id") override val id: kotlin.Long? = null,

    @field:Valid
    @get:JsonProperty("category") override val category: Category? = null,

    @field:Valid
    @get:JsonProperty("tags") override val tags: kotlin.collections.List<Tag>? = null,

    @field:Valid
    @get:JsonProperty("color") override val color: Color? = null
) : Pet, Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

