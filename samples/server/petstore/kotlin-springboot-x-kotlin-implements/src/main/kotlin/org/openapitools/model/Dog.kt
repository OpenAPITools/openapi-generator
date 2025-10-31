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
 * @param bark 
 * @param breed 
 * @param likesFetch Whether the dog enjoys fetching
 */
data class Dog(

    @get:JsonProperty("bark", required = true)
    val bark: kotlin.Boolean,

    @get:JsonProperty("breed", required = true)
    val breed: Dog.Breed,

    @get:JsonProperty("likesFetch", required = true)
    override val likesFetch: kotlin.Boolean,

    @get:JsonProperty("name", required = true)
    override val name: kotlin.String,

    @get:JsonProperty("photoUrls", required = true)
    override val photoUrls: kotlin.collections.List<kotlin.String>,

    @get:JsonProperty("petType", required = true)
    override val petType: kotlin.String,

    @get:JsonProperty("id", required = false)
    override val id: kotlin.Long? = null,

    @field:Valid
    @get:JsonProperty("category", required = false)
    override val category: Category? = null,

    @get:JsonProperty("nonRequiredWithDefaultList", required = false)
    override val nonRequiredWithDefaultList: kotlin.collections.List<kotlin.String> = arrayListOf("just some default string","another default string"),

    @get:JsonProperty("nonRequiredWithDefaultSet", required = false)
    override val nonRequiredWithDefaultSet: kotlin.collections.Set<kotlin.String> = setOf("more strings","look, it's a string!"),

    @get:JsonProperty("nonRequiredWithDefaultString", required = false)
    override val nonRequiredWithDefaultString: kotlin.String = "defaultValue",

    @get:JsonProperty("nonRequiredWithDefaultInt", required = false)
    override val nonRequiredWithDefaultInt: java.math.BigDecimal = java.math.BigDecimal("15"),

    @get:JsonProperty("nonRequiredWithDefaultLong", required = false)
    override val nonRequiredWithDefaultLong: java.math.BigDecimal = java.math.BigDecimal("15"),

    @get:JsonProperty("nonRequiredWithDefaultFloat", required = false)
    override val nonRequiredWithDefaultFloat: kotlin.Float = 15.45f,

    @get:JsonProperty("nonRequiredWithDefaultDouble", required = false)
    override val nonRequiredWithDefaultDouble: kotlin.Double = 15.45,

    @get:JsonProperty("nonRequiredWithDefaultEnum", required = false)
    override val nonRequiredWithDefaultEnum: Dog.NonRequiredWithDefaultEnum = NonRequiredWithDefaultEnum.THIS,

    @get:JsonProperty("nonRequiredWithDefaultEnumList", required = false)
    override val nonRequiredWithDefaultEnumList: kotlin.collections.List<Dog.NonRequiredWithDefaultEnumList> = arrayListOf(NonRequiredWithDefaultEnumList.THESE,NonRequiredWithDefaultEnumList.THOSE),

    @get:JsonProperty("nonRequiredWithDefaultEnumSet", required = false)
    override val nonRequiredWithDefaultEnumSet: kotlin.collections.Set<Dog.NonRequiredWithDefaultEnumSet> = setOf(NonRequiredWithDefaultEnumSet.THEM,NonRequiredWithDefaultEnumSet.THOSE),

    @field:Valid
    @get:JsonProperty("tags", required = false)
    override val tags: kotlin.collections.List<Tag>? = null,

    @field:Valid
    @get:JsonProperty("color", required = false)
    override val color: Color? = null
) : Pet, Serializable,  com.some.pack.Fetchable {

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
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'Dog'")
            }
        }
    }

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

