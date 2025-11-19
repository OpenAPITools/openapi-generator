package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.Category
import org.openapitools.model.Color
import org.openapitools.model.Pet
import org.openapitools.model.SomeEnum
import org.openapitools.model.SomeNullableEnumWithNonNullDefault
import org.openapitools.model.SomeNullableEnumWithNullDefault
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
import io.swagger.annotations.ApiModelProperty

/**
 * 
 * @param bark 
 * @param breed 
 * @param likesFetch Whether the dog enjoys fetching
 */
data class Dog(

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("bark", required = true)
    val bark: kotlin.Boolean,

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("breed", required = true)
    val breed: Dog.Breed,

    @ApiModelProperty(example = "null", required = true, value = "Whether the dog enjoys fetching")
    @get:JsonProperty("likesFetch", required = true)
    override val likesFetch: kotlin.Boolean,

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("name", required = true)
    override val name: kotlin.String,

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("photoUrls", required = true)
    override val photoUrls: kotlin.collections.List<kotlin.String>,

    @ApiModelProperty(example = "null", required = true, value = "")
    @get:JsonProperty("petType", required = true)
    override val petType: kotlin.String,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("id", required = false)
    override val id: kotlin.Long? = null,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("category", required = false)
    override val category: Category? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultList", required = false)
    override val nonRequiredWithDefaultList: kotlin.collections.List<kotlin.String> = arrayListOf("just some default string","another default string"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultSet", required = false)
    override val nonRequiredWithDefaultSet: kotlin.collections.Set<kotlin.String> = setOf("more strings","look, it's a string!"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultString", required = false)
    override val nonRequiredWithDefaultString: kotlin.String = "defaultValue",

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultInt", required = false)
    override val nonRequiredWithDefaultInt: java.math.BigDecimal = java.math.BigDecimal("15"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultLong", required = false)
    override val nonRequiredWithDefaultLong: java.math.BigDecimal = java.math.BigDecimal("15"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultFloat", required = false)
    override val nonRequiredWithDefaultFloat: kotlin.Float = 15.45f,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultDouble", required = false)
    override val nonRequiredWithDefaultDouble: kotlin.Double = 15.45,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultEnum", required = false)
    override val nonRequiredWithDefaultEnum: SomeEnum = SomeEnum.ENUMVALUE1,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultEnumList", required = false)
    override val nonRequiredWithDefaultEnumList: kotlin.collections.List<SomeEnum> = arrayListOf(SomeEnum.ENUMVALUE3,SomeEnum.ENUMVALUE1),

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredWithDefaultEnumSet", required = false)
    override val nonRequiredWithDefaultEnumSet: kotlin.collections.Set<SomeEnum> = setOf(SomeEnum.ENUMVALUE3,SomeEnum.ENUMVALUE1),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullList", required = false)
    override val nonRequiredNullableWithDefaultNullList: kotlin.collections.List<kotlin.String>? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullSet", required = false)
    override val nonRequiredNullableWithDefaultNullSet: kotlin.collections.Set<kotlin.String>? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullString", required = false)
    override val nonRequiredNullableWithDefaultNullString: kotlin.String? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullInt", required = false)
    override val nonRequiredNullableWithDefaultNullInt: java.math.BigDecimal? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullLong", required = false)
    override val nonRequiredNullableWithDefaultNullLong: java.math.BigDecimal? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullFloat", required = false)
    override val nonRequiredNullableWithDefaultNullFloat: kotlin.Float? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullDouble", required = false)
    override val nonRequiredNullableWithDefaultNullDouble: kotlin.Double? = null,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullEnum", required = false)
    override val nonRequiredNullableWithDefaultNullEnum: SomeNullableEnumWithNullDefault? = null,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullEnumList", required = false)
    override val nonRequiredNullableWithDefaultNullEnumList: kotlin.collections.List<SomeEnum>? = null,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNullEnumSet", required = false)
    override val nonRequiredNullableWithDefaultNullEnumSet: kotlin.collections.Set<SomeEnum>? = null,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullList", required = false)
    override val nonRequiredNullableWithDefaultNonNullList: kotlin.collections.List<kotlin.String>? = arrayListOf("some string","another string"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullSet", required = false)
    override val nonRequiredNullableWithDefaultNonNullSet: kotlin.collections.Set<kotlin.String>? = setOf("some string","another string"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullString", required = false)
    override val nonRequiredNullableWithDefaultNonNullString: kotlin.String? = "some string",

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullInt", required = false)
    override val nonRequiredNullableWithDefaultNonNullInt: java.math.BigDecimal? = java.math.BigDecimal("42"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullLong", required = false)
    override val nonRequiredNullableWithDefaultNonNullLong: java.math.BigDecimal? = java.math.BigDecimal("42"),

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullFloat", required = false)
    override val nonRequiredNullableWithDefaultNonNullFloat: kotlin.Float? = 15.45f,

    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullDouble", required = false)
    override val nonRequiredNullableWithDefaultNonNullDouble: kotlin.Double? = 15.45,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullEnum", required = false)
    override val nonRequiredNullableWithDefaultNonNullEnum: SomeNullableEnumWithNonNullDefault? = SomeNullableEnumWithNonNullDefault.ENUMVALUE1,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullEnumList", required = false)
    override val nonRequiredNullableWithDefaultNonNullEnumList: kotlin.collections.List<SomeEnum>? = arrayListOf(SomeEnum.ENUMVALUE1),

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullEnumSet", required = false)
    override val nonRequiredNullableWithDefaultNonNullEnumSet: kotlin.collections.Set<SomeEnum>? = setOf(SomeEnum.ENUMVALUE1),

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
    @get:JsonProperty("tags", required = false)
    override val tags: kotlin.collections.List<Tag>? = null,

    @field:Valid
    @ApiModelProperty(example = "null", required = false, value = "")
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

