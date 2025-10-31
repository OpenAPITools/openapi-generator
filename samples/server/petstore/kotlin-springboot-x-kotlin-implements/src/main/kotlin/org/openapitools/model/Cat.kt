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

/**
 * 
 * @param hunts 
 * @param age 
 */
data class Cat(

    @get:JsonProperty("name", required = true)
    override val name: kotlin.String,

    @get:JsonProperty("photoUrls", required = true)
    override val photoUrls: kotlin.collections.List<kotlin.String>,

    @get:JsonProperty("petType", required = true)
    override val petType: kotlin.String,

    @get:JsonProperty("hunts", required = false)
    val hunts: kotlin.Boolean? = null,

    @get:JsonProperty("age", required = false)
    val age: kotlin.Int? = null,

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

    @field:Valid
    @get:JsonProperty("nonRequiredWithDefaultEnum", required = false)
    override val nonRequiredWithDefaultEnum: SomeEnum = SomeEnum.ENUMVALUE1,

    @field:Valid
    @get:JsonProperty("nonRequiredWithDefaultEnumList", required = false)
    override val nonRequiredWithDefaultEnumList: kotlin.collections.List<SomeEnum> = arrayListOf(SomeEnum.ENUMVALUE3,SomeEnum.ENUMVALUE1),

    @field:Valid
    @get:JsonProperty("nonRequiredWithDefaultEnumSet", required = false)
    override val nonRequiredWithDefaultEnumSet: kotlin.collections.Set<SomeEnum> = setOf(SomeEnum.ENUMVALUE3,SomeEnum.ENUMVALUE1),

    @get:JsonProperty("nonRequiredNullableWithDefaultNullList", required = false)
    override val nonRequiredNullableWithDefaultNullList: kotlin.collections.List<kotlin.String>? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNullSet", required = false)
    override val nonRequiredNullableWithDefaultNullSet: kotlin.collections.Set<kotlin.String>? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNullString", required = false)
    override val nonRequiredNullableWithDefaultNullString: kotlin.String? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNullInt", required = false)
    override val nonRequiredNullableWithDefaultNullInt: java.math.BigDecimal? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNullLong", required = false)
    override val nonRequiredNullableWithDefaultNullLong: java.math.BigDecimal? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNullFloat", required = false)
    override val nonRequiredNullableWithDefaultNullFloat: kotlin.Float? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNullDouble", required = false)
    override val nonRequiredNullableWithDefaultNullDouble: kotlin.Double? = null,

    @field:Valid
    @get:JsonProperty("nonRequiredNullableWithDefaultNullEnum", required = false)
    override val nonRequiredNullableWithDefaultNullEnum: SomeNullableEnumWithNullDefault? = null,

    @field:Valid
    @get:JsonProperty("nonRequiredNullableWithDefaultNullEnumList", required = false)
    override val nonRequiredNullableWithDefaultNullEnumList: kotlin.collections.List<SomeEnum>? = null,

    @field:Valid
    @get:JsonProperty("nonRequiredNullableWithDefaultNullEnumSet", required = false)
    override val nonRequiredNullableWithDefaultNullEnumSet: kotlin.collections.Set<SomeEnum>? = null,

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullList", required = false)
    override val nonRequiredNullableWithDefaultNonNullList: kotlin.collections.List<kotlin.String>? = arrayListOf("some string","another string"),

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullSet", required = false)
    override val nonRequiredNullableWithDefaultNonNullSet: kotlin.collections.Set<kotlin.String>? = setOf("some string","another string"),

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullString", required = false)
    override val nonRequiredNullableWithDefaultNonNullString: kotlin.String? = "some string",

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullInt", required = false)
    override val nonRequiredNullableWithDefaultNonNullInt: java.math.BigDecimal? = java.math.BigDecimal("42"),

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullLong", required = false)
    override val nonRequiredNullableWithDefaultNonNullLong: java.math.BigDecimal? = java.math.BigDecimal("42"),

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullFloat", required = false)
    override val nonRequiredNullableWithDefaultNonNullFloat: kotlin.Float? = 15.45f,

    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullDouble", required = false)
    override val nonRequiredNullableWithDefaultNonNullDouble: kotlin.Double? = 15.45,

    @field:Valid
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullEnum", required = false)
    override val nonRequiredNullableWithDefaultNonNullEnum: SomeNullableEnumWithNonNullDefault? = SomeNullableEnumWithNonNullDefault.ENUMVALUE1,

    @field:Valid
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullEnumList", required = false)
    override val nonRequiredNullableWithDefaultNonNullEnumList: kotlin.collections.List<SomeEnum>? = arrayListOf(SomeEnum.ENUMVALUE1),

    @field:Valid
    @get:JsonProperty("nonRequiredNullableWithDefaultNonNullEnumSet", required = false)
    override val nonRequiredNullableWithDefaultNonNullEnumSet: kotlin.collections.Set<SomeEnum>? = setOf(SomeEnum.ENUMVALUE1),

    @field:Valid
    @get:JsonProperty("tags", required = false)
    override val tags: kotlin.collections.List<Tag>? = null,

    @field:Valid
    @get:JsonProperty("color", required = false)
    override val color: Color? = null
) : Pet, Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

