package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
import org.openapitools.model.Animal
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
 * 
 * @param className 
 * @param breed 
 * @param color 
 */
open class Dog(

    @get:JsonProperty("className", required = true) override val className: kotlin.String = "Dog",

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("breed") open val breed: kotlin.String? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("color") override val color: kotlin.String? = "red"
) : Animal {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other?.javaClass != javaClass) return false
        other as Dog
        return className == other.className
                && color == other.color
                && breed == other.breed
    }

    override fun hashCode(): Int {
        return Objects.hash(className, color, breed)
    }

    override fun toString(): String {
        return "Dog(className=$className, color=$color, breed=$breed)"
    }

    fun copy(
        className: kotlin.String = this.className,
        color: kotlin.String? = this.color,
        breed: kotlin.String? = this.breed
    ): Dog = Dog(className = className, color = color, breed = breed)

}

