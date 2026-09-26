package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
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

    @param:JsonProperty("className", required = true)
    @get:JsonProperty("className", required = true) override val className: kotlin.String,

    @param:JsonProperty("breed")
    @get:JsonProperty("breed") open val breed: kotlin.String? = null,

    @param:JsonProperty("color")
    @get:JsonProperty("color") override val color: kotlin.String? = "red"
) : Animal {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other?.javaClass != javaClass) return false
        other as Dog
        return className == other.className
                && breed == other.breed
                && color == other.color
    }

    override fun hashCode(): Int {
        return Objects.hash(className, breed, color)
    }

    override fun toString(): String {
        return "Dog(" +
            "className=" + className + ", " +
            "breed=" + breed + ", " +
            "color=" + color +
            ")"
    }

    fun copy(
        className: kotlin.String = this.className,
        breed: kotlin.String? = this.breed,
        color: kotlin.String? = this.color
    ): Dog = Dog(className = className, breed = breed, color = color)

}

