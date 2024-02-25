package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.annotation.JsonValue
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size
import jakarta.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * 
 * @param type 
 * @param nullableProperty 
 */

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type", visible = true)
@JsonSubTypes(
      JsonSubTypes.Type(value = ChildWithNullable::class, name = "ChildWithNullable")
)

interface ParentWithNullable{
                @get:Schema(example = "null", description = "")
        val type: ParentWithNullable.Type? 

                @get:Schema(example = "null", description = "")
        val nullableProperty: kotlin.String? 


    /**
    * 
    * Values: childWithNullable
    */
    enum class Type(val value: kotlin.String) {

        @JsonProperty("ChildWithNullable") childWithNullable("ChildWithNullable")
    }

}

