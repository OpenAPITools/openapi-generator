package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
import org.openapitools.model.Pet
import org.openapitools.model.User
import org.openapitools.configuration.ApiResponse
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
 * A non-generic model whose userResult property references a generic instance. Tests property-level substitution: userResult type → ApiResponse<User> while pet (a plain domain type) is left unchanged. 
 * @param userResult 
 * @param pet 
 * @param orderId 
 */
data class OrderDetails(

    @field:Valid
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("userResult") val userResult: ApiResponse<User>? = null,

    @field:Valid
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("pet") val pet: Pet? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("orderId") val orderId: kotlin.String? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

