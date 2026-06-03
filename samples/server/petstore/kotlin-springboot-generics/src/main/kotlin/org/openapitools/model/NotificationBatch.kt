package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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
 * A non-generic model with an array property of a generic-instance type. Tests array property substitution: responses type → List<ApiResponse<User>> 
 * @param responses 
 * @param batchId 
 */
data class NotificationBatch(

    @field:Valid
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("responses") val responses: kotlin.collections.List<ApiResponse<User>>? = null,

    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("batchId") val batchId: kotlin.String? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

