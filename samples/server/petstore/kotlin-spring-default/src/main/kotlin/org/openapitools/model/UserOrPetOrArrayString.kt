package org.openapitools.model

import java.util.Locale
import java.util.Objects
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonValue
import org.openapitools.model.Category
import org.openapitools.model.Pet
import org.openapitools.model.Tag
import org.openapitools.model.User
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Email
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import javax.validation.Valid
import io.swagger.v3.oas.annotations.media.Schema

/**
 * 
 * @param username 
 * @param name 
 * @param photoUrls 
 * @param id 
 * @param firstName 
 * @param lastName 
 * @param email 
 * @param password 
 * @param phone 
 * @param userStatus User Status
 * @param category 
 * @param tags 
 * @param status pet status in the store
 */
data class UserOrPetOrArrayString(

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("username", required = true)
    val username: kotlin.String,

    @Schema(example = "doggie", required = true, description = "")
    @get:JsonProperty("name", required = true)
    val name: kotlin.String,

    @Schema(example = "null", required = true, description = "")
    @get:JsonProperty("photoUrls", required = true)
    val photoUrls: kotlin.collections.List<kotlin.String>,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("id", required = false)
    val id: kotlin.Long? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("firstName", required = false)
    val firstName: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("lastName", required = false)
    val lastName: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("email", required = false)
    val email: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("password", required = false)
    val password: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("phone", required = false)
    val phone: kotlin.String? = null,

    @Schema(example = "null", required = false, description = "User Status")
    @get:JsonProperty("userStatus", required = false)
    val userStatus: kotlin.Int? = null,

    @field:Valid
    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("category", required = false)
    val category: Category? = null,

    @field:Valid
    @Schema(example = "null", required = false, description = "")
    @get:JsonProperty("tags", required = false)
    val tags: kotlin.collections.List<Tag>? = null,

    @Schema(example = "null", required = false, description = "pet status in the store")
    @Deprecated(message = "")
    @get:JsonProperty("status", required = false)
    val status: UserOrPetOrArrayString.Status? = null
) {

    /**
    * pet status in the store
    * Values: available,pending,sold
    */
    enum class Status(@get:JsonValue val value: kotlin.String) {

        available("available"),
        pending("pending"),
        sold("sold");

        companion object {
            @JvmStatic
            @JsonCreator
            fun forValue(value: kotlin.String): Status {
                return values().firstOrNull{it -> it.value == value}
                    ?: throw IllegalArgumentException("Unexpected value '$value' for enum 'UserOrPetOrArrayString'")
            }
        }
    }

}

