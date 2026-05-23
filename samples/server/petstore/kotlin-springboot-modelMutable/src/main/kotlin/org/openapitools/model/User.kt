package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.Nulls
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
 * A User who is purchasing from the pet store
 * @param id 
 * @param username 
 * @param firstName 
 * @param lastName 
 * @param email 
 * @param password 
 * @param phone 
 * @param userStatus User Status
 */
data class User(

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("id") var id: kotlin.Long? = null,

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("username") var username: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("firstName") var firstName: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("lastName") var lastName: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("email") var email: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("password") var password: kotlin.String? = null,

    @Schema(example = "null", description = "")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("phone") var phone: kotlin.String? = null,

    @Schema(example = "null", description = "User Status")
    @field:JsonSetter(nulls = Nulls.FAIL)
    @get:JsonProperty("userStatus") var userStatus: kotlin.Int? = null
) : java.io.Serializable {

    companion object {
        private const val serialVersionUID: kotlin.Long = 1
    }
}

