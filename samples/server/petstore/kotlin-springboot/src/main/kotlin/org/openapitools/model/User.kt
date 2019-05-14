package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonProperty
import javax.validation.constraints.DecimalMax
import javax.validation.constraints.DecimalMin
import javax.validation.constraints.Max
import javax.validation.constraints.Min
import javax.validation.constraints.NotNull
import javax.validation.constraints.Pattern
import javax.validation.constraints.Size
import io.swagger.annotations.ApiModelProperty

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
data class User (

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("id") val id: Long? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("username") val username: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("firstName") val firstName: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("lastName") val lastName: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("email") val email: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("password") val password: String? = null,

        @ApiModelProperty(example = "null", value = "")
        @JsonProperty("phone") val phone: String? = null,

        @ApiModelProperty(example = "null", value = "User Status")
        @JsonProperty("userStatus") val userStatus: Int? = null
) {

}

