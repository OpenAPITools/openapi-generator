package org.openapitools.model

import java.util.Objects
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.annotation.JsonProperty
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
* additional field as Enum
* Values: ALLOWED,IN_PROGRESS,REJECTED
*/
enum class MultipartMixedStatus(val value: kotlin.String) {

    @JsonProperty("ALLOWED") ALLOWED("ALLOWED"),
    @JsonProperty("IN_PROGRESS") IN_PROGRESS("IN_PROGRESS"),
    @JsonProperty("REJECTED") REJECTED("REJECTED")
}

