package org.openapitools.client.infrastructure

import tools.jackson.databind.DeserializationFeature
import tools.jackson.databind.ObjectMapper
import tools.jackson.databind.cfg.DateTimeFeature
import tools.jackson.databind.cfg.EnumFeature
import com.fasterxml.jackson.annotation.JsonInclude
import tools.jackson.module.kotlin.jsonMapper
import tools.jackson.module.kotlin.kotlinModule

object Serializer {
    @JvmStatic
    val jacksonObjectMapper: ObjectMapper = jsonMapper {
        addModule(kotlinModule())
        changeDefaultPropertyInclusion { it.withValueInclusion(JsonInclude.Include.NON_ABSENT) }
        enable(EnumFeature.READ_UNKNOWN_ENUM_VALUES_USING_DEFAULT_VALUE)
        disable(DateTimeFeature.WRITE_DATES_AS_TIMESTAMPS)
        disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
    }
}
