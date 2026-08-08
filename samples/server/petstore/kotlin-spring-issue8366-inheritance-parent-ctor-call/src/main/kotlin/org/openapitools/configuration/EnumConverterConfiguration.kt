package org.openapitools.configuration

import org.openapitools.model.DiscriminatingType

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.core.convert.converter.Converter

/**
 * This class provides Spring Converter beans for the enum models in the OpenAPI specification.
 *
 * By default, Spring only converts primitive types to enums using Enum::valueOf, which can prevent
 * correct conversion if the OpenAPI specification is using an `enumPropertyNaming` other than
 * `original` or the specification has an integer enum.
 */
@Configuration(value = "org.openapitools.configuration.enumConverterConfiguration")
class EnumConverterConfiguration {

    @Bean(name = ["org.openapitools.configuration.EnumConverterConfiguration.discriminatingTypeConverter"])
    fun discriminatingTypeConverter(): Converter<kotlin.String, DiscriminatingType> {
        return object: Converter<kotlin.String, DiscriminatingType> {
            override fun convert(source: kotlin.String): DiscriminatingType = DiscriminatingType.forValue(source)
        }
    }

}
