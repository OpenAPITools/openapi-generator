package org.openapitools.configuration

import org.openapitools.model.MultipartMixedStatus

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.core.convert.converter.Converter

@Configuration(value = "org.openapitools.configuration.enumConverterConfiguration")
class EnumConverterConfiguration {

    @Bean(name = ["org.openapitools.configuration.EnumConverterConfiguration.multipartMixedStatusConverter"])
    fun multipartMixedStatusConverter(): Converter<kotlin.String, MultipartMixedStatus> {
        return object: Converter<kotlin.String, MultipartMixedStatus> {
            override fun convert(source: kotlin.String): MultipartMixedStatus = MultipartMixedStatus.forValue(source)
        }
    }

}
