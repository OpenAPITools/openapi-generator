package org.openapitools.configuration

import org.openapitools.model.ReasonCode

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.core.convert.converter.Converter

@Configuration(value = "org.openapitools.configuration.enumConverterConfiguration")
class EnumConverterConfiguration {

    @Bean(name = ["org.openapitools.configuration.EnumConverterConfiguration.reasonCodeConverter"])
    fun reasonCodeConverter(): Converter<kotlin.Int, ReasonCode> {
        return object: Converter<kotlin.Int, ReasonCode> {
            override fun convert(source: kotlin.Int): ReasonCode = ReasonCode.forValue(source)
        }
    }

}
