package org.openapitools.configuration;

import org.openapitools.model.EnumClassDto;
import org.openapitools.model.OuterEnumDto;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;

/**
 * This class provides Spring Converter beans for the enum models in the OpenAPI specification.
 *
 * By default, Spring only converts primitive types to enums using Enum::valueOf, which can prevent
 * correct conversion if the OpenAPI specification is using an `enumPropertyNaming` other than
 * `original` or the specification has an integer enum.
 */
@Configuration(value = "org.openapitools.configuration.enumConverterConfiguration")
public class EnumConverterConfiguration {

    @Bean(name = "org.openapitools.configuration.EnumConverterConfiguration.enumClassConverter")
    Converter<String, EnumClassDto> enumClassConverter() {
        return new Converter<String, EnumClassDto>() {
            @Override
            public EnumClassDto convert(String source) {
                return EnumClassDto.fromValue(source);
            }
        };
    }
    @Bean(name = "org.openapitools.configuration.EnumConverterConfiguration.outerEnumConverter")
    Converter<String, OuterEnumDto> outerEnumConverter() {
        return new Converter<String, OuterEnumDto>() {
            @Override
            public OuterEnumDto convert(String source) {
                return OuterEnumDto.fromValue(source);
            }
        };
    }

}
