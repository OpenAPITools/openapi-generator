package org.openapitools.configuration;

import org.openapitools.model.EnumClassDto;
import org.openapitools.model.OuterEnumDto;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;

@Configuration
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
