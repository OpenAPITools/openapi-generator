package org.openapitools.configuration;

import org.openapitools.virtualan.model.EnumClass;
import org.openapitools.virtualan.model.OuterEnum;

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
    Converter<String, EnumClass> enumClassConverter() {
        return new Converter<String, EnumClass>() {
            @Override
            public EnumClass convert(String source) {
                return EnumClass.fromValue(source);
            }
        };
    }
    @Bean(name = "org.openapitools.configuration.EnumConverterConfiguration.outerEnumConverter")
    Converter<String, OuterEnum> outerEnumConverter() {
        return new Converter<String, OuterEnum>() {
            @Override
            public OuterEnum convert(String source) {
                return OuterEnum.fromValue(source);
            }
        };
    }

}
