package org.openapitools.configuration;

import org.openapitools.virtualan.model.EnumClass;
import org.openapitools.virtualan.model.OuterEnum;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;

@Configuration
public class EnumConverters {

    @Bean
    Converter<String, EnumClass> EnumClassConverter() {
        return new Converter<String, EnumClass>() {
            @Override
            public EnumClass convert(String source) {
                return EnumClass.fromValue(source);
            }
        };
    }
    @Bean
    Converter<String, OuterEnum> OuterEnumConverter() {
        return new Converter<String, OuterEnum>() {
            @Override
            public OuterEnum convert(String source) {
                return OuterEnum.fromValue(source);
            }
        };
    }

}
