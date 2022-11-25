package org.openapitools.configuration;

import org.openapitools.model.RefOrValueEnum;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;

@Configuration
public class EnumConverterConfiguration {

    @Bean
    Converter<String, RefOrValueEnum> refOrValueEnumConverter() {
        return new Converter<String, RefOrValueEnum>() {
            @Override
            public RefOrValueEnum convert(String source) {
                return RefOrValueEnum.fromValue(source);
            }
        };
    }

}
