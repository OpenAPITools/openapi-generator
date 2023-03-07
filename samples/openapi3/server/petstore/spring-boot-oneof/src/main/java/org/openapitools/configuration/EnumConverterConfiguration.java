package org.openapitools.configuration;

import org.openapitools.model.FruitType;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;

@Configuration
public class EnumConverterConfiguration {

    @Bean(name = "org.openapitools.configuration.EnumConverterConfiguration.fruitTypeConverter")
    Converter<String, FruitType> fruitTypeConverter() {
        return new Converter<String, FruitType>() {
            @Override
            public FruitType convert(String source) {
                return FruitType.fromValue(source);
            }
        };
    }

}
