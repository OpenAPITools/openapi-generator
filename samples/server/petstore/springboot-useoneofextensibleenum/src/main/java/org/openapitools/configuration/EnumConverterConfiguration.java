package org.openapitools.configuration;

import org.openapitools.model.OtherCountryEnum;
import org.openapitools.model.SubsetCountry;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;

@Configuration(value = "org.openapitools.configuration.enumConverterConfiguration")
public class EnumConverterConfiguration {

    @Bean(name = "org.openapitools.configuration.EnumConverterConfiguration.otherCountryEnumConverter")
    Converter<String, OtherCountryEnum> otherCountryEnumConverter() {
        return new Converter<String, OtherCountryEnum>() {
            @Override
            public OtherCountryEnum convert(String source) {
                return OtherCountryEnum.fromValue(source);
            }
        };
    }
    @Bean(name = "org.openapitools.configuration.EnumConverterConfiguration.subsetCountryConverter")
    Converter<String, SubsetCountry> subsetCountryConverter() {
        return new Converter<String, SubsetCountry>() {
            @Override
            public SubsetCountry convert(String source) {
                return SubsetCountry.fromValue(source);
            }
        };
    }

}
