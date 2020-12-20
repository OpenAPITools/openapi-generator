package org.openapitools.configuration;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springdoc.core.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.servlet.ServletContext;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Configuration
public class OpenAPIDocumentationConfig {

    @Bean
    public OpenAPI apiInfo() {
        return new OpenAPI()
            .info(new Info().title("OpenAPI Petstore")
            .description("This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\")
            .version("1.0.0")
            .license(new License().name("Apache-2.0").url("https://www.apache.org/licenses/LICENSE-2.0.html")))
            .externalDocs(new ExternalDocumentation()
                .description("This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\")
                .url(""));
    }

    @Bean
    public GroupedOpenApi groupedOpenApi() {
        return GroupedOpenApi.builder()
            .setGroup("")
            .pathsToMatch("/**")
            .build();
    }

}
