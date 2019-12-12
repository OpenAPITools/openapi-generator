package org.openapitools.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.springframework.web.util.UriComponentsBuilder;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;

import java.util.Optional;
import javax.servlet.ServletContext;


@Configuration
public class OpenAPIDocumentationConfig {

@Bean
public OpenAPI customOpenAPI() {
        return new OpenAPI()
            .components(new Components())
            .info(new Info()
                .title("OpenAPI Petstore")
                .description("This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\")
                .license(new License()
                    .name("Apache-2.0")
                    .url("http://www.apache.org/licenses/LICENSE-2.0.html"))
                .termsOfService("")
                .version("1.0.0")
                .contact(new Contact()
                    .email("")));
    }

}
