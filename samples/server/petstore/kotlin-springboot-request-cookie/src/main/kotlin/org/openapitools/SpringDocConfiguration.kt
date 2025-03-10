package org.openapitools

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.info.Contact
import io.swagger.v3.oas.models.info.License
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.security.SecurityScheme

@Configuration
class SpringDocConfiguration {

    @Bean
    fun apiInfo(): OpenAPI {
        return OpenAPI()
            .info(
                Info()
                    .title("OpenAPI Petstore")
                    .description("This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\")
                    .license(
                        License()
                            .name("Apache-2.0")
                            .url("https://www.apache.org/licenses/LICENSE-2.0.html")
                    )
                    .version("1.0.0")
            )
            .components(
                Components()
                    .addSecuritySchemes("petstore_auth", SecurityScheme()
                        .type(SecurityScheme.Type.OAUTH2)
                    )
                    .addSecuritySchemes("api_key", SecurityScheme()
                        .type(SecurityScheme.Type.APIKEY)
                        .`in`(SecurityScheme.In.HEADER)
                        .name("api_key")
                    )
                    .addSecuritySchemes("api_key_query", SecurityScheme()
                        .type(SecurityScheme.Type.APIKEY)
                        .`in`(SecurityScheme.In.QUERY)
                        .name("api_key_query")
                    )
                    .addSecuritySchemes("http_basic_test", SecurityScheme()
                        .type(SecurityScheme.Type.HTTP)
                        .scheme("basic")
                    )
                    .addSecuritySchemes("bearer_test", SecurityScheme()
                        .type(SecurityScheme.Type.HTTP)
                        .scheme("bearer")
                        .bearerFormat("JWT")
                    )
                    .addSecuritySchemes("http_signature_test", SecurityScheme()
                        .type(SecurityScheme.Type.HTTP)
                        .scheme("signature")
                    )
            )
    }
}
