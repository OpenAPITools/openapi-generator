package org.openapitools

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.info.Contact
import io.swagger.v3.oas.models.info.License
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.security.SecurityScheme

@jakarta.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"])
@Configuration
class SpringDocConfiguration {

    @Bean
    fun apiInfo(): OpenAPI {
        return OpenAPI()
            .info(
                Info()
                    .title("OpenAPI Petstore")
                    .description("This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.")
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
            )
    }
}
