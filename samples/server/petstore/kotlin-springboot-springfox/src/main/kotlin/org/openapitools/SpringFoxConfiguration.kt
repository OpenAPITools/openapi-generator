package org.openapitools

import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.web.util.UriComponentsBuilder
import springfox.documentation.builders.ApiInfoBuilder
import springfox.documentation.builders.RequestHandlerSelectors
import springfox.documentation.service.ApiInfo
import springfox.documentation.service.Contact
import springfox.documentation.spi.DocumentationType
import springfox.documentation.spring.web.paths.Paths
import springfox.documentation.spring.web.paths.RelativePathProvider
import springfox.documentation.spring.web.plugins.Docket
import springfox.documentation.swagger2.annotations.EnableSwagger2
import javax.servlet.ServletContext


@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"])
@Configuration
@EnableSwagger2
class SpringFoxConfiguration {

    fun apiInfo(): ApiInfo {
        return ApiInfoBuilder()
            .title("OpenAPI Petstore")
            .description("This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.")
            .license("Apache-2.0")
            .licenseUrl("https://www.apache.org/licenses/LICENSE-2.0.html")
            .termsOfServiceUrl("")
            .version("1.0.0")
            .contact(Contact("", "", ""))
            .build()
    }

    @Bean
    fun customImplementation(servletContext: ServletContext, @Value("\${openapi.openAPIPetstore.base-path:/v2}") basePath: String): Docket {
        return Docket(DocumentationType.SWAGGER_2)
                .select()
                    .apis(RequestHandlerSelectors.basePackage("org.openapitools.api"))
                    .build()
                .pathProvider(BasePathAwareRelativePathProvider(servletContext, basePath))
                .directModelSubstitute(java.time.LocalDate::class.java, java.sql.Date::class.java)
                .directModelSubstitute(java.time.OffsetDateTime::class.java, java.util.Date::class.java)
                .apiInfo(apiInfo())
    }

    class BasePathAwareRelativePathProvider(servletContext: ServletContext, private val basePath: String) :
        RelativePathProvider(servletContext) {

        override fun applicationPath(): String {
            return Paths.removeAdjacentForwardSlashes(
                UriComponentsBuilder.fromPath(super.applicationPath()).path(basePath).build().toString()
            )
        }

        override fun getOperationPath(operationPath: String): String {
            val uriComponentsBuilder = UriComponentsBuilder.fromPath("/")
            return Paths.removeAdjacentForwardSlashes(
                uriComponentsBuilder.path(operationPath.replaceFirst("^$basePath", "")).build().toString()
            )
        }
    }
}
