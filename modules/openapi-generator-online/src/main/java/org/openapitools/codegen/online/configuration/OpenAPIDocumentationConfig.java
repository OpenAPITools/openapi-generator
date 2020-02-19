/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.online.configuration;

import com.fasterxml.jackson.databind.JsonNode;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;


@Configuration
@EnableSwagger2
public class OpenAPIDocumentationConfig {

    ApiInfo apiInfo() {
        final Properties properties = new Properties();
        try (InputStream stream = this.getClass().getResourceAsStream("/version.properties")) {
            if (stream != null) {
                properties.load(stream);
            }
        } catch (IOException ex) {
            // ignore
        }

        String version = properties.getProperty("version", "unknown");

        return new ApiInfoBuilder()
            .title("OpenAPI Generator Online")
            .description("This is an online openapi generator server.  You can find out more at https://github.com/OpenAPITools/openapi-generator.")
            .license("Apache 2.0")
            .licenseUrl("https://www.apache.org/licenses/LICENSE-2.0.html")
            .termsOfServiceUrl("")
            .version(version)
            .contact(new Contact("","", ""))
            .build();
    }

    @Bean
    public Docket customImplementation(){
        return new Docket(DocumentationType.SWAGGER_2)
                .select()
                    .apis(RequestHandlerSelectors.basePackage("org.openapitools.codegen.online.api"))
                    .build()
                .forCodeGeneration(true)
                .directModelSubstitute(java.time.LocalDate.class, java.sql.Date.class)
                .directModelSubstitute(java.time.OffsetDateTime.class, java.util.Date.class)
                .directModelSubstitute(JsonNode.class, java.lang.Object.class)
                .ignoredParameterTypes(Resource.class)
                .ignoredParameterTypes(InputStream.class)
                .apiInfo(apiInfo());
    }

}
