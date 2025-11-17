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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.servers.Server;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Properties;

@Configuration
public class OpenAPIDocumentationConfig {

    @Bean
    public OpenAPI customOpenAPI() {
        final Properties properties = new Properties();
        try (InputStream stream = this.getClass().getResourceAsStream("/version.properties")) {
            if (stream != null) {
                properties.load(stream);
            }
        } catch (IOException ex) {
            // ignore
        }

        String version = properties.getProperty("version", "unknown");

        return new OpenAPI()
                .info(new Info()
                        .title("OpenAPI Generator Online")
                        .description("This is an online OpenAPI generator server. You can generate client libraries, server stubs, and API documentation from OpenAPI specifications. Find out more at https://github.com/OpenAPITools/openapi-generator.")
                        .version(version)
                        .license(new License()
                                .name("Apache 2.0")
                                .url("https://www.apache.org/licenses/LICENSE-2.0.html"))
                        .contact(new Contact()
                                .name("OpenAPI Generator Team")
                                .url("https://github.com/OpenAPITools/openapi-generator")));
    }
}