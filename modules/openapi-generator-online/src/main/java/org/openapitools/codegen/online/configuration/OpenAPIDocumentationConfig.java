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
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.springdoc.core.SpringDocUtils;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springdoc.core.customizers.ServerBaseUrlCustomizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.TreeMap;


@Configuration
public class OpenAPIDocumentationConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(OpenAPIDocumentationConfig.class);

    static {
        // document the free-form "spec" field as an object, as before
        SpringDocUtils.getConfig().replaceWithClass(JsonNode.class, Object.class);
    }

    Info apiInfo() {
        final Properties properties = new Properties();
        try (InputStream stream = this.getClass().getResourceAsStream("/version.properties")) {
            if (stream != null) {
                properties.load(stream);
            }
        } catch (IOException ex) {
            // ignore
        }

        String version = properties.getProperty("version", "unknown");

        return new Info()
                .title("OpenAPI Generator Online")
                .description("This is an online openapi generator server.  You can find out more at https://github.com/OpenAPITools/openapi-generator.")
                .license(new License().name("Apache 2.0").url("https://www.apache.org/licenses/LICENSE-2.0.html"))
                .version(version)
                .contact(new Contact());
    }

    @Bean
    public OpenAPI customImplementation() {
        OpenAPI openAPI = new OpenAPI().info(apiInfo());

        String serverUrl = serverUrl(System.getenv("GENERATOR_HOST"));
        if (serverUrl != null) {
            // OpenAPI 3 has no host/schemes/basePath:
            // the configured public address becomes the server URL
            openAPI.addServersItem(new Server().url(serverUrl));
        }

        return openAPI;
    }

    /**
     * Lists the paths in alphabetical order, as the springfox document did,
     * so the landing page keeps its order.
     */
    @Bean
    public OpenApiCustomiser sortPaths() {
        return openApi -> {
            if (openApi.getPaths() != null) {
                Paths sorted = new Paths();
                new TreeMap<>(openApi.getPaths()).forEach(sorted::addPathItem);
                sorted.setExtensions(openApi.getPaths().getExtensions());
                openApi.setPaths(sorted);
            }
        };
    }

    /**
     * Leaves "http:" out of the server URL derived from the request ("//host:port"), as the
     * springfox document left out the scheme: behind a proxy that terminates TLS the service sees
     * http, and clients then use the scheme of the page instead of calling http from an https
     * page. https and a server URL from GENERATOR_HOST are kept.
     */
    @Bean
    public ServerBaseUrlCustomizer schemeRelativeServerUrl() {
        return serverBaseUrl -> serverBaseUrl.replaceFirst("^http:", "");
    }

    /**
     * The public address configured in GENERATOR_HOST without trailing slashes, or null if none is
     * configured or it is neither a URL with a host nor a path. Clients append operation paths,
     * which start with "/", and the service does not match "//".
     */
    static String serverUrl(String generatorHost) {
        String url = StringUtils.stripEnd(generatorHost, "/");
        if (StringUtils.isBlank(url)) {
            return null;
        }
        if (!isUrlWithHostOrPath(url)) {
            LOGGER.warn("Ignoring GENERATOR_HOST '{}' in the API description: "
                    + "it is neither a URL with a host nor a path", generatorHost);
            return null;
        }
        return url;
    }

    private static boolean isUrlWithHostOrPath(String url) {
        try {
            URI uri = new URI(url);
            // a scheme needs a host after it: "http:///x" and "example.com:8888" have none
            return uri.getScheme() == null || uri.getRawAuthority() != null;
        } catch (URISyntaxException e) {
            return false;
        }
    }

}
