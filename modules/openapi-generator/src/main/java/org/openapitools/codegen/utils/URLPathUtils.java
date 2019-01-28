/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.servers.ServerVariable;
import io.swagger.v3.oas.models.servers.ServerVariables;
import org.openapitools.codegen.CodegenConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class URLPathUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(URLPathUtils.class);
    public static final String LOCAL_HOST = "http://localhost";
    public static final Pattern VARIABLE_PATTERN = Pattern.compile("\\{([^\\}]+)\\}");

    public static URL getServerURL(OpenAPI openAPI) {
        final List<Server> servers = openAPI.getServers();
        if (servers == null || servers.isEmpty()) {
            LOGGER.warn("Server information seems not defined in the spec. Default to {}.", LOCAL_HOST);
            return getDefaultUrl();
        }
        // TODO need a way to obtain all server URLs
        return getServerURL(servers.get(0));
    }

    public static URL getServerURL(final Server server) {
        String url = server.getUrl();
        ServerVariables variables = server.getVariables();
        if(variables == null) {
            variables = new ServerVariables();
        }
        Set<String> replacedVariables = new HashSet<>();
        Matcher matcher = VARIABLE_PATTERN.matcher(url);
        while(matcher.find()) {
            if(!replacedVariables.contains(matcher.group())) {
                ServerVariable variable = variables.get(matcher.group(1));
                String replacement;
                if(variable != null) {
                    if(variable.getDefault() != null) {
                        replacement = variable.getDefault();
                    } else if(variable.getEnum() != null && !variable.getEnum().isEmpty()) {
                        replacement = variable.getEnum().get(0);
                    } else {
                        LOGGER.warn("No value found for variable '{}' in server definition '{}', default to empty string.", matcher.group(1), server.getUrl());
                        replacement = "";
                    }
                } else {
                    LOGGER.warn("No variable '{}' found in server definition '{}', default to empty string.", matcher.group(1), server.getUrl());
                    replacement = "";
                }
                url = url.replace(matcher.group(), replacement);
                replacedVariables.add(matcher.group());
                matcher = VARIABLE_PATTERN.matcher(url);
            }
        }
        url = sanitizeUrl(url);

        try {
            return new URL(url);
        } catch (MalformedURLException e) {
            LOGGER.warn("Not valid URL: {}. Default to {}.", server.getUrl(), LOCAL_HOST);
            return getDefaultUrl();
        }
    }

    public static String getScheme(OpenAPI openAPI, CodegenConfig config) {
        URL url = getServerURL(openAPI);
        return getScheme(url, config);
    }

    public static String getScheme(URL url, CodegenConfig config) {
        String scheme;
        if (url != null) {
            scheme = url.getProtocol();
        } else {
            scheme = "https";
        }
        if (config != null) {
            scheme = config.escapeText(scheme);
        }
        return scheme;
    }


    /**
     * Return the port, example value <code>8080</code>
     * @param url server url
     * @param defaultPort if the port is not set
     * @return port
     */
    public static String getPort(URL url, int defaultPort) {
        return getPort(url, String.valueOf(defaultPort));
    }

    /**
     * Return the port, example value <code>8080</code>
     * @param url server url
     * @param defaultPort if the port is not set
     * @return port
     */
    public static String getPort(URL url, String defaultPort) {
        if (url == null || url.getPort() == -1) {
            return defaultPort;
        } else {
            return String.valueOf(url.getPort());
        }
    }

    /**
     * Return the path, example value <code>/abcdef/xyz</code>
     * @param url server url
     * @param defaultPath if the path is not empty
     * @return path
     */
    public static String getPath(URL url, String defaultPath) {
        if (url == null || url.getPath() == null || url.getPath().isEmpty()) {
            return defaultPath;
        } else {
            return url.getPath();
        }
    }

    /**
     * Get the protocol and the host, example value <code>https://www.abcdef.xyz</code>
     * @param url server url
     * @return protocolAndHost
     */
    public static String getProtocolAndHost(URL url) {
        if (url == null) {
            return LOCAL_HOST;
        } else {
            String protocol = (url.getProtocol() == null) ? "http" : url.getProtocol();
            return protocol + "://"+  url.getHost();
        }
    }

    /**
     * Return the first complete URL from the OpenAPI specification
     * @param openAPI current OpenAPI specification
     * @return host
     */
    public static String getHost(OpenAPI openAPI) {
        if (openAPI.getServers() != null && openAPI.getServers().size() > 0) {
            return sanitizeUrl(getServerURL(openAPI.getServers().get(0)).toString());
        }
        return LOCAL_HOST;
    }

    private static String sanitizeUrl(String url) {
        if (url.startsWith("//")) {
            url = "http:" + url;
            LOGGER.warn("'scheme' not defined in the spec (2.0). Default to [http] for server URL [{}]", url);
        } else if (url.startsWith("/")) {
            url = LOCAL_HOST + url;
            LOGGER.warn("'host' (OAS 2.0) or 'servers' (OAS 3.0) not defined in the spec. Default to [{}] for server URL [{}]", LOCAL_HOST, url);
        } else if (!url.matches("[a-zA-Z][0-9a-zA-Z.+\\-]+://.+")) {
            // Add http scheme for urls without a scheme.
            // 2.0 spec is restricted to the following schemes: "http", "https", "ws", "wss"
            // 3.0 spec does not have an enumerated list of schemes
            // This regex attempts to capture all schemes in IANA example schemes which
            // can have alpha-numeric characters and [.+-]. Examples are here:
            // https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
            url = "http://" + url;
            LOGGER.warn("'scheme' not defined in the spec (2.0). Default to [http] for server URL [{}]", url);
        }

        return url;
    }

    private static URL getDefaultUrl() {
        try {
            return new URL(LOCAL_HOST);
        } catch (MalformedURLException e) {
            return null;
        }
    }
}
